#### PLOTS FOR PAPER #######

library(data.table)
library(ggplot2)
library(tidyr)
library(gcookbook) ## for colour blind friendly palette
library(maptools)
library(rworldmap)
library(dplyr)

##### level of evidence - FIGURE 2 ######

## total across regions

load("cost_per_case/outputs/datall_forlevel.RData")

evidence_heatmap <- datall %>% 
  group_by(`World Health Organization Region`, Syndrome) %>% 
  summarise(level_evidence = median(evidence))


mine.heatmap <- ggplot(data = evidence_heatmap, mapping = aes(x = Syndrome,
                                                              y = `World Health Organization Region`,
                                                              fill = level_evidence))+
  geom_tile() + scale_fill_viridis_c(direction=-1) 

mine.heatmap




##### PLOTS OF DIFFERENT TYPES OF COST - supplementary material ######

load("cost_per_case/outputs/costing.table.region.G.RData")

outputs <- as.data.table(costing.table.region.G)
outputs[LOW_weighted_costing.both<=0 &
          HIGH_weighted_costing.both<=0 , cost.sig.flag:="SIG"]
outputs[LOW_weighted_costing.both>=0 &
          HIGH_weighted_costing.both>=0 , cost.sig.flag:="SIG"]
outputs[LOW_weighted_costing.both<0 &
          HIGH_weighted_costing.both>0 , cost.sig.flag := "NONSIG" ]

outputs.cost.sig <- outputs[cost.sig.flag=="SIG"]

AMR <- outputs.cost.sig[AMR_or_DRI=="AMR"]

colnames(AMR)[colnames(AMR) == 'AV_weighted_costing.both'] <- 'Scenario 1. Combined Sample'
colnames(AMR)[colnames(AMR) == 'AV_weighted_costing.sc2'] <- 'Scenario 2. Applied Adjustment Factor'
colnames(AMR)[colnames(AMR) == 'AV_weighted_costing.los'] <- 'LOS Cost Only'


AMR.melt <- melt(AMR, id=c("who.region", "syndrome", "class", "AMR_or_DRI", "gram.stain"),
                 measure=c("Scenario 1. Combined Sample","Scenario 2. Applied Adjustment Factor","LOS Cost Only"))

AMR.melt <- AMR.melt[class!="mdr"] ## remove TB to discuss separately

ggplot(data = AMR.melt, aes(class, value, color=variable, shape=variable)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha = 0.5, size = 2.5) +
  facet_grid(who.region ~ syndrome)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Resistance Exposure") +
  ylab("Excess Hospital Cost per Case (2019 USD)") +
  scale_color_brewer(palette="Dark2")

DRI <- outputs.cost.sig[AMR_or_DRI=="DRI"]

colnames(DRI)[colnames(DRI) == 'AV_weighted_costing.both'] <- 'Scenario 1. Combined Sample'
colnames(DRI)[colnames(DRI) == 'AV_weighted_costing.sc2'] <- 'Scenario 2. Applied Adjustment Factor'
colnames(DRI)[colnames(DRI) == 'AV_weighted_costing.los'] <- 'LOS Cost Only'


DRI.melt <- melt(DRI, id=c("who.region", "syndrome", "class", "AMR_or_DRI", "gram.stain"),
                 measure=c("Scenario 1. Combined Sample","Scenario 2. Applied Adjustment Factor","LOS Cost Only"))

DRI.melt <- DRI.melt[class!="mdr"] ## remove TB to discuss separately

ggplot(data = DRI.melt, aes(class, value, color=variable, shape=variable)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha = 0.5, size = 2.5) +
  facet_grid(who.region ~ syndrome)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Resistance Exposure") +
  ylab("Excess Hospital Cost per Case (2019 USD)") +
  scale_color_brewer(palette="Dark2")+
  ggtitle("DRI")

#### MAP PLOTS #######
###### PLOTS ################

#### add code to log numerical columns for plots
load("cost_per_case/outputs/scenario2_results_4plot.RData")

sc2.results.sum <-  sc2.results.long.keep[, lapply(.SD, mean, na.rm=TRUE),
            by = c("Country (ISO3 Code)",
                   "AMR_or_DRI"),
            .SDcols=c("Mean Cost - Across Both")]

### just doing AMR not DRI
sc2.results.sum <- sc2.results.sum[AMR_or_DRI=="AMR"]

sc2.results.sum.los <- sc2.results.long.keep[, lapply(.SD, mean, na.rm=TRUE),
                                             by = c("Country (ISO3 Code)",
                                                    "AMR_or_DRI"),
                                             .SDcols=c("Mean Cost - from Excess LOS")]
sc2.results.sum.sc2 <- sc2.results.long.keep[, lapply(.SD, mean, na.rm=TRUE),
                                             by = c("Country (ISO3 Code)",
                                                    "AMR_or_DRI"),
                                             .SDcols=c("Scenario 2 Mean Cost")]

joinData1 <- joinCountryData2Map( sc2.results.sum ,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "Country (ISO3 Code)")

joinDatalos <- joinCountryData2Map( sc2.results.sum.los ,
                                  joinCode = "ISO3",
                                  nameJoinColumn = "Country (ISO3 Code)")

joinData2 <- joinCountryData2Map( sc2.results.sum.sc2 ,
                                  joinCode = "ISO3",
                                  nameJoinColumn = "Country (ISO3 Code)")

mapping_function <- function(x, y){
  ##!!!
  ## doing alot by hand post production for now but next time try to integrate labels
  ## e.g. adding back transformed scale numbers and titles
  theMap <- mapCountryData( x, nameColumnToPlot=y, addLegend=FALSE ,
                            colourPalette = "terrain", mapTitle = y)
  
  labs <- theMap$cutVector %>% 
    exp() %>%
    round(-2)
  
  do.call( addMapLegend, c(theMap,
                           legendWidth=1, legendMar = 2,
                           legendIntervals='data',
                           legendLabels='all'))
  
  print(labs)
}

### need to separate out across AMR and DRI 
mapping_function(joinData1, "Mean Cost - Across Both")
mapping_function(joinDatalos, "Mean Cost - from Excess LOS")
mapping_function(joinData2, "Scenario 2 Mean Cost")



####******************* PLots not in use currently******************** ####
# ######## REGIONAL PLOTS for Hospital Costs######
# load("cost_per_case/outputs/costing.table.region.G.RData")
# 
# outputs <- as.data.table(costing.table.region.G)
# 
# ### there will be a more efficient way to do this 
# ### in the interest of time for producing paper doing a lot by hand
# AMR <- outputs[AMR_or_DRI=="AMR"]
# AMR_GNGP <- AMR[class!="mdr"] ## put TB on separate plots
# 
# LOS.plots <- function(EURO){
#   name.reg <- EURO$who.region[1]
#   x <- ggplot(EURO, aes(x=interaction(class), y=AV_weighted_costing.los, fill=syndrome)) + 
#     geom_bar(position=position_dodge(preserve="single"), stat="identity",
#              colour="black", # Use black outlines,
#              size=.3) +      # Thinner lines
#     geom_errorbar(aes(ymin=LOW_weighted_costing.los, ymax=HIGH_weighted_costing.los),
#                   size=.3,    # Thinner lines
#                   width=.2,
#                   position = position_dodge(width = 0.9, preserve = "single")) +
#     xlab("Resistance Exposure") +
#     ylab("LOS-associated Cost (2019 USD)") +
#     scale_fill_viridis_d()+
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#     coord_cartesian(ylim=c(-5000, 35000), expand = FALSE)+
#     ggtitle(name.reg)
#   return(x)
# }
# 
# AFRO <- AMR_GNGP[who.region=="AFRO"]
# EMRO <- AMR_GNGP[who.region=="EMRO"]
# EURO <- AMR_GNGP[who.region=="EURO"]
# PAHO <- AMR_GNGP[who.region=="PAHO"]
# SEARO <- AMR_GNGP[who.region=="SEARO"]
# WPRO <- AMR_GNGP[who.region=="WPRO"]
# 
# 
# LOS.plots(AFRO)
# LOS.plots(EMRO)
# LOS.plots(EURO)
# LOS.plots(PAHO)
# LOS.plots(SEARO)
# LOS.plots(WPRO)
# 
# 
# DRI <- outputs[AMR_or_DRI=="DRI"]
# DRI_GNGP <- DRI[class!="mdr"] ## put TB on separate plots
# 
# AFRO <- DRI_GNGP[who.region=="AFRO"]
# EMRO <- DRI_GNGP[who.region=="EMRO"]
# EURO <- DRI_GNGP[who.region=="EURO"]
# PAHO <- DRI_GNGP[who.region=="PAHO"]
# SEARO <- DRI_GNGP[who.region=="SEARO"]
# WPRO <- DRI_GNGP[who.region=="WPRO"]
# 
# LOS.plots(AFRO)
# LOS.plots(EMRO)
# LOS.plots(EURO)
# LOS.plots(PAHO)
# LOS.plots(SEARO)
# LOS.plots(WPRO)
# 
# 
# ##### TB cost per case ##########
# load("cost_per_case/outputs/costing.table.region.G.RData")
# 
# outputs <- as.data.table(costing.table.region.G)
# tb <- outputs[gram.stain=="tb"]
# tb <- tb[ ,-c("syndrome", "gram.stain","class")]
# 
# ggplot(tb, aes(x=who.region, y=AV_weighted_costing.los)) + 
#   geom_bar(position=position_dodge(preserve="single"), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=LOW_weighted_costing.los, ymax=HIGH_weighted_costing.los),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position = position_dodge(width = 0.9, preserve = "single")) 
# 
# ggplot(tb, aes(x=who.region, y=AV_weighted_costing.both)) + 
#   geom_bar(position=position_dodge(preserve="single"), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=LOW_weighted_costing.both, ymax=HIGH_weighted_costing.both),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position = position_dodge(width = 0.9, preserve = "single")) +
#   xlab("WHO Region")+
#   ylab("Excess Hospital Cost per Case (Scenario 1)")