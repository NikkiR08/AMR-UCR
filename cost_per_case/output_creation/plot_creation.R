#### temp plotting for presentation
library(data.table)
library(ggplot2)
library(tidyr)
library(gcookbook) ## for colour blind friendly palette
library(rworldmap)
library(dplyr)

##### abundance ######

## total across regions

load("cost_per_case/outputs/datall_forabundance.RData")

evidence_heatmap <- datall %>% 
  group_by(`World Health Organization Region`, Syndrome) %>% 
  summarise(Abundance = median(evidence))


mine.heatmap <- ggplot(data = evidence_heatmap, mapping = aes(x = Syndrome,
                                                              y = `World Health Organization Region`,
                                                              fill = Abundance))+
  geom_tile() + scale_fill_viridis_c(direction=-1) 

mine.heatmap

######## *****OLD***** ###########################
load("Data/costing_region_AMR.RData")
load("Data/AMR_Results_Table_AMR_nolabels.RData")

load("Data/costing_region_DRI.RData")
load("Data/AMR_Results_Table_DRI_nolabels.RData")

### drug syndrome all regions ####
dat <- costing.region.AMR

## limited syndromes
dat <- subset(dat,syndrome=="BSI"|
                syndrome=="UTI"|
                syndrome=="RTI"|
                syndrome=="SSI"|
                syndrom=="INF")

##removing XDR whilst using old data
dat <- subset(dat,dat$class!="xdr")
dat$Syndrome_Class <- paste(dat$syndrome,dat$gram.stain,dat$class)



all_mean <- ggplot() + geom_point(data = dat, aes(x = who.region, y = mean_costing, 
                                      color = Syndrome_Class)) + scale_fill_viridis_d()+
  xlab("Region (WHO)") +
  ylab("Healthcare System Cost (2019 USD)") 

all_mean

datD <- subset(costing.region.DRI,syndrome=="BSI"|
                syndrome=="UTI"|
                syndrome=="RTI"|
                syndrome=="SSI"|
                syndrome=="INF")

datD$Syndrome_Class <- paste(datD$syndrome,datD$gram.stain,datD$class)

##removing XDR whilst using old data
datD <- subset(datD,datD$class!="xdr")

all_meanD <- ggplot() + geom_point(data = datD, aes(x = who.region, y = mean_costing, 
                                                  color = Syndrome_Class)) + scale_fill_viridis_d()+
  xlab("Region (WHO)") +
  ylab("Healthcare System Cost (2019 USD)") 

all_meanD


### heatmap of info ####
combo <- rbind(results, results.long)
datall <- subset(combo, syndrome=="BSI"|
                   syndrome=="UTI"|
                   syndrome=="RTI"|
                   syndrome=="SSI"|
                   syndrome=="INF")

## create numerics for level of meta-analysis
datall <- as.data.table(datall)
datall$los.level.N <- 0
datall[los.level=="global",los.level.N := 1]
datall[los.level=="wbregion" ,los.level.N := 2]
datall[los.level=="income" ,los.level.N := 3]
datall[los.level=="whoc",los.level.N  := 4]

datall$cost.level.N <- 0
datall[extracted.cost.level=="global",cost.level.N := 1]
datall[extracted.cost.level=="wbregion" ,cost.level.N := 2]
datall[extracted.cost.level=="income" ,cost.level.N := 3]
datall[extracted.cost.level=="whoc",cost.level.N  := 4]

## replace NA number of studies so that summation doesn't produce NA later on
datall[is.na(los.no.studies), los.no.studies := 0]
datall[is.na(extracted.cost.no.studies), extracted.cost.no.studies := 0]

datall[ , evidence := (los.no.studies*los.level.N)+(extracted.cost.no.studies*
                                                      cost.level.N)]

## total across regions
evidence_heatmap <- datall %>% 
  group_by(`World Health Region`, syndrome) %>% 
  summarise(Abundance = mean(evidence))


mine.heatmap <- ggplot(data = evidence_heatmap, mapping = aes(x = syndrome,
                                                       y = who.region,
                                                       fill = Abundance))+
                      geom_tile() + scale_fill_viridis_c(direction=-1) 
 
mine.heatmap

######## INDIVIDUAL SYNDROME/CLASS PLOTS ######

temp <- costing.region.AMR
temp <- as.data.table(temp)
t <- dcast(temp, who.region + class +
             gram.stain ~ syndrome, value.var ="mean_costing")

low <- dcast(temp, who.region + class +
             gram.stain ~ syndrome, value.var ="low_costing")
high <- dcast(temp, who.region + class +
                gram.stain ~ syndrome, value.var ="high_costing")

all <- merge(low,high,by=c("who.region","class","gram.stain"))
all <- merge(all, t)

t.all <- all[complete.cases(all),]
# x = low, y = high

### 3GC BSI

graph1 <- temp[class=="3g cephalosporins" & 
                 (syndrome=="BSI"|syndrome=="UTI"|syndrome=="RTI")]


ggplot(graph1, aes(x=who.region, y=mean_costing, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_costing, ymax=high_costing),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Region (WHO)") +
  ylab("Healthcare System Cost (2019 USD)") +
  scale_fill_viridis_d()+
  ggtitle("The Effect of 3GC Resistance") +
  scale_y_continuous() +
  ylim(-1000,4000)

## penicillin resistance
graph2 <- temp[class=="penicillins" & 
                 (syndrome=="BSI"|syndrome=="UTI"|syndrome=="RTI"|syndrome=="SSI")]

ggplot(graph2, aes(x=who.region, y=mean_costing, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_costing, ymax=high_costing),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Region (WHO)") +
  ylab("Healthcare System Cost (2019 USD)") +
   scale_fill_viridis_d()+
  ggtitle("The Effect of Penicillin Resistance") +
  scale_y_continuous() +
  ylim(-10000,40000)

### PAHO variation

paho_bsi <- results[who.region=="PAHO" &
                      syndrome=="BSI"]


paho_bsi_g <- paho_bsi %>% 
  group_by(whoc.region, class) %>% 
  summarise(mean_cost = mean(mean.cost)) %>%
  as.data.table()

paho_bsi_g$mean_cost <- round(paho_bsi_g$mean_cost,0)
paho_bsi_g[class=="3g cephalosporins", class := "3GC"]

ggplot(paho_bsi_g, aes(x=class, y=mean_cost, fill=whoc.region)) + 
  geom_bar( position=position_dodge(),stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("Antibiotic Class") +
  ylab("Healthcare System Cost (2019 USD)") +  scale_fill_brewer(palette="Paired")+
  theme_minimal()+
  geom_text(aes(label=mean_cost), position=position_dodge(width=0.9), vjust=-0.25)

### useful info
mdr_tb <- results[syndrome=="RTI"& class=="mdr"]
length(which(mdr_tb$low.cost<0))
ssi <- results[syndrome=="SSI"]
length(which(ssi$low.cost<0))

##### input descriptive statistics #####
microbes <- table(lit_amr$microbe)
write.csv(microbes, file="Data/microbes_des_stat.csv")

microbes <- table(lit_dri$microbe)
write.csv(microbes, file="Data/microbes_des_stat_Dri.csv")


##### plots
library(xlsx)

# temp_slides_global <- read.xlsx("Data/global_matched_temp.xlsx",1)

# temp_slides_global[ , Exposure := paste(class, bacteria)]
# 
# ggplot(temp_slides_global, aes(x=Exposure, y=mean_cost, fill=syndrome)) + 
#   geom_bar(position=position_dodge(), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position=position_dodge(.9)) +
#   xlab("Resistance & Bacterial Exposure") +
#   ylab("Healthcare System Cost (2019 USD)") +
#   scale_fill_viridis_d()+
#   ggtitle("Global Averages") +
#   scale_y_continuous() +
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### reordering by 

# temp_slides_global$Exposure_Group <- factor(temp_slides_global$Exposure,
#                                             levels =c( "3GCs E. coli",
#                                             "carbapenems E. coli",
#                                             "3GCs K. pnuemoniae",
#                                             "carbapenems K. pnuemoniae",
#                                             "3GCs P. aeruginosa" ,
# "carbapenems P. aeruginosa"  , "3GCs A. baumannii" ,
# "carbapenems A. baumannii"  , "penicillins S. aureus"  ,
# "glycopeptides S. aureus"  ,    "penicillins S. pneumoniae" ,
# "glycopeptides S. pneumoniae", "penicillins E. faecium"    ,
# "glycopeptides E. faecium"   ))


ggplot(df1, aes(x=Exposure_Group, y=mean_cost, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Healthcare System Cost (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## remove nonsig
temp_slides_global_new <- temp_slides_global[Significance=="Significant"]


ggplot(temp_slides_global_new, aes(x=Exposure_Group, y=mean_cost, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Healthcare System Cost (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_log10()

## remove SSI top scale

ggplot(temp_slides_global, aes(x=interaction(Exposure_Group), y=mean_cost, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Healthcare System Cost (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim=c(NA, 21000), expand = FALSE)

## testing

temp_slides_global2 <- subset(temp_slides_global,Significance=="Significant")
temp_slides_global2 <- temp_slides_global2 %>% complete(Exposure_Group, nesting(syndrome))
ggplot(temp_slides_global2, aes(x=interaction(Exposure_Group), y=mean_cost, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Healthcare System Cost (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim=c(NA, 21000), expand = FALSE)
