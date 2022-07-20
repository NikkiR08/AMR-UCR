######### ANTIBIOTICS COSTING CALCULATIONS ################
library(tidyverse) 
library(data.table)
library(ggplot2)

### reading in data
load("Data/antibiotics/abx_combo_inputs.RData")

############ SCENARIO 1: GENERIC PRICE ADJUSTMENT FACTORS ########################

### oral adjustment factors
## create adjustment factor:
oral_generic_av <- abx_combo_inputs %>%
  filter(Route.of.Admin.x=="po") %>%
  group_by(iso3c) %>% 
  summarise(generic_adj_oral = median(generic.adj.x/abx.adj,na.rm=TRUE)) %>%
  as.data.table()

## group by antibiotic, form and country
inj_generic_av <- abx_combo_inputs %>%
  filter(Route.of.Admin.x=="inj") %>%
  group_by(iso3c) %>%
  summarise(generic_adj_inj = median(generic.adj.y/abx.adj, na.rm=TRUE)) %>%
  as.data.table()

## average adjustment factors across the countries:
median(oral_generic_av$generic_adj_oral)
median(inj_generic_av$generic_adj_inj)

## merge back in 
abx_sc1 <- merge(abx_combo_inputs, oral_generic_av, by="iso3c")
abx_sc1 <- merge(abx_sc1, inj_generic_av, by="iso3c")

# if generic missing adjust accordingly:
abx_sc1[is.na(generic.adj.x) & Route.of.Admin.x=="po", generic.adj.x := abx.adj*generic_adj_oral]
abx_sc1[is.na(generic.adj.y) & Route.of.Admin.x=="inj", generic.adj.y := abx.adj*generic_adj_inj]
abx_sc1[ , generic.cost.sc1 := generic.adj.x]
abx_sc1[is.na(generic.cost.sc1), generic.cost.sc1 := generic.adj.y]

## DESCRIPTIVE STATS
abx_sc1_des <- abx_sc1[iso3c=="GBR"] ## chose 1 country
length(which(abx_sc1_des$Route.of.Admin.x=="po"))
length(which(abx_sc1_des$Route.of.Admin.x=="inj"))
length(unique(abx_sc1$iso3c))
rm(abx_sc1_des)

## highlighting median/distributions of adjustments (for USA example)
test <- abx_combo_inputs[Route.of.Admin.x=="inj"&iso3c=="USA"]
test[ , diff := generic.adj.y/abx.adj]
plot(test$diff)
summary(test$diff, na.rm=TRUE)

## number of matched vs estimated through adjustment factor
length(which(is.na(abx_combo_inputs$generic.adj.y& abx_combo_inputs$Route.of.Admin.x=="inj")))/length(unique(abx_sc1$iso3c))
length(which(is.na(test$generic.adj.y)))
## they match (both ways of calculating the same thing)

test2 <- abx_combo_inputs[Route.of.Admin.x=="po"&iso3c=="USA"]
test2[ , diff := generic.adj.x/abx.adj]
plot(test2$diff)
summary(test2$diff, na.rm=TRUE)
length(which(is.na(abx_combo_inputs$generic.adj.x& abx_combo_inputs$Route.of.Admin.x=="po")))/length(unique(abx_sc1$iso3c))
length(which(is.na(test2$generic.adj.x)))

##### SCENARIO 2: INCOME GROUP PROXY ADJUSTMENTS #####################
abx_sc1_sc2 <- abx_sc1

## creating a cost variable across both administration types across income groups
abx_sc1_sc2[Income.group=="High income" , cost.sc2 := UK.adj.x]
abx_sc1_sc2[Income.group=="Upper middle income" , cost.sc2 := SA.adj.x]
abx_sc1_sc2[Income.group=="Lower middle income"|
                   Income.group=="Low income", cost.sc2 := India.adj.x]
abx_sc1_sc2[Income.group=="High income" & Route.of.Admin.x=="inj", cost.sc2 := UK.adj.y] 
abx_sc1_sc2[Income.group=="Upper middle income" & Route.of.Admin.x=="inj", cost.sc2 := SA.adj.y]
abx_sc1_sc2[(Income.group=="Lower middle income"|
              Income.group=="Low income") & Route.of.Admin.x=="inj", cost.sc2 := India.adj.y]

# ## number of combinations with no sc2 cost
# abx_sc1_sc2$combo <- paste(abx_sc1_sc2$MSH_antibiotic, abx_sc1_sc2$Route.of.Admin.x,
#                            abx_sc1_sc2$Dose.x)
# length(unique(abx_sc1_sc2$combo))
# temp.sc2 <- abx_sc1_sc2[is.na(cost.sc2)]
# length(unique(temp.sc2$combo))

## trying with data table (scenario 1 was done with piping)
df_factor_Y <- abx_sc1_sc2[ , .(diff.fctr=median(cost.sc2/abx.adj, na.rm=TRUE)), by=.(Income.group,
                                                                       Route.of.Admin.x)]

# ## just checking by hand to test same values given in scenario 1 if used this method
# abx_combo_inputs[ , .(generic_adj_oral=median(generic.adj.x/abx.adj,na.rm=TRUE)),
#                   by=.(iso3c, Route.of.Admin.x)] ## yes same values for first po groups

abx_sc1_sc2 <- merge(abx_sc1_sc2, df_factor_Y, by=c("Income.group", "Route.of.Admin.x"))

# if sc2 values currently missing adjust accordingly:
abx_sc1_sc2[is.na(cost.sc2) & Route.of.Admin.x=="po", cost.sc2 := abx.adj*diff.fctr]
abx_sc1_sc2[is.na(cost.sc2) & Route.of.Admin.x=="inj", cost.sc2:= abx.adj*diff.fctr]

## ## save without labels
## save(abx_sc1_sc2, file="Data/antibiotics/sc1_sc2_nolabels.RData")

## relabel for outputs
abx_sc1_sc2_output <- abx_sc1_sc2 %>% rename("Country (ISO3 Code)"="iso3c",
                                      "WHO-CHOICE Region"= "whoc.region",
                                      "Income Group" = "Income.group" ,
                                      "Antibiotic" = "MSH_antibiotic",
                                      "MSH Form" = "Form.x",
                                      "MSH Dose" = "Dose.x",
                                      "MSH Cost" = "abx.adj",
                                      "Scenario 1 Generic Cost Estimate" = "generic.cost.sc1",
                                      "Scenario 2 Cost Estimate" = "cost.sc2")  %>%
                          select("Country (ISO3 Code)","WHO-CHOICE Region",
                                 "Income Group" ,  "Antibiotic",
                                 "MSH Form","MSH Dose","MSH Cost",
                                 "Scenario 1 Generic Cost Estimate","Scenario 2 Cost Estimate" ) %>%
                          as.data.table()

## write.csv(abx_sc1_sc2_output, "Data/antibiotics/country_abx_cost.csv")
## save(abx_sc1_sc2_output, file="Data/antibiotics/country_abx_cost.RData")


#### read in from regional calculator for descriptive stats ######
load("Data/antibiotics/regional_abx.RData")

## scenario 1 vs scenario 2
names(regional.abx)[names(regional.abx) == 'weighted_cost_Scenario1'] <- 'Scenario 1'
names(regional.abx)[names(regional.abx) == 'weighted_cost_Scenario2'] <- 'Scenario 2'

mdata <- melt(regional.abx, id=c("who.region","Antibiotic", "MSH Form", "MSH Dose"))
ggplot(mdata, aes(x=value, colour=variable)) + geom_density() 


## regional averages 
mdata_region <- regional.abx[ , .(`Scenario 1`=median(`Scenario 1`, na.rm=TRUE),
                                  `Scenario 2`=median(`Scenario 2`, na.rm=TRUE)),
                              by=.(who.region,`MSH Form`)]

mdata_region <- melt(mdata_region, id=c("who.region","MSH Form"))
mdata_tabcap <- subset(mdata_region, `MSH Form`=="tab-cap")
mdata_inj <- subset(mdata_region, `MSH Form`=="vial") ## all vial in inj currently due to definitions and mapping
## might need to adapt the above if in future iterations expand definitions/change mapping assumptions

# regional bar plots:
# Change the colors manually
ggplot(data=mdata_tabcap, aes(x=who.region, y=value, fill=`variable`)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + ylab("Cost per unit ($2019)") +
  ggtitle("Tab-Cap")

ggplot(data=mdata_inj, aes(x=who.region, y=value, fill=`variable`)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + ylab("Cost per unit ($2019)") +
  ggtitle("Injectable")

### just to show one region - AFRO
afro <- regional.abx[who.region=="AFRO"]

afrodose <- afro %>%
  group_by(Antibiotic, `MSH Form`) %>%
  arrange(`MSH Dose`) %>%
  slice(n()) %>%
  ungroup

ggplot(data=afrodose, aes(x=Antibiotic, y=`Scenario 1`, color=`MSH Dose`)) +
  geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("Cost per unit (2019 USD)")


## South Africa
zaf <- abx_sc1_sc2_output[`Country (ISO3 Code)`=="ZAF" & `MSH Form`=="tab-cap"]
zaf <- zaf %>%
  group_by(Antibiotic, `MSH Form`) %>%
  arrange(`MSH Dose`) %>%
  slice(n()) %>%
  ungroup %>%
  arrange(`Scenario 2 Cost Estimate`) %>%
  select(Antibiotic,"MSH Cost",`Scenario 1 Generic Cost Estimate`,
         `Scenario 2 Cost Estimate`) %>%
  as.data.table()

zafG <- melt(zaf, id="Antibiotic")

orderzafG <- zafG[variable=="Scenario 2 Cost Estimate"]
orderzafG <- orderzafG$Antibiotic[order(orderzafG$value, decreasing=TRUE)]

zafG <- zafG %>%
    mutate(Antibiotic=factor(Antibiotic, levels=orderzafG))

ggplot(data=zafG, aes(x=Antibiotic, y=value, fill=`variable`)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  coord_flip() + xlab("Cost per Unit (2019 USD)")