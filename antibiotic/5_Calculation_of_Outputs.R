######### ANTIBIOTICS COSTING CALCULATIONS ################
library(tidyverse)
library(data.table)
library(ggplot2)

### reading in data
load("antibiotic/outputs/abx_combo_inputs.RData")

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
### !!! note removes some countries where there are no values
median(oral_generic_av$generic_adj_oral,na.rm=TRUE)
median(inj_generic_av$generic_adj_inj,na.rm=TRUE)

## merge back in
abx_sc1 <- merge(abx_combo_inputs, oral_generic_av, by="iso3c")
abx_sc1 <- merge(abx_sc1, inj_generic_av, by="iso3c")

# if generic missing adjust accordingly:
abx_sc1[is.na(generic.adj.x) & Route.of.Admin.x=="po", generic.adj.x := abx.adj*generic_adj_oral]
abx_sc1[is.na(generic.adj.y) & Route.of.Admin.x=="inj", generic.adj.y := abx.adj*generic_adj_inj]
abx_sc1[ , generic.cost.sc1 := generic.adj.x]
abx_sc1[is.na(generic.cost.sc1), generic.cost.sc1 := generic.adj.y]

## DESCRIPTIVE STATS
### !!! note this includes combinations that don't necessarily have a sc1 or2 cost, just an MSH cost
abx_sc1_des <- abx_sc1[iso3c=="CUB"] ## chose 1 country
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
abx_sc1_sc2$combo <- paste(abx_sc1_sc2$MSH_antibiotic, abx_sc1_sc2$Route.of.Admin.x,
                           abx_sc1_sc2$Dose.x)
length(unique(abx_sc1_sc2$combo))
temp.sc2 <- abx_sc1_sc2[is.na(cost.sc2)]
length(unique(temp.sc2$combo))

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
save(abx_sc1_sc2, file="antibiotic/outputs/sc1_sc2_nolabels.RData")

## relabel for outputs
abx_sc1_sc2_output <- abx_sc1_sc2 %>% rename("Country (ISO3 Code)"="iso3c",
                                      "WHO-CHOICE Region"= "whoc.region",
                                      "Income Group" = "Income.group" ,
                                      "Antibiotic" = "MSH_antibiotic",
                                      "MSH Form" = "Form.x",
                                      "MSH Dose" = "Dose.x",
                                      "MSH Cost" = "abx.adj",
                                      "Scenario 1 Generic Cost Estimate" = "generic.cost.sc1",
                                      "Scenario 2 Income Cost Estimate" = "cost.sc2")  %>%
                          select("Country (ISO3 Code)","WHO-CHOICE Region",
                                 "Income Group" ,  "Antibiotic",
                                 "MSH Form","MSH Dose","MSH Cost",
                                 "Scenario 1 Generic Cost Estimate","Scenario 2 Income Cost Estimate","Class" ,
                                 "Category") %>%
                          as.data.table()
### !!! idelly need to add rounding to 2dp

#### descriptive stats ####
### remove those with no scenario 1 or 2 cost
abx_sc1_sc2_complete <- abx_sc1_sc2_output[!is.na(`Scenario 1 Generic Cost Estimate`)|
                                             !is.na(`Scenario 2 Income Cost Estimate`)]
unique(abx_sc1_sc2_complete$Class)
nrow(abx_sc1_sc2_complete)/nrow(abx_sc1_sc2_output)

write.csv(abx_sc1_sc2_output, "antibiotic/outputs/country_abx_cost.csv")
save(abx_sc1_sc2_output, file="antibiotic/outputs/country_abx_cost.RData")

##### REGIONAL AVERAGES ##############
load("antibiotic/outputs/country_abx_cost.RData")
load("data_all/who_whoc_wb.RData")
who <- who_whoc_wb[ , c("iso3c","who.region")]

N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))
N <- N[Indicator.Code=="SP.POP.TOTL"]

### keep 2019 values
N <- N[ , c("Country.Code","X2019")]
setnames(N, "X2019", "npop")

combo <- merge(abx_sc1_sc2_output, N, by.x="Country (ISO3 Code)", by.y="Country.Code")
combo <- merge(combo, who, by.x="Country (ISO3 Code)", by.y="iso3c" )


regional.abx <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(who.region, Antibiotic,`MSH Form`, `MSH Dose`) %>% 
  summarise(weighted_cost_Scenario1 = weighted.mean(`Scenario 1 Generic Cost Estimate`, npop, na.rm=TRUE),
            weighted_cost_Scenario2 = weighted.mean(`Scenario 2 Income Cost Estimate`, npop, na.rm=TRUE)) %>%
  as.data.table()

write.csv(regional.abx, file="antibiotic/outputs/regional_abx_cost.csv")

#### BY CLASS AND REGION ######

regional.abx.class <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(who.region, Class) %>% 
  summarise(`Scenario 1 - Generic Proxy` = weighted.mean(`Scenario 1 Generic Cost Estimate`, npop, na.rm=TRUE),
            `Scenario2 - Income Group Proxy` = weighted.mean(`Scenario 2 Income Cost Estimate`, npop, na.rm=TRUE)) %>%
  as.data.table()

rg.abx.cls.melt <- melt(regional.abx.class, id=c("who.region","Class"),
                        measure=c("Scenario 1 - Generic Proxy",
                                  "Scenario2 - Income Group Proxy"))

ggplot(data = rg.abx.cls.melt, aes(Class, value, color=variable, shape=variable)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha = 0.5, size = 2.5) +
  facet_wrap(~who.region)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Antibiotic Class") +
  ylab("Excess Cost per Unit") +
  scale_color_brewer(palette="Dark2")
 
#### BY AWARE CATEGORY ######

aware.plot.df <- melt(combo, id=c("Country (ISO3 Code)","Antibiotic","MSH Dose",
                                  "MSH Form","Class","Category"),
                           measure=c("Scenario 1 Generic Cost Estimate",
                                     "Scenario 2 Income Cost Estimate"))

reserve <- combo[Category=="Reserve"]
unique(reserve$Antibiotic)
## "linezolid"
summary(reserve$`Scenario 1 Generic Cost Estimate`)
summary(reserve$`Scenario 2 Income Cost Estimate`, na.rm=TRUE)

ftable(aware.plot.df$`MSH Dose`)
## most common are 1000mg, 150mg, 250mg and 500mg

# ### 500 mg oral
# oral.500 <-aware.plot.df[`MSH Form`=="tab-cap"& 
#                                          `MSH Dose`=="500mg"]
# 
# ggplot(oral.500, aes(x=Category, y=value, fill=variable)) +
#   geom_boxplot()
# ### zoom without dropping data
# ggplot(oral.500, aes(x=Category, y=value, fill=variable)) +
#   geom_boxplot()+
#   coord_cartesian(ylim = c(0, 1))
# 
# ### 500 mg vial
# vial.500 <-aware.plot.df[`MSH Form`=="vial"& 
#                            `MSH Dose`=="500mg"]
# 
# ggplot(vial.500, aes(x=Category, y=value, fill=variable)) +
#   geom_boxplot()
# ### zoom without dropping data
# ggplot(vial.5000, aes(x=Category, y=value, fill=variable)) +
#   geom_boxplot()+
#   coord_cartesian(ylim = c(0, 3))
# #### !!! only 2 antibiotic (1 for each group in this category)
#### for vial so moved to previous large groups

### 250 mg oral
oral.250 <-aware.plot.df[`MSH Form`=="tab-cap"&
                           `MSH Dose`=="250mg"]
unique(oral.250$Antibiotic)
ggplot(oral.250, aes(x=Category, y=value, fill=variable)) +
  geom_boxplot()+
  xlab("AWARE Group")+ylab("Cost per Unit (2019 USD)")+
  theme(text = element_text(size = 20))
### zoom without dropping data
ggplot(oral.250, aes(x=Category, y=value, fill=variable)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 0.5))+
  xlab("AWARE Group")+ylab("Cost per Unit (2019 USD)")+
  theme(text = element_text(size = 20))

o250 <- oral.250[variable=="Scenario 1 Generic Cost Estimate"]
o250 %>% group_by(Category)%>%
summarise(med = median(value))

### 1000 mg vial
vial.1000 <-aware.plot.df[`MSH Form`=="vial"& 
                           `MSH Dose`=="1g"]
unique(vial.1000$Antibiotic)

ggplot(vial.1000, aes(x=Category, y=value, fill=variable)) +
  geom_boxplot()+
  xlab("AWARE Group")+ylab("Cost per Unit (2019 USD)")+
  theme(text = element_text(size = 20))
### zoom without dropping data
ggplot(vial.1000, aes(x=Category, y=value, fill=variable)) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 5))+
  xlab("AWARE Group")+ylab("Cost per Unit (2019 USD)")+
  theme(text = element_text(size = 20))

v1 <- vial.1000[variable=="Scenario 1 Generic Cost Estimate"]
v1 %>% group_by(Category)%>%
  summarise(med = median(value,na.rm=TRUE))

oral.250 %>% group_by(Category,variable)%>%
  summarise(med = median(value,na.rm=TRUE))

vial.1000 %>% group_by(Category,variable)%>%
  summarise(med = median(value,na.rm=TRUE))