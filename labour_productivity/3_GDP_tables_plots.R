##### CREATING THE GDP OUTPUTS FROM THE MODEL RESULTS

###loading libraries and data
library(tidyverse)
library(data.table)

load("labour_productivity/outputs/gdp_results_all.RData")

### tables #####

### !!! next iteration make a function that does for each 
## scenario so not copying and pasting the col names of interest

results_GDP_difference <- results %>%
  mutate(sc1_ecoli_loss = (basecase_Y.2019usd-sc1_ecoli_Y.2019usd), 
         sc2_ecoli_loss = (basecase_Y.2019usd-sc2_ecoli_Y.2019usd),
         sc1_saureus_loss = (basecase_Y.2019usd-sc1_saureus_Y.2019usd),
         sc2_saureus_loss = (basecase_Y.2019usd-sc2_saureus_Y.2019usd)) %>%
  group_by(iso3c, year) %>%
    mutate(sc1_ecoli_MAD = median(sc1_ecoli_loss, na.rm=TRUE), ## MAD = median absolute difference (Annual)
           sc1_ecoli_MPD = median((sc1_ecoli_loss/basecase_Y.2019usd),na.rm=TRUE),## MPD = median percentage difference (Annual)
           sc2_ecoli_MAD = median(sc2_ecoli_loss, na.rm=TRUE),
           sc2_ecoli_MPD = median((sc2_ecoli_loss/basecase_Y.2019usd),na.rm=TRUE),
           sc1_saureus_MAD = median(sc1_saureus_loss, na.rm=TRUE),
           sc1_saureus_MPD = median((sc1_saureus_loss/basecase_Y.2019usd),na.rm=TRUE),
           sc2_saureus_MAD = median(sc2_saureus_loss, na.rm=TRUE),
           sc2_saureus_MPD = median((sc2_saureus_loss/basecase_Y.2019usd),na.rm=TRUE),
                   sc1_ecoli_TAD = sum(sc1_ecoli_loss), ## TAD = total absolute difference across the period (total 15 year loss)
            sc1_ecoli_TPD = sum(sc1_ecoli_loss)/sum(basecase_Y.2019usd), ## TPD = total percentage difference across the period 
            sc2_ecoli_TAD = sum(sc2_ecoli_loss),
            sc2_ecoli_TPD = sum(sc2_ecoli_loss)/sum(basecase_Y.2019usd),
            sc1_saureus_TAD = sum(sc1_saureus_loss),
            sc1_saureus_TPD = sum(sc1_saureus_loss)/sum(basecase_Y.2019usd),
            sc2_saureus_TAD = sum(sc2_saureus_loss),
            sc2_saureus_TPD = sum(sc2_saureus_loss)/sum(basecase_Y.2019usd)) %>%
  select(c("year","iso3c","whoc.region","who.region","Income.group",
           "wb.region",
           "sc1_ecoli_MAD" , "sc1_ecoli_MPD",
           "sc2_ecoli_MAD" ,"sc2_ecoli_MPD",
           "sc1_saureus_MAD","sc1_saureus_MPD",
           "sc2_saureus_MAD","sc2_saureus_MPD" ,
           "sc1_ecoli_TAD" , "sc1_ecoli_TPD",
           "sc2_ecoli_TAD" ,"sc2_ecoli_TPD",
           "sc1_saureus_TAD","sc1_saureus_TPD",
           "sc2_saureus_TAD","sc2_saureus_TPD" )) %>%
  # group_by(iso3c) %>% 
  # slice_max(year, n = 1) %>%
  as.data.table()
