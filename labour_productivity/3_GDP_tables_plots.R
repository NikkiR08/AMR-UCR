##### CREATING THE GDP OUTPUTS FROM THE MODEL RESULTS

###loading libraries and data
library(tidyverse)
library(data.table)
library(rworldmap)

load("labour_productivity/outputs/gdp_results_all.RData")

### tables #####

### !!! next iteration make a function that does for each 
## scenario so not copying and pasting the col names of interest

## run first with 2019 to check the difference is 0 - it is
results_GDP_difference <- results %>%
  mutate(sc1_ecoli_loss = (basecase_Y.2019usd-sc1_ecoli_Y.2019usd), 
         sc2_ecoli_loss = (basecase_Y.2019usd-sc2_ecoli_Y.2019usd),
         sc1_saureus_loss = (basecase_Y.2019usd-sc1_saureus_Y.2019usd),
         sc2_saureus_loss = (basecase_Y.2019usd-sc2_saureus_Y.2019usd)) %>%
  as.data.table()

## remove 2019
results_GDP_difference <- results_GDP_difference[year!=2019]

save(results_GDP_difference, file="labour_productivity/outputs/results_GDP_difference.RData")

## get average and total values

results_GDP <- results_GDP_difference %>% 
  group_by(iso3c) %>%
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
  slice_max(year, n = 1) %>% ## get the latest year available - the numbers should be the same across years
  select(c("iso3c","whoc.region","who.region","Income.group",
           "wb.region",
           "sc1_ecoli_MAD" , "sc1_ecoli_MPD",
           "sc2_ecoli_MAD" ,"sc2_ecoli_MPD",
           "sc1_saureus_MAD","sc1_saureus_MPD",
           "sc2_saureus_MAD","sc2_saureus_MPD" ,
           "sc1_ecoli_TAD" , "sc1_ecoli_TPD",
           "sc2_ecoli_TAD" ,"sc2_ecoli_TPD",
           "sc1_saureus_TAD","sc1_saureus_TPD",
           "sc2_saureus_TAD","sc2_saureus_TPD" )) %>%
  as.data.table()

write.csv(results_GDP, file="labour_productivity/outputs/average_total_GDP.csv")

##### for regional averages ######
load("labour_productivity/inputs/N_adapted.RData")

## just get 2019 population weighting
N <- N[year==2019 & 	
         Indicator.Code == "SP.POP.TOTL"]
names(N)[names(N) == 'value'] <- 'npop'
N <- N[ ,-c("year")]
## in future iterations could build into the weighting over years 
## and then provide summary statistics on those regionally weighted averages ?

combo <- merge(results_GDP, N, by.x="iso3c", by.y="Country.Code")

regional.gdp <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(who.region) %>% 
  summarise(across(where(is.numeric), ~ weighted.mean(.x, npop, na.rm = TRUE)))%>%
mutate_if(is.numeric, round, 6) %>% ## !!! remove this if want to use values further and keep accuracy
  as.data.table() 
  ### !!! add code to format table so % for PD and 0 dp for AD columns
  ### for now I do in excel post
write.csv(regional.gdp, file="labour_productivity/outputs/regional_GDP_impacts.csv")

###### PLOTS ################

#### add code to log numerical columns for plots

map_data <- results_GDP %>% 
  mutate_at(vars(ends_with("TAD")|ends_with("TPD")), list(lg = ~log(.)))

joinData <- joinCountryData2Map( map_data,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "iso3c")

mapping_function <- function(x, y){
##!!!
  ## doing alot by hand for now but next time try to integrate labels
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

mapping_function(joinData, "sc1_ecoli_TAD_lg")
mapping_function(joinData, "sc2_ecoli_TAD_lg")

## remove log for % diff maps
mapping_function_nolog <- function(x, y){
  ##!!!
  ## doing alot by hand for now but next time try to integrate labels
  ## e.g. adding back transformed scale numbers and titles
  theMap <- mapCountryData( x, nameColumnToPlot=y, addLegend=FALSE ,
                            colourPalette = "terrain", mapTitle = y)
  
  labs <- theMap$cutVector
  labs <- format(labs, scientific=FALSE)
  
  do.call( addMapLegend, c(theMap,
                           legendWidth=1, legendMar = 2,
                           legendIntervals='data',
                           legendLabels='all'))
  
  print(labs)
}

joinData <- joinCountryData2Map( results_GDP,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "iso3c")
mapping_function_nolog(joinData, "sc1_ecoli_TPD")
mapping_function_nolog(joinData, "sc2_ecoli_TPD")

##### some calculations for paper #####
sum(results_GDP$sc2_ecoli_TAD)
sum(results_GDP$sc2_saureus_TAD)

0.005*6
