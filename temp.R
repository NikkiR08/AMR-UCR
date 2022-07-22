#### column sums for Y_difference
figure_results <- results %>%
  group_by(country) %>%
  mutate( annual_GDP_basecase = Y.x,
          annual_GDP_percapita_basecase = Y_PC.x,
          total_Y_basecase = sum(Y.x, na.rm=TRUE),
          mean_Ypc_basecase = mean(Y_PC.x, na.rm=TRUE),
          annual_GDP_sc1ecoli = Y.y,
          annual_GDP_percapita_sc1ecoli = Y_PC.y,
          total_Y_sc1ecoli= sum(Y.y, na.rm=TRUE),
          mean_Ypc_sc1ecoli = mean(Y_PC.y, na.rm=TRUE)) %>%
  as.data.table()

figure_results <- figure_results[ , c("country","year","annual_GDP_basecase" ,"annual_GDP_percapita_basecase",
                                      "total_Y_basecase" ,"mean_Ypc_basecase",           
                                      "annual_GDP_sc1ecoli" ,"annual_GDP_percapita_sc1ecoli",
                                      "total_Y_sc1ecoli" ,"mean_Ypc_sc1ecoli")]



## taking the totals and the annual amounts by and for 2035 respectively
figure_results <-
  figure_results %>% 
  group_by(country) %>% 
  slice_max(year, n = 1)  ## get the latest year available

#### SCENARIO 2 Ecoli MAPPING 
temp_sc2ecoli <- sc2_ecoli %>%
  group_by(country) %>%
  mutate(annual_GDP_sc2ecoli = Y,
         annual_GDP_percapita_sc2ecoli = Y_PC,
         total_Y_sc2ecoli= sum(Y, na.rm=TRUE),
         mean_Ypc_sc2ecoli = mean(Y_PC, na.rm=TRUE)) %>%
  select(c("country","year",          
           "annual_GDP_sc2ecoli" ,"annual_GDP_percapita_sc2ecoli",
           "total_Y_sc2ecoli" ,"mean_Ypc_sc2ecoli")) %>%
  group_by(country) %>% 
  slice_max(year, n = 1) %>%
  as.data.table()

temp_sc1saureus <- sc1_saureus %>%
  group_by(country) %>%
  mutate(annual_GDP_sc1saureus = Y,
         annual_GDP_percapita_sc1saureus = Y_PC,
         total_Y_sc1saureus = sum(Y, na.rm=TRUE),
         mean_Ypc_sc1saureus = mean(Y_PC, na.rm=TRUE)) %>%
  select(c("country","year",          
           "annual_GDP_sc1saureus" ,"annual_GDP_percapita_sc1saureus",
           "total_Y_sc1saureus" ,"mean_Ypc_sc1saureus")) %>%
  group_by(country) %>% 
  slice_max(year, n = 1) %>%
  as.data.table()

temp_sc2saureus <- sc2_saureus %>%
  group_by(country) %>%
  mutate(annual_GDP_sc2saureus = Y,
         annual_GDP_percapita_sc2saureus = Y_PC,
         total_Y_sc2saureus = sum(Y, na.rm=TRUE),
         mean_Ypc_sc2saureus = mean(Y_PC, na.rm=TRUE)) %>%
  select(c("country","year",          
           "annual_GDP_sc2saureus" ,"annual_GDP_percapita_sc2saureus",
           "total_Y_sc2saureus" ,"mean_Ypc_sc2saureus")) %>%
  group_by(country) %>% 
  slice_max(year, n = 1) %>%
  as.data.table()


### merge together
temp.all <- merge(figure_results, temp_sc2ecoli, by=c("country","year"))
temp.all <- merge(temp.all, temp_sc2ecoli, by=c("country","year"))
temp.all <- merge(temp.all, temp_sc1saureus, by=c("country","year"))
temp.all <- merge(temp.all, temp_sc2saureus, by=c("country","year"))
temp.all <- as.data.table(temp.all)

temp.all[ , total_Y_loss_sc1ecoli :=total_Y_basecase- total_Y_sc1ecoli] ## note its LOSS here hence the way round of the calculations
temp.all[ , total_Y_loss_sc2ecoli := total_Y_basecase-total_Y_sc2ecoli.x] ##  multiple .x and .y - could clean !!!
temp.all[ , total_Y_loss_sc1saureus :=total_Y_basecase- total_Y_sc1saureus]
temp.all[ , total_Y_loss_sc2saureus := total_Y_basecase-total_Y_sc2saureus]

temp.all[ , percentage_Y_loss_sc1ecoli := total_Y_loss_sc1ecoli/total_Y_basecase]
temp.all[ , percentage_Y_loss_sc2ecoli := total_Y_loss_sc2ecoli/total_Y_basecase]
temp.all[ , percentage_Y_loss_sc1saureus := total_Y_loss_sc1saureus/total_Y_basecase]
temp.all[ , percentage_Y_loss_sc2saureus := total_Y_loss_sc2saureus/total_Y_basecase]


temp.all[ , annual_loss_sc1ecoli :=total_Y_basecase- total_Y_sc1ecoli] ## note its LOSS here hence the way round of the calculations
temp.all[ , total_Y_loss_sc2ecoli := total_Y_basecase-total_Y_sc2ecoli.x] ## multiple .x and .y - could clean - but the same anyway
temp.all[ , total_Y_loss_sc1saureus :=total_Y_basecase- total_Y_sc1saureus]
temp.all[ , total_Y_loss_sc2saureus := total_Y_basecase-total_Y_sc2saureus]

## YPC impact of 100% resistance in year 2035
temp.all[ , Ypc_loss_sc2ecoli :=round((annual_GDP_percapita_basecase - annual_GDP_percapita_sc2ecoli.x),2)]

# ### round values for plots - don't use for now
# maps.dat <- temp.all %>% mutate_at(vars("total_Y_loss_sc1ecoli" ,   "total_Y_loss_sc2ecoli" ,
#                                         "total_Y_loss_sc1saureus" ,  "total_Y_loss_sc2saureus"),
#                                    funs(round(., 0))) %>%
#   
#   mutate_at(vars("percentage_Y_loss_sc1ecoli" ,   "percentage_Y_loss_sc2ecoli" ,
#                  "percentage_Y_loss_sc1saureus" ,  "percentage_Y_loss_sc2saureus"),
#             funs(round(., 5))) 
#         select() %>%
#   as.data.table()

##### plots/maps ######

## 100% resistance

joinData <- joinCountryData2Map( temp.all,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "country")
theMap <- mapCountryData( joinData, nameColumnToPlot="total_Y_loss_sc2ecoli", addLegend=FALSE ,
                          colourPalette = "topo", mapTitle = "GDP loss SC2 E. coli vs BaseLine")
do.call( addMapLegend, c(theMap, legendWidth=1, legendMar = 2))

theMap3 <- mapCountryData( joinData, nameColumnToPlot="percentage_Y_loss_sc2ecoli", addLegend=FALSE,
                           colourPalette = "topo",mapTitle = "% GDP loss SC2 E. coli vs BaseLine")
do.call( addMapLegend, c(theMap3, legendWidth=1, legendMar = 2))

theMap2 <- mapCountryData( joinData, nameColumnToPlot="total_Y_loss_sc2saureus", 
                           addLegend=FALSE,colourPalette = "topo",mapTitle = "GDP loss SC2 S. aureus vs BaseLine")
do.call( addMapLegend, c(theMap2, legendWidth=1, legendMar = 2))

theMap4 <- mapCountryData( joinData, nameColumnToPlot="percentage_Y_loss_sc2saureus", 
                           addLegend=FALSE,colourPalette = "topo", mapTitle = "% GDP loss SC2 S. aureus vs Baseline")
do.call( addMapLegend, c(theMap4, legendWidth=1, legendMar = 2))


### full results table ####
#### column sums for Y_difference
figure_results <- results %>%
  group_by(country) %>%
  mutate( annual_GDP_basecase = Y.x,
          annual_GDP_percapita_basecase = Y_PC.x,
          total_Y_basecase = sum(Y.x, na.rm=TRUE),
          mean_Ypc_basecase = mean(Y_PC.x, na.rm=TRUE),
          annual_GDP_sc1ecoli = Y.y,
          annual_GDP_percapita_sc1ecoli = Y_PC.y,
          total_Y_sc1ecoli= sum(Y.y, na.rm=TRUE),
          mean_Ypc_sc1ecoli = mean(Y_PC.y, na.rm=TRUE)) %>%
  as.data.table()

figure_results <- figure_results[ , c("country","year","annual_GDP_basecase" ,"annual_GDP_percapita_basecase",
                                      "total_Y_basecase" ,"mean_Ypc_basecase",           
                                      "annual_GDP_sc1ecoli" ,"annual_GDP_percapita_sc1ecoli",
                                      "total_Y_sc1ecoli" ,"mean_Ypc_sc1ecoli")]


#### SCENARIO 2 Ecoli MAPPING 
temp_sc2ecoli <- sc2_ecoli %>%
  group_by(country) %>%
  mutate(annual_GDP_sc2ecoli = Y,
         annual_GDP_percapita_sc2ecoli = Y_PC,
         total_Y_sc2ecoli= sum(Y, na.rm=TRUE),
         mean_Ypc_sc2ecoli = mean(Y_PC, na.rm=TRUE)) %>%
  select(c("country","year",          
           "annual_GDP_sc2ecoli" ,"annual_GDP_percapita_sc2ecoli",
           "total_Y_sc2ecoli" ,"mean_Ypc_sc2ecoli"))  %>%
  as.data.table()

temp_sc1saureus <- sc1_saureus %>%
  group_by(country) %>%
  mutate(annual_GDP_sc1saureus = Y,
         annual_GDP_percapita_sc1saureus = Y_PC,
         total_Y_sc1saureus = sum(Y, na.rm=TRUE),
         mean_Ypc_sc1saureus = mean(Y_PC, na.rm=TRUE)) %>%
  select(c("country","year",          
           "annual_GDP_sc1saureus" ,"annual_GDP_percapita_sc1saureus",
           "total_Y_sc1saureus" ,"mean_Ypc_sc1saureus")) %>%
  as.data.table()

temp_sc2saureus <- sc2_saureus %>%
  group_by(country) %>%
  mutate(annual_GDP_sc2saureus = Y,
         annual_GDP_percapita_sc2saureus = Y_PC,
         total_Y_sc2saureus = sum(Y, na.rm=TRUE),
         mean_Ypc_sc2saureus = mean(Y_PC, na.rm=TRUE)) %>%
  select(c("country","year",          
           "annual_GDP_sc2saureus" ,"annual_GDP_percapita_sc2saureus",
           "total_Y_sc2saureus" ,"mean_Ypc_sc2saureus")) %>%
  as.data.table()

temp.all.years <- merge(figure_results, temp_sc2ecoli, by=c("country","year"))
temp.all.years <- merge(temp.all.years, temp_sc2ecoli, by=c("country","year"))
temp.all.years <- merge(temp.all.years, temp_sc1saureus, by=c("country","year"))
temp.all.years <- merge(temp.all.years, temp_sc2saureus, by=c("country","year"))
temp.all.years  <- as.data.table(temp.all.years)

# write.csv(temp.all.years, file="Data/macro/GDP_outputs_scenarios.csv")

# write.csv(temp.all, file="Data/macro/GDP_output_countries_total.csv")
