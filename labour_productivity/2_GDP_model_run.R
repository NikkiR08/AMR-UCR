
## LOADING THE PACKAGES
library(dplyr)
library(data.table)
library(rworldmap)

load("Data/macro/macro_data.Rdata")
load("Data/who_whoc_wb.RData")


macro_data[ , Y_PC := GDP/N]
macro_data[ , Y := GDP]
# macro_data[ , g_a_av := 0.01] ## set to moderate for now

run_forecast <- function(x){
x <- as.data.table(x)
x[ ,g_y := as.numeric(NA)]
x[ ,g_k := as.numeric(NA)]
x[ ,g_ypc := as.numeric(NA)]

#### recursive
countries <- unique(x$country)
time <- 2035-2019+1
  
temp_l <- list()
for (i in 1:length(countries)){
  iso <- countries[[i]]
  temp_l[[i]] <- x[country==iso]
  for (j in 2:time){
    temp_l[[i]]$g_k[j] <-(((1-temp_l[[i]]$delta[j])+(temp_l[[i]]$I_Y[j-1]/
                                                       temp_l[[i]]$K_Y[j-1]))/
                            (1+temp_l[[i]]$g_n[j])*(1+temp_l[[i]]$g_wn[j])*(1+temp_l[[i]]$g_lfp_av[j]))-1
    temp_l[[i]]$g_y[j] <- ((1+temp_l[[i]]$g_a_av[j])*((1+temp_l[[i]]$g_k[j])^
                                                           (1-temp_l[[i]]$labsh[j]))*
                                   ((1+temp_l[[i]]$g_h_av[j])^temp_l[[i]]$labsh[j]))-1
    temp_l[[i]]$g_ypc[j] <- ((1+temp_l[[i]]$g_wn[j])*(1+temp_l[[i]]$g_lfp_av[j])*
                               (1+temp_l[[i]]$g_y[j]))-1
    temp_l[[i]]$K_Y[j] <- ((1+temp_l[[i]]$g_k[j])/(1+temp_l[[i]]$g_y[j]))*
                              (temp_l[[i]]$K_Y[j-1])
    temp_l[[i]]$Y_PC[j] <-  temp_l[[i]]$Y_PC[j-1]+(temp_l[[i]]$Y_PC[j-1]*temp_l[[i]]$g_ypc[j])
    temp_l[[i]]$N[j] <- temp_l[[i]]$N[j-1]+(temp_l[[i]]$N[j-1]*temp_l[[i]]$g_n[j])
    temp_l[[i]]$Y[j] <- temp_l[[i]]$N[j]*temp_l[[i]]$Y_PC[j] 
  }
}

i_forecast <- as.data.table(do.call(rbind, temp_l))
return(i_forecast)
}

######## BASECASE
basecase <- macro_data
basecase_forecast <- run_forecast(basecase)

## find all countries to fill missing

## first repeat all the rows for the who data set
wholist <- who_whoc_wb %>% slice(rep(1:n(), each = 2035-2019+1)) %>%
  group_by(iso3c) %>% mutate(id = row_number()) %>% ## add row number 
  as.data.table()

## then use row id to create year variable
wholist[ ,year := id+2018]
wholist <- wholist[ , -c("id")]
wholist[ , country := iso3c]

completing.cases.who <- function(wholist,basecase_forecast){
## merge together total dataset by country
macro_data_missing <- merge(wholist, basecase_forecast, by=c("country","year"),all=TRUE)

# n.na <- subset(macro_data_missing, is.na(Y)) ## checking how many countries use averages
# length(unique(n.na$country))

## replace missing values by income group averages for that year
macro_data_missing <- macro_data_missing %>% group_by(Income.group,year) %>%
  mutate(Y=ifelse(is.na(Y),mean(Y,na.rm=TRUE),Y)) %>%
  mutate(Y_PC=ifelse(is.na(Y_PC),mean(Y_PC,na.rm=TRUE),Y_PC))
return(macro_data_missing)
}

basecase <- completing.cases.who(wholist, basecase_forecast)

############# E coli SCENARIO ####

########### CURRENT RESISTANCE #####
##### MORTALITY IMPACT
sc1_ecoli <- as.data.table(macro_data)
sc1_ecoli[ , WAP := W_N*N]
sc1_ecoli[ , DAP := N-WAP]
sc1_ecoli[, WAP2 := WAP-((0.0000346752)*WAP)]
sc1_ecoli[, DAP2 := DAP-((0.0001277745)*DAP) ]
sc1_ecoli[ , N := WAP2+DAP2]
sc1_ecoli[, W_N := WAP2/N]

sc1_ecoli <- sc1_ecoli %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 


sc1_ecoli_forecast <- run_forecast(sc1_ecoli)

sc1_ecoli <- completing.cases.who(wholist, sc1_ecoli_forecast)

#### putting scenario results together
results <- basecase
results <- as.data.table(merge(results, sc1_ecoli, by=c("country","year")))

##### 100% RESISTANCE #####
##### MORTALITY IMPACT
sc2_ecoli <- as.data.table(macro_data)
sc2_ecoli[ , WAP := W_N*N]
sc2_ecoli[ , DAP := N-WAP]
sc2_ecoli[, WAP2 := WAP-((0.0000888294)*WAP)] 
sc2_ecoli[, DAP2 := DAP-((0.000029313702)*DAP) ]
sc2_ecoli[ , N := WAP2+DAP2]
sc2_ecoli[, W_N := WAP2/N]


sc2_ecoli <- sc2_ecoli %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 

sc2_ecoli_forecast <- run_forecast(sc2_ecoli)

sc2_ecoli <- completing.cases.who(wholist, sc2_ecoli_forecast)


############# S. aureus SCENARIO ####
##### MORTALITY IMPACT
sc1_saureus <- as.data.table(macro_data)
sc1_saureus[ , WAP := W_N*N]
sc1_saureus[ , DAP := N-WAP]
sc1_saureus[, WAP2 := WAP-((0.0000067392)*WAP)]
sc1_saureus[, DAP2 := DAP-((0.00001726272)*DAP) ]
sc1_saureus[ , N := WAP2+DAP2]
sc1_saureus[, W_N := WAP2/N]

sc1_saureus <- sc1_saureus %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 


sc1_saureus_forecast <- run_forecast(sc1_saureus)

sc1_saureus <- completing.cases.who(wholist, sc1_saureus_forecast)

##### MORTALITY IMPACT
sc2_saureus <- as.data.table(macro_data)
sc2_saureus[ , WAP := W_N*N]
sc2_saureus[ , DAP := N-WAP]
sc2_saureus[, WAP2 := WAP-((0.000022464)*WAP)]
sc2_saureus[, DAP2 := DAP-((0.0000575424)*DAP) ]
sc2_saureus[ , N := WAP2+DAP2]
sc2_saureus[, W_N := WAP2/N]


sc2_saureus <- sc2_saureus %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 

sc2_saureus_forecast <- run_forecast(sc2_saureus)

sc2_saureus <- completing.cases.who(wholist, sc2_saureus_forecast)


###### formatting results #####

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
