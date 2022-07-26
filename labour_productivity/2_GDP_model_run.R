############ RUNNING THE GDP MODELS


## LOADING THE PACKAGES
library(tidyverse)
library(dplyr)
library(data.table)
library(rworldmap)
library(zoo)

## LOADING THE DATA

load("labour_productivity/outputs/macro_data.Rdata")
load("data_all/who_whoc_wb.RData")

###### CONVERT TO 2019 USD ######
source("general_functions/inflation.R")
load("data_all/who_whoc_wb.RData")

converting.2019usd.nominal <- function(basecase_forecast){
  all <- as.data.table(basecase_forecast)
  
  all[ , cost_year := 2010]
  all[ , cost_currency := "USD"]
  
  ## get local currency units matched
  all <- merge(all, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)
  
  # converting costs
  for (i in 1:nrow(all)){
    all[i, Y.2019usd := cost_adj_abx(2019,all[i],"Y",
                                     inf_xch_4function)]
    all[i, Y_PC.2019usd := cost_adj_abx(2019,all[i],"Y_PC",
                                        inf_xch_4function)]
  }
  return(all)
}


##### FUNCTIONS AND DATA TO HELP FILL IN MISSING DATA LATER #####
## first repeat all the rows for the who data set
wholist <- who_whoc_wb %>% slice(rep(1:n(), each = 2035-2019+1)) %>%
  group_by(iso3c) %>% mutate(id = row_number()) %>% ## add row number 
  as.data.table()

## then use row id to create year variable
wholist[ ,year := id+2018]
wholist <- wholist[ , -c("id")]
wholist[ , country := iso3c]

#### re-merge labour and N data for those with missing other day
N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))
N <- N[Indicator.Code=="SP.POP.TOTL"| ## total population
         Indicator.Code=="SP.POP.1564.TO.ZS"] ## working age % of whole population
## reshape data so year are a variable not column names:
yearsreshape <- colnames(N)
yearsreshape <- yearsreshape[-(1:4)]
N <- melt(N, measure.vars = dput(as.character(yearsreshape)),
          variable.name = "year", value.name = "value") ## melt columns to a year variable
N$year = as.numeric(gsub("\\X", "", N$year)) ## remove x value across them and turn numeric
N <- N[year %in% c(2019:2035)]  ### get the years we want
N <- N[ , c("Country.Code","Indicator.Code","year","value")]

### creation of the growth rates
N <-  dcast(N, Country.Code + year ~ Indicator.Code, value.var="value", fill=0)
N <- N %>%
  group_by(Country.Code) %>% # For each country calculate...
  mutate(W_N = SP.POP.1564.TO.ZS/100) %>% 
  as.data.table() 
N <- N[ , -c("SP.POP.1564.TO.ZS")]
colnames(N) <- c("country","year","N","W_N")

##### AMR INPUTS ###################
## SPECIFYING THE INPUTS
ecoli.base.wk <- 0.000011442816
ecoli.100.wk <- 0.0000346752
ecoli.base.dp <- 0.000029313702
ecoli.100.dp <- 0.0000888294

mrsa.base.wk <- 0.0000067392
mrsa.100.wk <- 0.000022464
mrsa.base.dp <- 0.00001726272
mrsa.100.dp <- 0.0000575424

#### RUNNING THE MODEL ###################
macro_data[ , Y_PC := GDP/N]
macro_data[ , Y := GDP]
# macro_data[ , g_a_av := 0.01] ## currently use macro_data values 

### this is the initial 2019 value
## remove GDP column to avoid confusion, should use Y
macro_data <- macro_data[ , -c("GDP")]

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
### handing countries with missing country estimates
completing.cases.who <- function(wholist,basecase_forecast,ecoli.base.wk,ecoli.base.dp){
## merge together total dataset by country
macro_data_all <- merge(wholist, basecase_forecast, by=c("country","year"),all=TRUE)
# n.na <- subset(macro_data_missing, is.na(Y)) ## checking how many countries use averages
# length(unique(n.na$country))

## if missing - merge back in pop projection numbers
temp_all <- merge(macro_data_all, N, by.x=c("iso3c","year"),
                  by.y=c("country","year"))
temp_all <- as.data.table(temp_all)

## add a flag for which ones are the imputs ones
temp_all[ , missing_flag := 0]
temp_all[is.na(Y_PC), missing_flag := 1]

temp_all[ missing_flag==1, WAP := W_N.y*N.y]
temp_all[ missing_flag==1, DAP := N.y-WAP]
temp_all[ missing_flag==1, WAP2 := WAP-((ecoli.base.wk)*WAP)]
temp_all[ missing_flag==1, DAP2 := DAP-((ecoli.base.dp)*DAP) ]
temp_all[ missing_flag==1, N.x := WAP2+DAP2]
temp_all[ missing_flag==1, W_N.x := WAP2/N.x]

## if WAP is na remove from analysis
temp_all <- temp_all[!is.na(WAP)|missing_flag==0]
### currently removed: "AND" "DMA" "ERI" "KNA" "MCO" "MHL" "NRU" "PLW" "SMR" "TUV"

## replace missing Y per capita values by income group averages for that year
macro_data2 <- temp_all %>% group_by(Income.group,year) %>%
  mutate_at("Y_PC", na.aggregate)%>%
  as.data.table()

## checking doing the right calc
# test <- temp_all[Income.group=="Low income"&year==2020]
# mean(test$Y_PC,na.rm=TRUE) ## checked against a missing low income and matches

## update Y as a function on N and Y_PC
macro_data2[is.na(Y) & year==2019,Y := Y_PC*N.y] ## as model starts from 2019 onwards
macro_data2[is.na(Y) & year!=2019,Y := Y_PC*N.x] 

## relabelling and getting rid of non-need cols
macro_data2 <- macro_data2[, -c( "N.y", "W_N.y","WAP" , "DAP" ,"WAP2" ,"DAP2" )]

return(macro_data2)
}

basecase_forecast <- completing.cases.who(wholist, basecase_forecast,0,0)

############# E coli SCENARIO ####

########### CURRENT RESISTANCE #####
##### MORTALITY IMPACT
sc1_ecoli <- as.data.table(macro_data)
sc1_ecoli[ , WAP := W_N*N]
sc1_ecoli[ , DAP := N-WAP]
sc1_ecoli[, WAP2 := WAP-((ecoli.base.wk)*WAP)]
sc1_ecoli[, DAP2 := DAP-((ecoli.base.dp)*DAP) ]
sc1_ecoli[ , N := WAP2+DAP2]
sc1_ecoli[, W_N := WAP2/N]

sc1_ecoli <- sc1_ecoli %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 


sc1_ecoli_forecast <- run_forecast(sc1_ecoli)
sc1_ecoli_forecast <- completing.cases.who(wholist, sc1_ecoli_forecast, ecoli.base.wk, ecoli.base.dp)

##### 100% RESISTANCE #####
##### MORTALITY IMPACT
sc2_ecoli <- as.data.table(macro_data)
sc2_ecoli[ , WAP := W_N*N]
sc2_ecoli[ , DAP := N-WAP]
sc2_ecoli[, WAP2 := WAP-((ecoli.100.wk)*WAP)] 
sc2_ecoli[, DAP2 := DAP-((ecoli.100.dp)*DAP) ]
sc2_ecoli[ , N := WAP2+DAP2]
sc2_ecoli[, W_N := WAP2/N]


sc2_ecoli <- sc2_ecoli %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 

sc2_ecoli_forecast <- run_forecast(sc2_ecoli)
sc2_ecoli_forecast  <- completing.cases.who(wholist, sc2_ecoli_forecast,
                                            ecoli.100.wk,ecoli.100.dp)


############# S. aureus SCENARIO ####
##### MORTALITY IMPACT
sc1_saureus <- as.data.table(macro_data)
sc1_saureus[ , WAP := W_N*N]
sc1_saureus[ , DAP := N-WAP]
sc1_saureus[, WAP2 := WAP-((mrsa.base.wk)*WAP)]
sc1_saureus[, DAP2 := DAP-((mrsa.base.dp)*DAP) ]
sc1_saureus[ , N := WAP2+DAP2]
sc1_saureus[, W_N := WAP2/N]

sc1_saureus <- sc1_saureus %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 


sc1_saureus_forecast <- run_forecast(sc1_saureus)

sc1_saureus_forecast <- completing.cases.who(wholist, sc1_saureus_forecast,
                                             mrsa.base.wk, mrsa.base.dp)

##### MORTALITY IMPACT
sc2_saureus <- as.data.table(macro_data)
sc2_saureus[ , WAP := W_N*N]
sc2_saureus[ , DAP := N-WAP]
sc2_saureus[, WAP2 := WAP-((mrsa.100.wk)*WAP)]
sc2_saureus[, DAP2 := DAP-((mrsa.100.dp)*DAP) ]
sc2_saureus[ , N := WAP2+DAP2]
sc2_saureus[, W_N := WAP2/N]


sc2_saureus <- sc2_saureus %>%
  group_by(country) %>% # For each country calculate...
  mutate(g_n = (N - lag(N))/lag(N), # growth rate of population
         g_wn = (W_N - lag(W_N))/lag(W_N)) %>% 
  as.data.table() 

sc2_saureus_forecast <- run_forecast(sc2_saureus)
sc2_saureus_forecast <- completing.cases.who(wholist, sc2_saureus_forecast,
                                             mrsa.100.wk, mrsa.100.dp)

####### converting to 2019 USD ####
basecase_forecast <- converting.2019usd.nominal(basecase_forecast)
sc1_ecoli_forecast <- converting.2019usd.nominal(sc1_ecoli_forecast)
sc2_ecoli_forecast <- converting.2019usd.nominal(sc2_ecoli_forecast)
sc1_saureus_forecast <- converting.2019usd.nominal(sc1_saureus_forecast)
sc2_saureus_forecast <- converting.2019usd.nominal(sc2_saureus_forecast)

###### CREATING RESULTS TABLE #####

### creating a function to clean each result before merging
cleaning.results <- function(basecase_forecast,t){
basecase_forecast <- basecase_forecast %>%
  filter(!is.na(Y.2019usd) & !is.na(Y_PC.2019usd) ) %>%
  rename_at(c("Y.2019usd","Y_PC.2019usd"), ~ paste(t, ., sep = "_")) %>%
  as.data.table()
 
basecase_forecast <- basecase_forecast[ ,- c( "g_lfp_av"   ,  
                         "I_Y"     ,      "g_IY",   
                         "g_IY_av"   ,    "K_Y"   ,       
                         "g_h","g_a"   ,
                         "g_h_av" , "g_a_av"  ,     
                         "labsh"  ,  "delta" , "N.x", "g_n" ,        
                        "g_wn"  ,"W_N.x" ,  
                         "g_y"  ,"g_k" , "g_ypc","Y","Y_PC",
                        "cost_currency","country.y","currency_name",
                        "currency_code","number","cost_year")]
  return(basecase_forecast)
}

basecase_results <- cleaning.results(basecase_forecast,"basecase")
sc1_ecoli_results <- cleaning.results(sc1_ecoli_forecast,"sc1_ecoli")
sc2_ecoli_results <- cleaning.results(sc2_ecoli_forecast,"sc2_ecoli")
sc1_saureus_results <- cleaning.results(sc1_saureus_forecast,"sc1_saureus")
sc2_saureus_results<- cleaning.results(sc2_saureus_forecast,"sc2_saureus")

results <- list(basecase_results,
                sc1_ecoli_results,
                sc2_ecoli_results,
                sc1_saureus_results,
                sc2_saureus_results)

results <- results %>% reduce(full_join, by=c("iso3c","year",
                                              "country.x","who.region", 
                                              "whoc.region","wb.region" , "Income.group"))

save(results, file="labour_productivity/outputs/gdp_results_all.RData")
write.csv(results, file="labour_productivity/outputs/gdp_results_all.csv")
