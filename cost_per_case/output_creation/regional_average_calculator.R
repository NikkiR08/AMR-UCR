###### FUNCTIONS FOR CALCULATING REGIONAL AVERAGES FOR THE OUTPUT CSV/RESULTS

library(dplyr)
library(data.table)

#### meta-analyses results don't have WHO region so need to extract that
load("Data/who_whoc_wb.RData")
who <- who_whoc_wb[ , c("iso3c","who.region")]

#### LOADING POPULATION ESTIMATES
N <- as.data.table(read.csv("Data/macro/Population-EstimatesData_092020.csv"))
N <- N[Indicator.Code=="SP.POP.TOTL"]

### keep 2019 values
N <- N[ , c("Country.Code","X2019")]
setnames(N, "X2019", "npop")

n.samples <- 10000

########### LOS ESTIMATES ##########################

## susceptible results #############
load("Data/susceptible_los_results.RData")
  ## note N and who have to be defined before running the function !
  start.time <- Sys.time()
REGIONAL.LOS <- function(los.output.cc.S){
  ## input is an los.output.cc type (see meta analyses scripts)
  ## need to have n.samples, N and who defined before runnin
  
  los.output.cc.S[ , Country.Code := iso3c.x]
  
  combo <- merge(los.output.cc.S, N, by="Country.Code")
  combo <- merge(combo, who, by.x="Country.Code", by.y="iso3c" )
  
  los.cc <- as.data.table(combo)
  los.cc <- los.cc[!is.na(npop)]
  
  #### breaking down by country for sampling ######
  los.cc[ , ID := c(1:nrow(los.cc))]
  
  sample.los.cc <- list()
  
  for (i in 1:max(unique(los.cc$ID))){
    
    ## go by each id
    dt.temp <- los.cc[ID == i]
    
    ## sample
    rnorm.sample.los <- rnorm(n=n.samples, mean=dt.temp$TE.final, sd=dt.temp$seTE.final)
  
    # ## extracting out the variables
    # sample.los.cc[ , i] <- rnorm.sample
    # colnames(sample.los.cc)[i] <- dt.temp$ID
    sample.los.cc[[i]]<- dt.temp$ID
    sample.los.cc[[i]][1:n.samples] <- rnorm.sample.los
    
  }

  los.cc.thin <- los.cc[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain","TE.final","ID","no.studies","level")]
  list.los.cc <- rep(list(los.cc.thin),n.samples)
  
  ## for J represents ID (so each country,drug, bug, syndrome group) and i represents the run
  
  for (i in 1:n.samples){
    temp <- as.data.table(list.los.cc[[i]])
    for (j in 1:max(los.cc$ID)){
      temp[ID==j, TE.final := sample.los.cc[[j]][i]]
    }
    list.los.cc[[i]] <- temp
  }

  
  ##### breaking down by region for averages #####
  ## !! currently inefficient but does the job in terms of getting the averages
  ## likely a faster way using piping and/or apply() functions
  
  regional.averages <- list()
  
  for (i in 1:n.samples){
    temp <- list.los.cc[[i]]
    combo <- merge(temp, N, by.x="iso3c.x", by.y="Country.Code")
    combo <- merge(combo, who, by.x="iso3c.x", by.y="iso3c" )

    ### filter and average
    combo <- combo %>%
    filter(!is.na(npop)) %>%
      group_by(who.region, syndrome, class, gram.stain) %>% 
      summarise(weighted_TE = weighted.mean(TE.final, npop)) 
    
    regional.averages[[i]] <- as.data.table(combo)
    
  }
  
  ## bind to one data.table
  regional.output <- rbindlist(regional.averages)
  
  regional.output <- as.data.table(regional.output)
  
  ## (1) los-based cost
  mean.valuesL <- regional.output[,.(mean_los = mean(weighted_TE, na.rm = TRUE)),
                           by=c("who.region","syndrome", "class", "gram.stain")] 
  
  lowL <- regional.output[,.(low_los = quantile(weighted_TE,0.025, na.rm = TRUE)), 
                   by=c("who.region","syndrome", "class", "gram.stain")] 
  
  highL <- regional.output[,.(high_los = quantile(weighted_TE,0.975, na.rm = TRUE)), 
                    by=c("who.region","syndrome", "class", "gram.stain")] 
  
  los.region <- merge(mean.valuesL, lowL, by = c("who.region","syndrome", "class", "gram.stain"))
  
  los.region <- merge(los.region, highL, by = c("who.region","syndrome", "class", "gram.stain"))
  
  return(los.region)
}

los.region.S <- REGIONAL.LOS(los.output.cc.S)
# save(los.region.S, file="Data/los_region_S.RData")
# write.csv(los.region.S, file="Data/los_region_S.csv")

##### AMRresults #############
load("Data/los_output_cc.RData")

los.region.AMR <- REGIONAL.LOS(los.output.cc)
end.time <- Sys.time()
# save(los.region.AMR, file="Data/los_region_AMR.RData")
# write.csv(los.region.AMR, file="Data/los_region_AMR.csv")

##### DRIresults #############
load("Data/los_output_cc_DRI.RData")

los.region.DRI <- REGIONAL.LOS(los.output.cc.DRI)

# save(los.region.DRI, file="Data/los_region_DRI.RData")
# write.csv(los.region.DRI, file="Data/los_region_DRI.csv")

#################### COSTING REGIONAL VALUES #################

REGIONAL.COSTING <- function(results.long){
  ## input is an results.long type (see meta analyses scripts) - without full labels
  ## need to have n.samples, N and who defined before running
  
  ## calculate se from 95% CI (could update meta code to calculate directly then input here)
  
  results.long[ , cost.se := (high.cost-low.cost)/3.92]
  
  results.long[ , Country.Code := iso3c.x]
  
  combo <- merge(results.long, N, by="Country.Code")
  combo <- merge(combo, who, by.x="Country.Code", by.y="iso3c" )
  
  costing.cc <- as.data.table(combo)
  costing.cc <- costing.cc[!is.na(npop)]
  
  #### breaking down by country for sampling ######
  costing.cc[ , ID := c(1:nrow(costing.cc))]
  
  sample.costing.cc <- list()
  
  for (i in 1:max(unique(costing.cc$ID))){
    
    ## go by each id
    dt.temp <- costing.cc[ID == i]
    
    ## sample
    rnorm.sample.los <- rnorm(n=n.samples, mean=dt.temp$mean.cost, sd=dt.temp$cost.se)
    
    # ## extracting out the variables
    # sample.costing.cc[ , i] <- rnorm.sample
    # colnames(sample.costing.cc)[i] <- dt.temp$ID
    sample.costing.cc[[i]]<- dt.temp$ID
    sample.costing.cc[[i]][1:n.samples] <- rnorm.sample.los
    
  }
  
  costing.cc.thin <- costing.cc[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain","mean.cost","ID")]
  list.costing.cc <- rep(list(costing.cc.thin),n.samples)
  
  ## for J represents ID (so each country,drug, bug, syndrome group) and i represents the run
  
  for (i in 1:n.samples){
    temp <- as.data.table(list.costing.cc[[i]])
    for (j in 1:max(costing.cc$ID)){
      temp[ID==j, mean.cost := sample.costing.cc[[j]][i]]
    }
    list.costing.cc[[i]] <- temp
  }
  
  ##### breaking down by region for averages #####
  ## !! currently inefficient but does the job in terms of getting the averages
  ## likely a faster way using piping and/or apply() functions
  
  regional.averages <- list()
  
  for (i in 1:n.samples){
    temp <- list.costing.cc[[i]]
    combo <- merge(temp, N, by.x="iso3c.x", by.y="Country.Code")
    combo <- merge(combo, who, by.x="iso3c.x", by.y="iso3c" )
    
    ### filter and average
    combo <- combo %>%
      filter(!is.na(npop)) %>%
      group_by(who.region, syndrome, class, gram.stain) %>% 
      summarise(weighted_TE = weighted.mean(mean.cost, npop)) 
    
    regional.averages[[i]] <- combo
    
  }
  
  ## bind to one data.table
  regional.output <- rbindlist(regional.averages)
  
  regional.output <- as.data.table(regional.output)
  
  ## (1) los-based cost
  mean.valuesL <- regional.output[,.(mean_costing = mean(weighted_TE, na.rm = TRUE)),
                                  by=c("who.region","syndrome", "class", "gram.stain")] 
  
  lowL <- regional.output[,.(low_costing = quantile(weighted_TE,0.025, na.rm = TRUE)), 
                          by=c("who.region","syndrome", "class", "gram.stain")] 
  
  highL <- regional.output[,.(high_costing = quantile(weighted_TE,0.975, na.rm = TRUE)), 
                           by=c("who.region","syndrome", "class", "gram.stain")] 
  
  costing.region <- merge(mean.valuesL, lowL, by = c("who.region","syndrome", "class", "gram.stain"))
  
  costing.region <- merge(costing.region, highL, by = c("who.region","syndrome", "class", "gram.stain"))
  
  return(costing.region)
}

# ##### AMRresults #############
load("Data/AMR_Results_Table_AMR_nolabels.RData")

costing.region.AMR <- REGIONAL.COSTING(results)

# save(costing.region.AMR, file="Data/costing_region_AMR.RData")
# write.csv(costing.region.AMR, file="Data/costing_region_AMR.csv")


##### DRIresults #############
load("Data/AMR_Results_Table_DRI_nolabels.RData")

## check right ones loaded in 

costing.region.DRI <- REGIONAL.COSTING(results.long)

# save(costing.region.DRI, file="Data/costing_region_DRI.RData")
# write.csv(costing.region.DRI, file="Data/costing_region_DRI.csv")


########### PRODUCTIVITY ESTIMATES ##########################

load("Data/labour_wage_2019USD.RData")


combo <- merge(labour_productivity_all, N, by.x="iso3c", by.y="Country.Code")


regional.labour <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(who.region) %>% 
  summarise(average_scenario1 = weighted.mean(BaseCase_2019USD, npop),
            average_scenario2 = weighted.mean(Scenario2_2019USD, npop)) %>%
  as.data.table()

# save(regional.labour, file="Data/regional_labour.RData")
# write.csv(regional.labour, file="Data/regional_labour.csv")


#### gdp ####

# gdp <- as.data.table(read.csv("Data/GDP_outputs_scenarios.csv"))
# ### !!! not found in latest runs come back to 

global.country <- read.csv("Data/macro/GDP_output_countries_total.csv")


combo <- merge(global.country, N, by.x="country", by.y="Country.Code")
combo <- merge(combo, who, by.x="country", by.y="iso3c" )

regional.gdp <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(who.region, year) %>% 
  summarise(across(where(is.numeric), ~ weighted.mean(.x, npop, na.rm = TRUE)))

## removing per capita numbers for now
regional.gdp <- regional.gdp[,!grepl("Ypc", colnames(regional.gdp))]

save(regional.gdp, file="Data/regional_gdp.RData")
write.csv(regional.gdp, file="Data/regional_gdp.csv")

## removing per capita numbers for now
global.country  <- global.country[,!grepl("Ypc", colnames(global.country))]
write.csv(global.country, file="Data/country_gdp.csv")

############# antibiotic ##########################

load("Data/antibiotics/country_abx_cost.RData")

combo <- merge(abx_sc1_sc2_output, N, by.x="Country (ISO3 Code)", by.y="Country.Code")
combo <- merge(combo, who, by.x="Country (ISO3 Code)", by.y="iso3c" )


regional.abx <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(who.region, Antibiotic,`MSH Form`, `MSH Dose`) %>% 
  summarise(weighted_cost_Scenario1 = weighted.mean(`Scenario 1 Generic Cost Estimate`, npop),
            weighted_cost_Scenario2 = weighted.mean(`Scenario 2 Cost Estimate`, npop)) %>%
  as.data.table()

# save(regional.abx, file="Data/antibiotics/regional_abx.RData")
# write.csv(regional.abx, file="Data/antibiotics/regional_abx.csv")


################**** GLOBAL AVERAGES *******########
#### meta-analyses results don't have WHO region so need to extract that
load("Data/who_whoc_wb.RData")
who <- who_whoc_wb[ , c("iso3c","who.region")]

#### LOADING POPULATION ESTIMATES
N <- as.data.table(read.csv("Data/macro/Population-EstimatesData_092020.csv"))
N <- N[Indicator.Code=="SP.POP.TOTL"]

### keep 2019 values
N <- N[ , c("Country.Code","X2019")]
setnames(N, "X2019", "npop")

N_WHO <- merge(N, who, by.x="Country.Code", by.y="iso3c")

## load external matching of bugs
load("Data/external_bug_syndrome_matched.RData")

load("Data/costing_region_AMR.RData")
load("Data/costing_region_DRI.RData")

costing.region.AMR$flag <- "AMR"
costing.region.DRI$flag <- "DRI"

## combine
cost.output <- rbind(costing.region.AMR,
                     costing.region.DRI)

global_bug_match <- function(x){
  y <- merge(x, N_WHO, by="who.region", allow.cartesian = TRUE)
  y <- y %>%
    filter(!is.na(npop)) %>%
    group_by(syndrome, class, gram.stain, flag) %>% 
    summarise(global_mean = weighted.mean(mean_costing, npop),
              global_low = weighted.mean(low_costing, npop), ##!!! come back to do work out properly
              global_high = weighted.mean(high_costing,npop)) 
  y <- merge(syndrome_matched, y, by=c("syndrome", "gram.stain", "class"), all=FALSE)
  y <-y[order(y$flag),]
  return(y)
}

bug_matched_costing_global<- global_bug_match(cost.output)
write.csv(bug_matched_costing_global, file = "Data/bug_matched_costing_global.csv")

load("Data/los_region_AMR.RData")
load("Data/los_region_DRI.RData")
load("Data/los_region_S.RData")


los.region.AMR$flag <- "AMR"
los.region.DRI$flag <- "DRI"
los.region.S$flag <- "S"

## combine
los.output <- rbind(los.region.AMR,
                    los.region.DRI)
los.output <- rbind(los.output,
                    los.region.S)
x <- los.output
  y <- merge(x, N_WHO, by="who.region", allow.cartesian = TRUE)
  y <- y %>%
    filter(!is.na(npop)) %>%
    group_by(syndrome, class, gram.stain, flag) %>% 
    summarise(global_mean_los = weighted.mean(mean_los, npop),
              global_low_los = weighted.mean(low_los, npop), ##!!! come back to do work out properly
              global_high_los = weighted.mean(high_los,npop)) 
  y <- merge(syndrome_matched, y, by=c("syndrome", "gram.stain", "class"), all=FALSE)
  y <-y[order(y$flag),]

bug_matched_los_global <- y
write.csv(bug_matched_los_global, file = "Data/bug_matched_los_global.csv")

####### antibiotics 
load("Data/antibiotics/country_abx_cost.RData")

combo <- merge(abx_sc1_sc2_output, N_WHO, by.x="Country (ISO3 Code)", by.y="Country.Code")

global.abx <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(Antibiotic,`MSH Form`, `MSH Dose`) %>% 
  summarise(weighted_cost_Scenario1 = weighted.mean(`Scenario 1 Generic Cost Estimate`, npop),
            weighted_cost_Scenario2 = weighted.mean(`Scenario 2 Cost Estimate`, npop)) %>%
  as.data.table()

write.csv(global.abx, "Data/global_abx.csv")

### productivity

load("Data/labour_wage_2019USD.RData")

combo <- merge(labour_productivity_all, N_WHO, by.x="iso3c", by.y="Country.Code")

global.labour <- combo %>%
  filter(!is.na(npop)) %>%
  summarise(average_scenario1 = weighted.mean(BaseCase_2019USD, npop),
            average_scenario2 = weighted.mean(Scenario2_2019USD, npop)) %>%
  as.data.table()

save(global.labour, file="Data/global_labour.RData")
write.csv(global.labour, file="Data/global_labour.csv")

#### GDP
global.gdp <- combo %>%
  filter(!is.na(npop)) %>%
  group_by(year) %>% 
  summarise(across(where(is.numeric), ~ weighted.mean(.x, npop, na.rm = TRUE)))

## removing per capita numbers for now
global.gdp <- global.gdp[,!grepl("Ypc", colnames(global.gdp))]

save(global.gdp, file="Data/global_gdp.RData")
write.csv(global.gdp, file="Data/global_gdp.csv")
