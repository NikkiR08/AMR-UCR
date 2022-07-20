###################### PRODUCTIVITY COSTS AND COST CONVERSIONS
#######################################################################

############## PACKAGES #################
library(Rilostat)
library(wbstats)
library(data.table)
library(tabulizer)
library(dplyr)
library(countrycode)
library(ggplot2)

# #### testing to check outliers - when running below
# upper_bound <- quantile(employ$employ, 0.975)
# lower_bound <- quantile(employ$employ, 0.025)
# employ_check <- employ[employ<20]


########### USING LOCAL CURRENCY UNITS ####################


########### DATA ###################

load("data_all/who_whoc_wb.RData")

# dat <- get_ilostat(id = 'EAR_4MTH__CUR_NB_A SEX_ECO', segment = 'indicator')
# save(dat, file="Data/ILO_wage.RData")
# load("Data/ILO_wage.RData")
##!! updated data with bulk download from # https://www.ilo.org/shinyapps/bulkexplorer47/ - last accessed May 2021
# filtered Sex - Total, Economic activity - Total, currency - ppp
dat <- read.csv("data_all/EAR_4MTH_SEX_ECO_CUR_NB_A-filtered.csv")

# employ <- get_ilostat(id = 'EMP_DWAP_SEX_AGE_RT_A', segment = 'indicator')
# save(employ, file="data_all/ILO_employ_ratio.RData")
### !!! although note this was saved as employ_rate and have not double checked same when copying over code
load("data_all/ILO_emp_ratio.RData") ## see blow for saving
load("data_all/ilo_dic.RData")

#######**** Exchange
source("general_functions/inflation.R")

#########***** LABOUR PRODUCTIVITY COSTS *****#####
# ## to see table of contents
# toc <- get_ilostat_toc()
# View(toc)

### Average monthly earnings of employees - Harmonized series 

dat <- as.data.table(dat)
dat[ , time := as.numeric(time)] 

# dat <- dat[order(-time),head(.SD, 1) , by = ref_area] # if wanted latest year but keeping all for now
wage <- dat %>%
  group_by(ref_area.label) %>%
  filter(sex.label=="Sex: Total", classif2.label=="Currency: U.S. dollars",
         grepl('Total',classif1.label),time>=1990 & time<=2019) %>%
  select(ref_area.label,time, obs_value, classif1.label) %>%
  setNames(., c(c("country","year","wage","classif1"))) %>%
  as.data.table()

## add back iso3c codes:
wage$iso3c <- countrycode(wage$country, origin="country.name", destination="iso3c") 
# !!! NOTE -  Some values were not matched unambiguously: Kosovo

### used USD to then convert to LCUs 
### (use these instead of ILO LCUs as some countries have multiple currencies and our definitions might not align)

who_whoc_wb <- as.data.table(who_whoc_wb)
## combine to get the abx data for each country
wage_all <- merge(wage,who_whoc_wb, by="iso3c")

wage_all[ , cost_year := year]
wage_all[ , cost_currency := "USD"]

## get local currency units matched
wage_all <- merge(wage_all, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)


for (i in 1:nrow(wage_all)){
  wage_all[i, wage_2019usd  := 
               cost_adj_abx(2019,wage_all[i], 
                            "wage",inf_xch_4function)]
}

# ## employ rates
employ <- as.data.table(employ)
employ[ , time := as.numeric(time)]
employ <- employ %>% filter(sex=="SEX_T",
                            (classif1=="AGE_10YRBANDS_TOTAL"|
                               classif1== "AGE_YTHADULT_YGE15"|
                               classif1=="AGE_AGGREGATE_TOTAL" ),
                            time>=1990 & time<=2019) %>%
  select(ref_area,time, obs_value) %>%
  setNames(., c(c("iso3c","year","employ"))) %>%
  as.data.table()

## remove MLI value as think mistake
##!!! note might need to check here if using other data with esp low values
emply <- employ[employ>=10]

### just taking the latest available data and assuming no growth
wage_latest <- wage_all %>% group_by(iso3c) %>%
  slice_max(year, n = 1) %>% ## get the latest year available
  slice(1) %>% ## if there are more than 1 for each take max value 1 (i.e. if both classif1 totals are present & are slightly different)
  as.data.table()

employ_latest <- employ %>% group_by(iso3c) %>%
  slice_max(year, n = 1) %>% ## get the latest year available
  slice(1) %>%  ## take just one employment rate from the classif1
  as.data.table()

#### !!! might want to deal with multiple classif1 but slightly differing values
### if use distinct() need to add back in the types and pick preference here
### just taking based on how ordered in this

## merge:
both_latest <- merge(wage_latest, employ_latest, by =c("iso3c"), all.x=TRUE, all.y=TRUE)

## updating NA countries:
both_latest$country<- countrycode(both_latest$iso3c, origin="iso3c", destination="country.name") 
## !!! note ANT, KOS not matched 

## need both values to calculate adjusted wage
## note of missing values:
x <- both_latest[is.na(wage_2019usd)] 
x$country ## quite a lot of countries

x <- both_latest[is.na(employ)]
x$country ## Djibouti

# load("Data/ILO_emp_ratio.RData")
# x <- as.data.table(employ)
# x <- x[ref_area=="CHN"|ref_area=="DJI"|ref_area=="KWT"]
# unique(x$classif1)
# only have "AGE_YTHADULT_YGE15" & "AGE_AGGREGATE_TOTAL" classifications
# went back and added in to filter code

both_latest <- both_latest[!is.na(wage_2019usd) & !is.na(employ)]

#### SCENARIO 1: TREND ADJUSTMENT ######

### calculate average growth and model to get both at 2019 values

## first remove duplicate total values based on multiple classif1 being present
employ_trend <- employ %>% group_by(iso3c,year) %>%
  slice(1) %>%  ## take just one employment rate from the classif1
  as.data.table()

#### !!! note
# employ_trend2 <- distinct(employ)
# setdiff(employ_trend2, employ_trend)
# ## employ_trend2 this has more rows per year for countries with multiple values
# ## currently ordered so that takes "AGE_AGGREGATE_TOTAL" value from employ_trend %>% pipe 
# ## so using that one !!! but if you reorder employ_trend before re-running be wary of this

wage_trend <- wage_all %>% group_by(iso3c,year) %>%
  slice(1) %>%  ## take just one employment rate from the classif1
  as.data.table()

wage_trend[ , ID := .GRP, by=.(iso3c)]
wage_trend <- wage_trend[order(iso3c,year)]

lwage <- list() ## create an empty list to fill

for (i in 1:max(wage_trend$ID)){ ## for each country 
  temp <- wage_trend[ID==i] ## filter by that country
  if (nrow(temp)==1){
    ## assume no growth as no data 
    temp[ , wage_2019 := wage_2019usd]
    temp <- temp[ , c("iso3c","wage_2019")]
    lwage[[i]] <- temp
  }else{  
    ## finds average trend in wage growth
    temp[ , g_w := 1+(wage_2019usd-lag(wage_2019usd))/lag(wage_2019usd)]  ## wage differential
    temp[ , g_t := (year-lag(year))] ## year differential
    temp[ , annual_gw := g_w^(1/g_t)] ## annual growth rate
    temp[ , mean_agw := mean(annual_gw, na.rm=TRUE)] ## mean annual growth rate 
    temp[ , last_year := max(year)] ## latest year available
    temp[ , last_wage := temp[year==last_year, wage_2019usd]] ## last wage
    temp <- tail(temp,1) ## taking 1 value
    temp[ , wage_2019 := last_wage*(mean_agw^(2019-last_year))] ## calculating wage in 2019
    temp <- temp[ , c("iso3c","wage_2019")]
    lwage[[i]] <- temp
  }
}

wage_2019 <- rbindlist(lwage)

## doing the same for employ rate
employ_trend[ , ID := .GRP, by=.(iso3c)]
employ_trend <- employ_trend[order(iso3c,year)] ## order by year

lemploy <- list() ## create an empty list to fill

for (i in 1:max(employ_trend$ID)){
  temp <- employ_trend[ID==i]
  if (nrow(temp)==1){
    ## assume no growth as no data 
    temp[ , employ_2019 := employ]
    temp <- temp[ , c("iso3c","employ_2019")]
    lemploy[[i]] <- temp
  }else{
    temp[ , g_w := 1+(employ-lag(employ))/lag(employ)]
    temp[ , g_t := (year-lag(year))]
    temp[ , annual_gw := g_w^(1/g_t)]
    temp[ , mean_agw := mean(annual_gw, na.rm=TRUE)]
    temp[ , last_year := max(year)]
    temp[ , last_employ := temp[year==last_year, employ]]
    temp <- tail(temp,1)
    temp[ , employ_2019 := last_employ*(mean_agw^(2019-last_year))]
    temp <- temp[ , c("iso3c","employ_2019")]
    lemploy[[i]] <- temp
  }
}

employ_2019 <- rbindlist(lemploy)

employ_2019[employ_2019>=100, employ_2019 := 99.9]

### merge by year to get those with same cost year & employ rate year
both_trend <- merge(wage_2019, employ_2019, by =c("iso3c"), all.x=TRUE, all.y=FALSE)

### countries with no employ rate still
x <- both_trend[is.na(employ_2019)] ## NA is the average of those with no iso3c
# can be dropped (is later below with !is.na(Scenario1_est))
# the others are GIB and DJI, these will also end up being dropped 

both_trend[ , `Scenario 1 - 2019 USD` := wage_2019*(employ_2019/100)]

## drop NA values
both_trend <- both_trend[!is.na(Scenario1_est)] 

######## scenario 2###############
## no trends incorporated:
both_latest <- merge(wage_latest, employ_latest, by =c("iso3c"), all.x=TRUE, all.y=TRUE)

## updating NA countries:
both_latest$country<- countrycode(both_latest$iso3c, origin="iso3c", destination="country.name") 
## !!! note ANT not matched 

## need both values to calculate adjusted wage
## note of missing values:
x <- both_latest[is.na(wage)] 
x$country

x <- both_latest[is.na(employ)]
x$country

both_latest <- both_latest[!is.na(wage) & !is.na(employ)]

both_latest[ , `Scenario 2 - 2019 USD`:= wage_2019usd*(employ/100)] 

#### create results table with both values

## keep columns
both_latest <- both_latest[ , c("iso3c","Scenario 2 - 2019 USD")]
both_trend <- both_trend[ , c("iso3c","Scenario 1 - 2019 USD")]

both <- merge(both_latest, both_trend, by="iso3c")
both[ , BaseCase_2019USD := round(`Scenario 1 - 2019 USD`,2)]
both[ , Scenario2_2019USD := round(`Scenario 2 - 2019 USD`,2)]

### merge with WHO_Choice list of countries
labour_productivity <- merge(who_whoc_wb, both, by="iso3c", all.x=TRUE)
### income group averages for those with NA values

## add a flag so know which ones are based on averages
labour_productivity <- as.data.table(labour_productivity)
labour_productivity[         , Missing_Data_Flag := "no"]
labour_productivity[is.na(BaseCase_2019USD), Missing_Data_Flag := "yes"]

flag_no <- labour_productivity[Missing_Data_Flag== "no"]
flag_yes <- labour_productivity[Missing_Data_Flag== "yes"]

av.adjwage1 <- labour_productivity[, mean(BaseCase_2019USD, na.rm=TRUE), 
                                   by = c("Income.group")]
av.adjwage1 <- rename(av.adjwage1, BaseCase_2019USD=V1)

## scenario 2 averages by income group:
av.adjwage2 <- labour_productivity[, mean(Scenario2_2019USD, na.rm=TRUE), 
                                   by = c("Income.group")]
av.adjwage2 <- rename(av.adjwage2, Scenario2_2019USD=V1)

flag_yes_m <- merge(flag_yes, av.adjwage1, by="Income.group")
flag_yes_m <- merge(flag_yes_m, av.adjwage2, by="Income.group")

## tidy up before binding back together
flag_yes_m[ , BaseCase_2019USD := BaseCase_2019USD.y]
flag_yes_m[ , Scenario2_2019USD := Scenario2_2019USD.y]

flag_yes_m <- flag_yes_m[ , -c("BaseCase_2019USD.x" ,"Scenario2_2019USD.x",
                               "BaseCase_2019USD.y" , "Scenario2_2019USD.y")]

labour_productivity_all <- rbind(flag_yes_m, flag_no)

labour_productivity_all <- labour_productivity_all[ , -c("Scenario 2 - 2019 USD",
                                                         "Scenario 1 - 2019 USD", "Missing_Data_Flag" )]

save(labour_productivity_all, file="labour_productivity/outputs/labour_wage_2019USD.RData")
write.csv(labour_productivity_all, file="labour_productivity/outputs/labour_wage_2019USD.csv")

#### PLOTS AFTER REGIONAL AVERAGES SCRIPT ####
load("Data/regional_labour.RData")

hc_region <- melt(regional.labour, id=c("who.region"))
ggplot(data=hc_region, aes(x=who.region, y=value, fill=`variable`)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()

## income group averages
Y_av <- merge(av.adjwage1, av.adjwage2, by="Income.group")

## missing data
length(which(labour_productivity$Missing_Data_Flag=="yes"))
length(which(labour_productivity$Missing_Data_Flag=="no"))
