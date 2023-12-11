###################### PRODUCTIVITY COSTS AND COST CONVERSIONS
#######################################################################

############## PACKAGES #################
library(Rilostat)
library(wbstats)
library(data.table)
# library(tabulizer)
library(dplyr)
library(countrycode)

########### DATA ###################

## previous datasets
load("data_all/who_whoc_wb.RData")
load("cost_per_case/outputs/costing_TE.RData")

#######**** Exchange
source("general_functions/inflation.R")

################*********** WHO Choice Cost Estimates ********##########

# ### extract tables
# PATH = "cost_per_case/inputs/input_who_choice.pdf"
# lst <- extract_tables(PATH, encoding="UTF-8") 
# 
# ## separate tables into primary, secondary and tertiary hospital costs
# lst1 <-lst[c(1:5)]   ## primary hospitals
# lst2 <- lst[c(2:10)] ## secondary hospitals
# lst3 <- lst[c(11:15)]  ## tertiary hospitals
# primary.cost <- as.data.frame(do.call(rbind, lst1))
# ## keep just the model predicted cost for now
# primary.cost <- primary.cost[,c(1,3:6)]
# colnames(primary.cost) <- c("region","mean_cost","high95","low95","SD")
# save(primary.cost, file="data_all/primary_cost.RData")

load("cost_per_case/inputs/primary_cost.RData")
primary.cost <- as.data.table(primary.cost)
### primary cost in 2010 USD

################********** CURRENCY CONVERSIONS ***********#################

primary.cost[ , mean_cost := as.numeric(as.character(mean_cost))]
primary.cost[ , high95 := as.numeric(as.character(high95))]
primary.cost[ , low95 := as.numeric(as.character(low95))]
primary.cost[ , SD := as.numeric(as.character(SD))]

## !!! removing NA values (Somalia & Democratic People's Republic of Korea)
primary.cost <- primary.cost[!is.na(mean_cost)]

primary.cost$iso3c <- countrycode(primary.cost$region, origin="country.name", destination="iso3c")


for (i in 1:nrow(primary.cost)){
  primary.cost[i, mean_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                   "mean_cost",inf_xch_4function)]
  primary.cost[i, high95_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                     "high95",inf_xch_4function)]
  primary.cost[i, low95_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                    "low95",inf_xch_4function)]
  primary.cost[i, SD_i := inflation_exchange_PPP(2010,2019,primary.cost[i], 
                                                 "SD",inf_xch_4function)]
  }


whoc.cc.2019 <- merge(primary.cost, who_whoc_wb, by="iso3c", all.x=TRUE,all.y=FALSE)
save(whoc.cc.2019, file="cost_per_case/outputs/whoc_cc_2019USD.RData")

#########literature costing#####

costing.TE.currency <- merge(costing.TE, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)
costing.TE.currency <- costing.TE.currency[iso3c=="EUSA", currency_code := "EUR"]
costing.TE.currency <- costing.TE.currency[iso3c=="EUSA", number := 978]
costing.TE.currency <- costing.TE.currency[iso3c=="EUSA", currency_name := "Euro"]

# converting costs
for (i in 1:nrow(costing.TE.currency)){
  costing.TE.currency[i, TE.adj := cost_adj_lit(2019,costing.TE.currency[i], 
                                                   "TE",inf_xch_4function)]
  costing.TE.currency[i, seTE.adj:= cost_adj_lit(2019,costing.TE.currency[i], 
                                                     "seTE",inf_xch_4function)]
}

## see which ones have no adjusted cost
none.adj <- subset(costing.TE.currency, is.na(TE.adj))

### see inflation script for function information on inflation
### !!! you would need to update this for other data if there were other currencies/iso3 not converted
## at the moment the only non converted used USD so used USA inflation
none.adj[ , iso3c_temp := iso3c]
none.adj[is.na(TE.adj)&cost_currency=="USD",
                    iso3c := "USA"]
for (i in 1:nrow(none.adj)){
  none.adj[i, TE.adj := cost_adj_lit(2019,none.adj[i], 
                                                "TE",inf_xch_4function)]
  none.adj[i, seTE.adj:= cost_adj_lit(2019,none.adj[i], 
                                                 "seTE",inf_xch_4function)]
}
## revert iso3c back
none.adj[ , iso3c := iso3c_temp]
none.adj[, iso3c_temp:=NULL] ## remove column 

## remove the NA ones from full dataset
costing.TE.currency <- costing.TE.currency[!is.na(TE.adj)] ## get rid of NA rows in previous data
costing.TE.adj <- rbind(costing.TE.currency, none.adj) ## rbind the newly filled ones in 

## update costing year/currency to avoid confusion
costing.TE.adj[ , cost_year := 2019]
costing.TE.adj[ , cost_currency:= "USD"]
costing.TE.adj <- costing.TE.adj[ , -c("currency_name","currency_code")]

save(costing.TE.adj, file="cost_per_case/outputs/costing_TE_AMR_2019USD.RData")
