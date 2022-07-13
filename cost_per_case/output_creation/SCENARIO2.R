#################### SCENARIO 2 - ADJUSTING BED DAY COSTS BY EXCESS COST PER CASE
#######################################################################
##### BOTH AMR AND DRI

############## PACKAGES #################
library(dplyr)
library(data.table)
library(ggplot2)

### DATA & FUNCTIONS #################
load("cost_per_case/outputs/whoc_cc_2019USD.RData")

### AMR DATA
load("cost_per_case/outputs/los_TE.RData")
load("cost_per_case/outputs/costing_TE_AMR_2019USD.RData")

AMR_losTE <- los.TE
AMR_costingTE <- costing.TE.adj

### DRI DATA
load("cost_per_case/outputs/los_TE_DRI.RData")
load("cost_per_case/outputs/costing_TE_DRI_2019USD.RData")

DRI_losTE <- los.TE
DRI_costingTE <- costing.TE.adj

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

### COMBINING AMR AND DRI DATASETS

## check colnames match
colnames(DRI_losTE)==colnames(AMR_losTE)
colnames(DRI_costingTE)==colnames(AMR_costingTE)


los.TE <- rbind(AMR_losTE,DRI_losTE)
costing.TE.adj <- rbind(AMR_costingTE, DRI_costingTE)


#### FINDING THE ADJUSTMENT FACTORS #####

both.los <- los.TE[unique_id_4both>0]
both.cost <- costing.TE.adj[unique_id_4both>0]

## check match
nrow(both.los)==nrow(both.cost)
# TRUE

## check there's no duplicate unique_id_4boths
duplicated(both.los$unique_id_4both)
setorder(both.los,unique_id_4both)
both.los$unique_id_4both
### multiples for EUSA mapping

duplicated(both.cost$unique_id_4both)
setorder(both.cost,unique_id_4both)
both.cost$unique_id_4both
 ### multiples for EUSA mapping

# remove EUSA ones as no WHO-CHOICE available:
both.los <- both.los[iso3c!="EUSA"]
both.cost <- both.cost[iso3c!="EUSA"]
nrow(both.los)==nrow(both.cost)
# remove TWN as no WHO-CHOICE cost available
both.los <- both.los[iso3c!="TWN"]
both.cost <- both.cost[iso3c!="TWN"]
## nrow drops from 105 to 81

### FIRST CALCULATE COST BY COUNTRY
whoc.cc <- as.data.table(whoc.cc.2019)
whoc.cc <- whoc.cc[!is.na(iso3c)]

# merge los with whoc costs by country
los.cost <- merge(both.los, whoc.cc, by="iso3c")

los.cost[ , los.cost := mean_i*TE]

both.cost[ , cost.cost := TE.adj]

both <- merge(los.cost, both.cost,by="unique_id_4both")

## check income groups the same
both$Income.group.x==both$Income.group.y
both$Income.group.y==both$Income.group
# TRUE FOR ALL 

both[ , adj.factor := (cost.cost-los.cost)/los.cost]
both[is.na(adj.factor)] ## empty

### !!! did hand spot check - noticed 1 has TE = 0 so creates -Inf
## have removed this 
both <- both[adj.factor!=-Inf]

## summary stats on type of exposures
both[ ,.N, by=c("syndrome.x","class.x","bacteria.code.x")]
# write.csv(both, file="Data/cost_adj_descriptive_DRIAMR.csv")
both[ ,.N, by=c("Income.group.x")]

### plotting the values over groups
both[ ,flag := .GRP, by = .(class.x,gram.stain.x)]
# key creation
key_creation_DB <- function(both,x){
  x <- as.numeric(x)
  grp1 <- both %>% filter(flag==x) %>%
    select(gram.stain.x, class.x, flag) %>%
    filter(row_number()==1) 
  grp1[ , flag_name := paste(gram.stain.x,class.x, sep=" & ")]
  return(grp1)
}

gp1 <- key_creation_DB(both,1) ## warning message as from list but stil works ok
gp2 <- key_creation_DB(both,2)
gp3 <- key_creation_DB(both,3)
gp4 <- key_creation_DB(both,4)
gp5 <- key_creation_DB(both,5)

gpDBlst <- list(gp1,gp2,gp3,gp4,gp5)
gpDB <- rbindlist(gpDBlst)
both.gpbd <- merge(both, gpDB, by="flag")

ggplot(both.gpbd, aes(as.factor(flag_name),adj.factor)) + geom_point()

## doing the same across WHOC groups and income groups
both[ ,flag := .GRP, by = .(Income.group)]
# key creation
key_creation_I <- function(both,x){
  x <- as.numeric(x)
  grp1 <- both %>% filter(flag==x) %>%
    select(Income.group.x, flag) %>%
    filter(row_number()==1) 
  grp1[ , flag_name := Income.group.x]
  return(grp1)
}

gp1 <- key_creation_I(both,1)
gp2 <- key_creation_I(both,2)
gp3 <- key_creation_I(both,3)

gpIlst <- list(gp1,gp2,gp3)
gpI <- rbindlist(gpIlst)
both.I <- merge(both, gpI, by="flag")

ggplot(both.I, aes(as.factor(flag_name),adj.factor)) + geom_point()
ggplot(both.I, aes(as.factor(flag_name),adj.factor)) + geom_boxplot()

summary(both$adj.factor)
## income group average adjustment factors
### MEDIAN VALUE CHOSEN
av.adj <- both[, median(adj.factor), by = c("Income.group")]
av.adj <- rename(av.adj, adj.factor=V1)
av.adj

## how many studies fed into it
av.adj_N <- both[, length(unique(unique_id_4both)), by = c("Income.group")]
av.adj_N

## set low income to lower middle income as no data currently
av.adj <- as.data.frame(av.adj)
av.adj[4,] <- c(c("Low income",1))
av.adj[4,2] <- av.adj[3,2]

## read in the final data output table -  with AMR and DRI 
load("cost_per_case/outputs/AMR_Results_Table.RData")
AMR <- results
AMR$AMR_or_DRI <- "AMR"

load("cost_per_case/outputs/DRI_Results_Table.RData")
DRI <-results
DRI$AMR_or_DRI <- "DRI"

results.long <- rbind(AMR,DRI)

sc2.results.long <- merge(results.long, av.adj, 
                          by.x="World Bank Income Status",
                          by.y="Income.group")
## formating numeric values that can be used in multiplication
sc2.results.long[ , los.cost := `Mean Cost - from Excess LOS`  ]
sc2.results.long[ , adj.factor := as.numeric(adj.factor)]

sc2.results.long[ , 'Scenario 2 Mean Cost' := los.cost + (los.cost * adj.factor)]

## remove those that don't have LOS costs 
# as can't then adjust
sc2.results.long <- sc2.results.long[!is.na(`Mean Cost - from Excess LOS`)]

length(which(is.na(sc2.results.long$`Scenario 2 Mean Cost`))) ## should be 0 

save(sc2.results.long, file="cost_per_case/outputs/scenario2.results.RData")
# write.csv(sc2.results.long, file="cost_per_case/outputs/scenario2.results.csv")
