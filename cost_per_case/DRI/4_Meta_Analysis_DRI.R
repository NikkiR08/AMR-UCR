###################### GROUPING AND META-ANALYSIS
#######################################################################

############## PACKAGES #################
library(stringr)
library(dplyr)
library(data.table)
# library(meta) used older version of meta
install.packages("remotes")
remotes::install_github("guido-s/meta", ref = "R-book-first-edition")
library(meta)

########### DATA ########################
load("cost_per_case/outputs/los_TE.RData")
load("cost_per_case/outputs/costing_TE_AMR_2019USD.RData")
load("cost_per_case/outputs/whoc_cc_2019USD.RData")

## load data dictionaries
load("data_all/who_whoc_wb.RData")
load("data_all/dic_bug.RData")
load("data_all/dic_drug.RData")
load("cost_per_case/outputs/eusa.RData")

#### grouping syndromes

## grouping B-J and J
los.TE[syndrome=="J", syndrome := "B-J"]
costing.TE.adj[syndrome=="J", syndrome := "B-J"]

## grouping INF and COL/INF
los.TE[syndrome=="INF", syndrome := "COL/INF"]
costing.TE.adj[syndrome=="INF", syndrome := "COL/INF"]

######### FUNCTIONS ###########
who_whoc_wb <- as.data.table(who_whoc_wb)
who_whoc_wb <- who_whoc_wb[!is.na(iso3c)] ## although none got dropped in last run just for running purposes

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

## set the number of sample runs 
n.samples <- 1000

pb = txtProgressBar(min = 0, max = n.samples, initial = 0, style = 3)

meta.grouping <- function(x){

 # x <- los.TE ## use when testing function changes (REMEMBER TO NOT USE IN MAIN FUNCTION)

  # x is either "los.TE" or "costing.TE.adj" from the previous scripts
  # output is a data.table with estimates for the groupings which we have
  # atleast global estimates for

l.est <- as.data.table(x)
## remove NA values - !!! check which ones get dropped
l.est <- l.est[!is.na(TE)] ## none dropped in last running 
l.est <- l.est[!is.na(seTE)] ## none dropped in last running
l.est <- l.est[!seTE==Inf] ## 1 dropped for LOS - Spindel et al 2005 for los.TE, 0 for costing.TE
l.est <- l.est[!seTE==0] ## 1 dropped for LOS - Ericson et al 2015 , 0 for costing.TE
l.est[ , seTE := abs(seTE)]
l.est[ , Income.group := as.character(Income.group)]
# l.est <- l.est[!is.na(Income.group)]
# l.est <- l.est[!is.na(whoc.region)]

###### creating all options #####
### creating the data.table that then fill with all combinations
region <- c(unique(who_whoc_wb$iso3c), "EuSA")
n.region <- length(region)
## number of syndromes is currently just for any we have estimates for
synd <- unique(l.est$syndrome)
n.synd <- length(synd)
synd.tb <- c("RTI")
n.synd.tb <- length(synd.tb)

### define classes of interest per gram stain/type
class.gp <- c("glycopeptides","penicillins")              
class.gn <- c("3g cephalosporins","carbapenems")    
class.tb <- c("mdr","xdr")

##GP
output.gp <- data.table(class=rep(class.gp, (n.synd*n.region)))
output.gp[ , syndrome := rep(synd, each=((length(class.gp))), len=nrow(output.gp))]
output.gp[ , region := rep(region, each=n.synd*((length(class.gp))) , len=nrow(output.gp))]
output.gp[ , gram.stain := "gp"]
##GN
output.gn <- data.table(class=rep(class.gn, (n.synd*n.region)))
output.gn[ , syndrome := rep(synd, each=((length(class.gn))), len=nrow(output.gn))]
output.gn[ , region := rep(region, each=n.synd*((length(class.gn))) , len=nrow(output.gn))]
output.gn[ , gram.stain := "gn"]
##TB
output.tb <- data.table(class=rep(class.tb, (n.synd.tb*n.region)))
output.tb[ , syndrome := rep(synd.tb, each=((length(class.tb))), len=nrow(output.tb))]
output.tb[ , region := rep(region, each=n.synd.tb*((length(class.tb))) , len=nrow(output.tb))]
output.tb[ , gram.stain := "tb"]

## combine into one dataset
l = list(output.gp,output.gn, output.tb)
dt.all <- rbindlist(l, use.names=TRUE)
rm(l)
colnames(dt.all)[colnames(dt.all) == 'region'] <- 'iso3c'
dt.all <- merge(dt.all, who_whoc_wb, by="iso3c")
dt.all <- merge(dt.all, l.est, by=c("iso3c","syndrome", "class", "gram.stain"), all.x = TRUE, all.y=TRUE)
dt.all <- dt.all[ ,-c("who.region.y" ,
                 "whoc.region.y","wb.region.y",
                 "Income.group.y")]
colnames(dt.all)[colnames(dt.all) == 'who.region.x'] <- 'who.region'
colnames(dt.all)[colnames(dt.all) == 'whoc.region.x'] <- 'whoc.region'
colnames(dt.all)[colnames(dt.all) == 'wb.region.x'] <- 'wb.region'
colnames(dt.all)[colnames(dt.all) == 'Income.group.x'] <- 'Income.group'

## relabelling those not in WHOC list
dt.all[iso3c=="TWN", wb.region:="East Asia & Pacific"]
dt.all[iso3c=="TWN", Income.group:="High income"]
dt.all[iso3c=="HKG", wb.region:="East Asia & Pacific"]
dt.all[iso3c=="HKG", Income.group:="High income"]

## adding EuSA region
# get unique combinations from EuSA.RData
EuSA <- EuSA %>% group_by(who.region,wb.region,Income.group) %>%
  filter(row_number() == 1)%>% ## take just 1 per region combination
  select("iso3c","who.region","whoc.region","Income.group","wb.region") %>%
  as.data.table()

## merge into dt.all 
# get the EuSA ones:
temp.EuSA <- dt.all[iso3c=="EUSA"]
temp.EuSA <- merge(temp.EuSA, EuSA, by="iso3c", allow.cartesian = TRUE)
## allow.cartesian as want for all in temp.EuSA to be matched with each group

temp.EuSA <- temp.EuSA %>% 
  select(!ends_with(".x")) %>% ## removing unneeded columns
  group_by(row_id, syndrome, class, gram.stain, who.region.y,Income.group.y) %>%
  filter(row_number() == 1)%>% ## take just 1 per study & bug/drug/syndrome + who.region/region combination
  as.data.table()

colnames(temp.EuSA)[colnames(temp.EuSA) == 'who.region.y'] <- 'who.region' ## match column names to data.table with main results
colnames(temp.EuSA)[colnames(temp.EuSA) == 'whoc.region.y'] <- 'whoc.region'
colnames(temp.EuSA)[colnames(temp.EuSA) == 'wb.region.y'] <- 'wb.region'
colnames(temp.EuSA)[colnames(temp.EuSA) == 'Income.group.y'] <- 'Income.group'

dt.all <- dt.all[iso3c!="EUSA"]
dt.all <- rbind(dt.all, temp.EuSA)

### !!! note because of this - for who region (not used), WB region 
## and global groupings, you need to remove duplicate rows from EUSA 
## manip before counting otherwise will double count/use studies
## not needed for income groups as that's the duplication

dt.all[ , group_whoc := .GRP, by =.(whoc.region, syndrome, class, gram.stain)]
dt.all[ , group_income := .GRP, by =.(Income.group,syndrome, class, gram.stain)]
dt.all[ , group_wbregion := .GRP, by =.(wb.region,syndrome, class, gram.stain)]
dt.all[ , group_global := .GRP, by =.(syndrome, class, gram.stain)]


#### grouping information by WHOC regions ######

n.studies <- dt.all %>%
  filter(!is.na(whoc.region)) %>% ## remove NA values otherwise grouped into own group
  as.data.table()
## have to break pipe to use data.table unique
  n.studies <- unique(n.studies,by=c("group_whoc","row_id"))  ## removing duplicate studies from region mapping
  n.studies <- n.studies %>% 
    group_by(group_whoc) %>% 
  count(!is.na(TE)) 

n.studies <- as.data.table(n.studies)
colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
n.studies[any== FALSE, n := 0]

## if n=1 just use that study
n.studies.1 <- subset(n.studies, n==1)
n.1 <- merge(n.studies.1, dt.all, by="group_whoc")
n.1 <- n.1[ , c("whoc.region","syndrome", "class", "gram.stain","TE","seTE", "group_whoc","group_income","group_global","n")]
n.1 <- n.1[!is.na(TE)]

## form final list through merging n.1 and n.2
# get unique group rows from dt all
setkey(dt.all, group_whoc)
dt.output <- dt.all[J(unique(group_whoc)), mult = "first"]
dt.output[ , TE := NA]
dt.output[ , seTE := NA]

n.1 <- n.1[ , c("group_whoc", "TE","seTE","n")]
dt.output <- merge(dt.output, n.1, by="group_whoc", all.x=TRUE, all.y=FALSE)
dt.output[ , TE.x := TE.y]
dt.output[ , seTE.x := seTE.y]
dt.output <- dt.output[ , -c("TE.y","seTE.y")]

## if n>1 then meta-analysis
n.studies.2 <- subset(n.studies, n>1)
t <- as.numeric(nrow(n.studies.2))
if (t>0){
n.2  <- merge(n.studies.2, dt.all, c("group_whoc"))
n.2  <- n.2[!is.na(TE)]
n.2[ , group_ID := .GRP, by =.(whoc.region, syndrome, class, gram.stain)]
n.2 <- unique(n.2, by=c("group_whoc","row_id"))

# output for preallocation
output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                          seT = rep(0,max(unique(n.2$group_ID))),
                          lowerT = rep(0,max(unique(n.2$group_ID))),
                          upperT = rep(0,max(unique(n.2$group_ID))),
                          group_ID= rep(0,max(unique(n.2$group_ID))),
                          group_whoc=rep(0,max(unique(n.2$group_ID))))

for (i in 1:max(unique(n.2$group_ID))){
  
  ## split into group wanted
  dt.temp <- n.2[group_ID == i]
  
  #### metagen() function when have SE value
  m.dl <- metagen(TE,
                  seTE,
                  data=dt.temp,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  hakn = FALSE,
                  prediction=TRUE,
                  sm="SMD")
  
  ## extracting out the variables
  output.meta[i,TE := m.dl$TE.random]
  output.meta[i,seT := m.dl$seTE.random]
  output.meta[i,lowerT := m.dl$lower.random]
  output.meta[i,upperT := m.dl$upper.random] 
  output.meta[i, group_ID := i]
  output.meta[i, group_whoc := min(dt.temp$group_whoc)]
  output.meta[i, n := m.dl$k]
  
}

output.meta <- output.meta[ ,c("group_whoc","TE","seT","n")]
dt.output <- merge(dt.output, output.meta, by="group_whoc", all.x=TRUE, all.y=FALSE)
dt.output[is.na(TE.x), TE.x := TE]
dt.output[is.na(seTE.x), seTE.x := seT]
dt.output[is.na(n.x), n.x:= n.y]
dt.output <- dt.output[ , -c("TE","seT","n.y")]

}

## see which have no estimates
dt.na.whoc <- dt.output[is.na(dt.output$TE.x)]

######## grouping by WB Income Classification ########
n.studies <- dt.all %>%
  filter(!is.na(Income.group)) %>% ## remove NA values otherwise grouped into own group
  as.data.table()
## have to break pipe to use data.table unique
n.studies <- unique(n.studies,by=c("group_income","row_id"))  ## removing duplicate studies from region mapping
n.studies <- n.studies %>% 
  group_by(group_income) %>% 
  count(!is.na(TE)) 

n.studies <- as.data.table(n.studies)
colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
n.studies[any== FALSE, n := 0]

## if n=1 just use that study
n.studies.1 <- subset(n.studies, n==1)
n.1 <- merge(n.studies.1, dt.all, by="group_income")
n.1 <- n.1[!is.na(TE)]
# get unique group rows from dt all
setkey(dt.all, group_income)
dt.output.income <- dt.all[J(unique(group_income)), mult = "first"]
dt.output.income[ , TE := NA]
dt.output.income[ , seTE := NA]

n.1 <- n.1[ , c("group_income", "TE","seTE","n")]
dt.output.income <- merge(dt.output.income, n.1, by="group_income", all.x=TRUE, all.y=FALSE)
dt.output.income[ , TE.x := TE.y]
dt.output.income[ , seTE.x := seTE.y]
dt.output.income <- dt.output.income[ , -c("TE.y","seTE.y")]

## if n>1 then meta-analysis
n.studies.2 <- subset(n.studies, n>1)
t <- as.numeric(nrow(n.studies.2))
if (t>0){
n.2  <- merge(n.studies.2, dt.all, by="group_income")
n.2 <- n.2[!is.na(TE)]
n.2[ , group_ID := .GRP, by =.(Income.group, syndrome, class, gram.stain)]
n.2 <- unique(n.2, by=c("group_income","row_id"))

# output for preallocation
output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                          seT = rep(0,max(unique(n.2$group_ID))),
                          lowerT = rep(0,max(unique(n.2$group_ID))),
                          upperT = rep(0,max(unique(n.2$group_ID))),
                          group_ID= rep(0,max(unique(n.2$group_ID))),
                          group_income =rep(0,max(unique(n.2$group_ID))))

for (i in 1:max(unique(n.2$group_ID))){
  
  ## split into group wanted
  dt.temp <- n.2[group_ID == i]
  
  #### metagen() function when have SE value
  m.dl <- metagen(TE,
                  seTE,
                  data=dt.temp,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  hakn = FALSE,
                  prediction=TRUE,
                  sm="SMD")
  
  ## extracting out the variables
  output.meta[i,TE := m.dl$TE.random]
  output.meta[i,seT := m.dl$seTE.random]
  output.meta[i,lowerT := m.dl$lower.random]
  output.meta[i,upperT := m.dl$upper.random] 
  output.meta[i, group_ID := i]
  output.meta[i, group_income := min(dt.temp$group_income)]
  output.meta[i, n := m.dl$k]
  
}

output.meta <- output.meta[ ,c("group_income","TE","seT","n")]
dt.output.income <- merge(dt.output.income, output.meta, by="group_income", all.x=TRUE, all.y=FALSE)
dt.output.income[is.na(TE.x), TE.x := TE]
dt.output.income[is.na(seTE.x), seTE.x := seT]
dt.output.income[is.na(n.x), n.x:= n.y]
dt.output.income <- dt.output.income[ , -c("TE","seT","n.y")]

}

dt.na.income <- dt.output.income[is.na(dt.output.income$TE.x)]

###### grouping by WB regional estimates ###################

n.studies <- dt.all %>%
  filter(!is.na(wb.region)) %>% ## remove NA values otherwise grouped into own group
  as.data.table()
## have to break pipe to use data.table unique
n.studies <- unique(n.studies,by=c("group_wbregion","row_id"))  ## removing duplicate studies from region mapping
n.studies <- n.studies %>% 
  group_by(group_wbregion) %>% 
  count(!is.na(TE)) 

n.studies <- as.data.table(n.studies)
colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
n.studies[any== FALSE, n := 0]

## if n=1 just use that study
n.studies.1 <- subset(n.studies, n==1)
n.1 <- merge(n.studies.1, dt.all, by="group_wbregion")
n.1 <- n.1[!is.na(TE)]
# get unique group rows from dt all
setkey(dt.all, group_wbregion)
dt.output.wbregion <- dt.all[J(unique(group_wbregion)), mult = "first"]
dt.output.wbregion[ , TE := NA]
dt.output.wbregion[ , seTE := NA]

n.1 <- n.1[ , c("group_wbregion", "TE","seTE","n")]
dt.output.wbregion <- merge(dt.output.wbregion, n.1, by="group_wbregion", all.x=TRUE, all.y=FALSE)
dt.output.wbregion[ , TE.x := TE.y]
dt.output.wbregion[ , seTE.x := seTE.y]
dt.output.wbregion <- dt.output.wbregion[ , -c("TE.y","seTE.y")]

## if n>1 then meta-analysis
n.studies.2 <- subset(n.studies, n>1)
t <- as.numeric(nrow(n.studies.2))
if (t>0){
  n.2  <- merge(n.studies.2, dt.all, by="group_wbregion")
  n.2 <- n.2[!is.na(TE)]
  n.2[ , group_ID := .GRP, by =.(wb.region, syndrome, class, gram.stain)]
  n.2 <- unique(n.2, by=c("group_wbregion","row_id"))
  
  # output for preallocation
  output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                            seT = rep(0,max(unique(n.2$group_ID))),
                            lowerT = rep(0,max(unique(n.2$group_ID))),
                            upperT = rep(0,max(unique(n.2$group_ID))),
                            group_ID= rep(0,max(unique(n.2$group_ID))),
                            group_wbregion =rep(0,max(unique(n.2$group_ID))))
  
  for (i in 1:max(unique(n.2$group_ID))){
    
    ## split into group wanted
    dt.temp <- n.2[group_ID == i]
    
    #### metagen() function when have SE value
    m.dl <- metagen(TE,
                    seTE,
                    data=dt.temp,
                    comb.fixed = FALSE,
                    comb.random = TRUE,
                    hakn = FALSE,
                    prediction=TRUE,
                    sm="SMD")
    
    ## extracting out the variables
    output.meta[i,TE := m.dl$TE.random]
    output.meta[i,seT := m.dl$seTE.random]
    output.meta[i,lowerT := m.dl$lower.random]
    output.meta[i,upperT := m.dl$upper.random] 
    output.meta[i, group_ID := i]
    output.meta[i, group_wbregion := min(dt.temp$group_wbregion)]
    output.meta[i, n := m.dl$k]
    
  }
  
  output.meta <- output.meta[ ,c("group_wbregion","TE","seT","n")]
  dt.output.wbregion <- merge(dt.output.wbregion, output.meta, by="group_wbregion", all.x=TRUE, all.y=FALSE)
  dt.output.wbregion[is.na(TE.x), TE.x := TE]
  dt.output.wbregion[is.na(seTE.x), seTE.x := seT]
  dt.output.wbregion[is.na(n.x), n.x:= n.y]
  dt.output.wbregion <- dt.output.wbregion[ , -c("TE","seT","n.y")]

}

dt.na.wbregion <- dt.output.wbregion[is.na(dt.output.wbregion$TE.x)]

###### grouping by global estimates ###################

## have to break pipe to use data.table unique
n.studies <- unique(dt.all,by=c("row_id"))  ## removing duplicate studies from region mapping
n.studies <- n.studies %>% 
  group_by(group_global) %>% 
  count(!is.na(TE)) 


n.studies <- as.data.table(n.studies)
colnames(n.studies)[colnames(n.studies) == '!is.na(TE)'] <- 'any'
n.studies[any== FALSE, n := 0]

## if n=1 just use that study
n.studies.1 <- subset(n.studies, n==1)
n.1 <- merge(n.studies.1, dt.all, by="group_global")
n.1 <- n.1[ , c("syndrome", "class", "gram.stain","TE","seTE", "group_whoc","group_income","group_global","n")]
n.1 <- n.1[!is.na(TE)]

## form final list through merging n.1 and n.2
# get unique group rows from dt all
setkey(dt.all, group_global)
dt.output.global <- dt.all[J(unique(group_global)), mult = "first"]
dt.output.global[ , TE := NA]
dt.output.global[ , seTE := NA]

n.1 <- n.1[ , c("group_global", "TE","seTE","n")]
dt.output.global <- merge(dt.output.global, n.1, by="group_global", all.x=TRUE, all.y=FALSE)
dt.output.global[ , TE.x := TE.y]
dt.output.global[ , seTE.x := seTE.y]
dt.output.global <- dt.output.global[ , -c("TE.y","seTE.y")]
  
## if n>1 then meta-analysis
n.studies.2 <- subset(n.studies, n>1)
t <- as.numeric(nrow(n.studies.2))
if (t>0){
n.2  <- merge(n.studies.2, dt.all, by="group_global")
n.2 <- n.2[!is.na(TE)]
n.2[ , group_ID := .GRP, by =.(syndrome, class, gram.stain)]
n.2 <- unique(n.2, by=c("row_id"))

# output for preallocation
output.meta <- data.table(TE = rep(0,max(unique(n.2$group_ID))),
                          seT = rep(0,max(unique(n.2$group_ID))),
                          lowerT = rep(0,max(unique(n.2$group_ID))),
                          upperT = rep(0,max(unique(n.2$group_ID))),
                          group_ID= rep(0,max(unique(n.2$group_ID))),
                          group_global =rep(0,max(unique(n.2$group_ID))))

for (i in 1:max(unique(n.2$group_ID))){
  
  ## split into group wanted
  dt.temp <- n.2[group_ID == i]
  
  #### metagen() function when have SE value
  m.dl <- metagen(TE,
                  seTE,
                  data=dt.temp,
                  comb.fixed = FALSE,
                  comb.random = TRUE,
                  hakn = FALSE,
                  prediction=TRUE,
                  sm="SMD")
  
  ## extracting out the variables
  output.meta[i,TE := m.dl$TE.random]
  output.meta[i,seT := m.dl$seTE.random]
  output.meta[i,lowerT := m.dl$lower.random]
  output.meta[i,upperT := m.dl$upper.random] 
  output.meta[i, group_ID := i]
  output.meta[i, group_global := min(dt.temp$group_global)]
  output.meta[i, n := m.dl$k]
  
}

output.meta <- output.meta[ ,c("group_global","TE","seT","n")]
dt.output.global <- merge(dt.output.global, output.meta, by="group_global", all.x=TRUE, all.y=FALSE)
dt.output.global[is.na(TE.x), TE.x := TE]
dt.output.global[is.na(seTE.x), seTE.x := seT]
dt.output.global[is.na(n.x), n.x:= n.y]
dt.output.global <- dt.output.global[ , -c("TE","seT","n.y")]

}

dt.na.global <- dt.output.global[is.na(dt.output.global$TE.x)]
dt.na.global <- dt.na.global[ , c("syndrome","class","gram.stain")]
# write.csv(dt.na.global, "cost_per_case/outputs/missing_global_combinations.csv")

### combining altogether
dt.output <- dt.output[ ,c("iso3c","whoc.region","syndrome","class","gram.stain",
                           "TE.x","seTE.x","group_whoc","n.x")]
setnames(dt.output, "TE.x", "TE.whoc")
setnames(dt.output, "seTE.x", "seTE.whoc")
setnames(dt.output, "n.x", "n.whoc")

dt.output.income <- dt.output.income[ ,c("Income.group","syndrome","class","gram.stain",
                                  "TE.x","seTE.x","group_income","n.x")]
setnames(dt.output.income, "TE.x", "TE.income")
setnames(dt.output.income, "seTE.x", "seTE.income")
setnames(dt.output.income, "n.x", "n.income")

dt.output.wbregion <- dt.output.wbregion[ ,c("wb.region","syndrome","class","gram.stain",
                                         "TE.x","seTE.x","group_wbregion","n.x")]
setnames(dt.output.wbregion, "TE.x", "TE.wbregion")
setnames(dt.output.wbregion, "seTE.x", "seTE.wbregion")
setnames(dt.output.wbregion, "n.x", "n.wbregion")


dt.output.global <- dt.output.global[ ,c("syndrome","class","gram.stain",
                                         "TE.x","seTE.x","group_global","n.x")]
setnames(dt.output.global, "TE.x", "TE.global")
setnames(dt.output.global, "seTE.x", "seTE.global")
setnames(dt.output.global, "n.x", "n.global")

## merge dt.all with outputs
# first remove the dt.all categories we don't want to map down to
# i.e. non-WHO-classified countries
dt.all.merge <- dt.all[!is.na(whoc.region)]

dt.output.country <- merge(dt.all.merge, dt.output, by=c("whoc.region",
                                                   "syndrome",
                                                   "class",
                                                   "gram.stain"))
### note that income.group.x and income.group.y are different
# because the one row chosen for the WHOC regional data (e.g. AGO) might be different
# to the one being merged onto to (e.g. AFG) but this will get removed anyway
# so ignore the ".y" columns for now

dt.output.all <- merge(dt.output.country, dt.output.income, by=c("Income.group",
                                                         "syndrome",
                                                         "class",
                                                         "gram.stain"))

dt.output.all2 <- merge(dt.output.all, dt.output.wbregion, by=c("wb.region",
                                                                "syndrome",
                                                              "class",
                                                              "gram.stain"))

dt.output.all3 <- merge(dt.output.all2, dt.output.global, by=c("syndrome",
                                                         "class",
                                                         "gram.stain"), 
                        allow.cartesian = TRUE)
## if whoc available
dt.output.all3[ , TE.final := TE.whoc]
dt.output.all3[TE.final==TE.whoc, level := "whoc"]
dt.output.all3[TE.final==TE.whoc, no.studies := n.whoc]

## if income available
dt.output.all3[is.na(TE.final), TE.final := TE.income ]
## have to add in extra is.na() in case all income are in one whoc region
dt.output.all3[TE.final==TE.income & is.na(TE.whoc), level := "income"]
dt.output.all3[TE.final==TE.income, no.studies := n.income ]

## if wb region available
dt.output.all3[is.na(TE.final), TE.final := TE.wbregion]
dt.output.all3[TE.final==TE.wbregion & is.na(TE.whoc)
               & is.na(TE.income), level := "wbregion"]
dt.output.all3[TE.final==TE.wbregion, no.studies := n.wbregion ]

## if global available
dt.output.all3[is.na(TE.final), TE.final := TE.global ]
dt.output.all3[TE.final==TE.global & is.na(TE.whoc)
               & is.na(TE.income) & is.na(TE.wbregion) , level := "global"]
dt.output.all3[TE.final==TE.global, no.studies := n.global ]

## setting the standard error accordingly
dt.output.all3[ , seTE.final := seTE.whoc] 
dt.output.all3[is.na(seTE.final), seTE.final := seTE.income ]
dt.output.all3[is.na(seTE.final), seTE.final := seTE.wbregion ]
dt.output.all3[is.na(seTE.final), seTE.final := seTE.global ]


## just ones we have global estimates for
## otherwise can use same syndrome + gram.stain - but will already
# have those estimates in the output file & would mean a lot more code
los.output.cc <- dt.output.all3[!is.na(TE.final)]
los.output.cc <- los.output.cc[ ,c("syndrome","class" , "gram.stain",  "iso3c.x",
                    "whoc.region"   , "Income.group"    ,  "wb.region"    , "TE.final"     ,  
                    "seTE.final" ,   "level"    ,      "no.studies")]

return(los.output.cc)
}


################******** META ANALYSIS LOS ********############
los.output.cc <- meta.grouping(los.TE)
write.csv(los.output.cc, file="cost_per_case/outputs/los_meta.csv")
save(los.output.cc, file="cost_per_case/outputs/los_output_cc.RData")

##############******** META ANALYSIS COSTING ***********##########

## replace TE and seTE with adjusted values so can use the same function
costing.TE.adj[ , TE := TE.adj]
costing.TE.adj[ , seTE := TE.adj]

costing.output.cc <- meta.grouping(costing.TE.adj)
write.csv(costing.output.cc, file="cost_per_case/outputs/costing_meta.csv")
save(costing.output.cc, file="cost_per_case/outputs/costing_output_cc.RData")

##############******** SAMPLING WHO CHOICE ***********##########
whoc.cc <- as.data.table(whoc.cc.2019)

## get the pre-grouped WHOC cost estimates by region
# whoc.cc <- whoc.cc[is.na(iso3c)]
# whoc.names <- c("AFRO D" , "AFRO E" , "EMRO E","EMRO D",  
#                 "EURO A"  ,"EURO B" ,"EURO C" , "AMRO A" ,
#                 "AMRO B"  ,"AMRO D",  "SEARO B" ,"SEARO D",
#                 "WPRO A" , "WPRO B" )
# whoc.cc <- whoc.cc[ region %in% whoc.names]
# whoc.cc[ , whoc.region := region] ## relabel the region to fit previous groups
# whoc.cc[ , ID := .GRP, by =.(whoc.region)] ## create numeric ID variable
## !!! note need to change some of the below as well if you want just region

## get the pre-grouped WHOC cost estimates by country
whoc.cc <- whoc.cc[!is.na(iso3c)]
whoc.cc[ , ID := .GRP, by =.(iso3c)] ## create numeric ID variable

as.numeric.factor <- function(x) {as.numeric(as.character(x))}

whoc.cc$mean_i <- as.numeric.factor(whoc.cc$mean_i) ##!!! make sure to use inflated costs "_i" 
whoc.cc$SD_i <- as.numeric.factor(whoc.cc$SD_i)

sample.whoc.cc <- list()
# data.table(matrix(NA, nrow=10000,ncol=length(whoc.cc$ID)))

set.seed(280)

### creating a list[[n]][x] where you have n regions and x draws 
# for the mean value
for (i in 1:max(unique(whoc.cc$ID))){
  
  ## split into group wanted
  dt.temp <- whoc.cc[ID == i]
  
  ### taken from https://devinincerti.com/2018/02/10/psa.html#gamma-and-lognormal-distributions
  ## credit to Devin Incerti, 2018 for the lnorm_mom function
  lnorm_mom <- function(mean, sd){
    if (mean > 0){
      sigma2 <- log((sd^2 + mean^2)/mean^2)
      mu <- log(mean) - 1/2 * sigma2
    } else{
      stop("Mean must be positive")
    }
    return(list(mu = mu, sigma2 = sigma2))
  }
  
  lnorm.pars <- lnorm_mom(dt.temp$mean_i, dt.temp$SD_i)
  lnorm.sample <- rlnorm(n.samples, meanlog = lnorm.pars$mu, sdlog = sqrt(lnorm.pars$sigma2))
  
  # ## extracting out the variables
  # sample.whoc.cc[ , i] <- lnorm.sample
  # colnames(sample.whoc.cc)[i] <- as.character(dt.temp$whoc.region)
  sample.whoc.cc[[i]]<- dt.temp$ID
  sample.whoc.cc[[i]][1:n.samples] <- lnorm.sample
  setTxtProgressBar(pb,i)  
}

## creat a thinner version of who.cc 
whoc.cc.thin <- whoc.cc[ , c("iso3c", "mean_i","ID")]
setnames(whoc.cc.thin, "iso3c", "iso3c.x") ## renaming column to match los.cc.thin
## create a list where you have iso3c, mean and id repeated x times
list.whoc.cc <- rep(list(whoc.cc.thin),n.samples)
# then replace each value with the draw values
## for J represents ID (so each region/country) and i represents the run
set.seed(280)
for (i in 1:n.samples){
  temp <- as.data.table(list.whoc.cc[[i]])
  for (j in 1:max(dt.temp$ID)){
    temp[ID==j, mean_i := sample.whoc.cc[[j]][i]]
  }
  list.whoc.cc[[i]] <- temp
}

save(list.whoc.cc, file="cost_per_case/outputs/list.whoc.cc.RData")

##############******** SAMPLING LOS ***********##########

los.cc <- as.data.table(los.output.cc)
los.cc[ , ID := c(1:nrow(los.cc))]

sample.los.cc <- list()
set.seed(280)
for (i in 1:max(unique(los.cc$ID))){
  
  ## split into group wanted
  dt.temp <- los.cc[ ID == i]
  ### 
  rnorm.sample <- rnorm(n=n.samples, mean=dt.temp$TE.final, sd=dt.temp$seTE.final)
  # ## extracting out the variables
  # sample.los.cc[ , i] <- rnorm.sample
  # colnames(sample.los.cc)[i] <- dt.temp$ID
  sample.los.cc[[i]]<- dt.temp$ID
  sample.los.cc[[i]][1:n.samples] <- rnorm.sample
  setTxtProgressBar(pb,i)  
}

# ### plot code for checks
# ggplot2::ggplot(data.frame(x = rnorm.sample), aes_string("x")) + 
#   geom_histogram(color = "white", fill = "dodgerblue3", binwidth = .4)


los.cc.thin <- los.cc[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain","TE.final","ID","no.studies","level")]
list.los.cc <- rep(list(los.cc.thin),n.samples)

## for J represents ID (so each country,drug, bug, syndrome group) and i represents the run

for (i in 1:n.samples){
  temp <- as.data.table(list.los.cc[[i]])
  for (j in 1:max(dt.temp$ID)){  
    temp[ID==j, TE.final := sample.los.cc[[j]][i]]
  }
  list.los.cc[[i]] <- temp
  setTxtProgressBar(pb,i)  
}

save(list.los.cc, file="cost_per_case/outputs/list.los.cc.RData")

##############******** ESTIMATING TOTAL COST ***********##########

## creating a list templat to fill
los.cost.temp <- merge(whoc.cc.thin,los.cc.thin, by="iso3c.x",allow.cartesian=TRUE)
los.cost.temp <- rep(list(los.cost.temp),n.samples)

### creating the multiplication
for (i in 1:n.samples){
  costs <- as.data.table(list.whoc.cc[[i]])
  los <- as.data.table(list.los.cc[[i]])
  los.cost <- merge(los, costs, by="iso3c.x",allow.cartesian = TRUE)
  los.cost[ , total.cost := mean_i*TE.final]
  los.cost.temp[[i]] <- los.cost
}

## test on i=1 the above & then to see what is missing...
# temp <- los.cost[,-c("mean_i","ID.y")]
# setnames(temp, "ID.x","ID")
# missing <- setdiff(los, temp)
# unique(missing$iso3c.x)
# ## "PRK" "SOM" "ZWE" removed as do not have WHO-CHOICE values

total.los.cost <- rbindlist(los.cost.temp)
total.los.cost <- as.data.table(total.los.cost)

##############******** SAMPLING EXTRACTED COSTS ***********##########

costing.cc <- as.data.table(costing.output.cc)
costing.cc[ , ID := c(1:nrow(costing.cc))]

sample.costing.cc <- list()
set.seed(280)
for (i in 1:max(unique(costing.cc$ID))){
  
  ## split into group wanted
  dt.temp.2 <- costing.cc[ID == i]
  ## sample
  rnorm.sample.2 <- rnorm(n=n.samples, mean=dt.temp.2$TE.final, sd=dt.temp.2$seTE.final)
  # ## extracting out the variables
  sample.costing.cc[[i]]<- dt.temp.2$ID
  sample.costing.cc[[i]][1:n.samples] <- rnorm.sample.2
  setTxtProgressBar(pb,i)  
  
}

costing.cc.thin <- costing.cc[ , c("iso3c.x","whoc.region", "syndrome","class","gram.stain","TE.final","ID","no.studies","level")]
list.costing.cc <- rep(list(costing.cc.thin),n.samples)

## for J represents ID (so each country,drug, bug, syndrome group) and i represents the run

for (i in 1:n.samples){
  temp.2 <- as.data.table(list.costing.cc[[i]])
  for (j in 1:max(dt.temp.2$ID)){
    temp.2[ID==j, TE.final := sample.costing.cc[[j]][i]]
  }
  list.costing.cc[[i]] <- temp.2
}


save(list.costing.cc, file="cost_per_case/outputs/list.costing.cc.RData")

total.costing <- rbindlist(list.costing.cc)
total.costing<- as.data.table(total.costing)

##############******** COMBINING TOTAL COST SAMPLE & LITERATURE COST ***********##########
## renaming columns from the direct cost meta and the los*whoc cost meta
setnames(total.costing, "TE.final", "extracted.cost")
setnames(total.costing, "no.studies", "extracted.cost.no.studies")
setnames(total.costing, "level", "extracted.cost.level")

setnames(total.los.cost,"total.cost","los.cost")
setnames(total.los.cost, "no.studies", "los.no.studies")
setnames(total.los.cost, "level", "los.level")

total.los.cost <- total.los.cost[ , -c("ID.x","ID.y","mean_i")]
total.costing <- total.costing[ , -c("ID")]

save(total.costing, file="cost_per_case/outputs/total_costing.RData")
save(total.los.cost, file="cost_per_case/outputs/total_los_cost.RData")
### saving samples directly in case needed & cleaning space
save(sample.costing.cc, file="cost_per_case/outputs/sample_costing_cc.RData")
save(sample.los.cc, file="cost_per_case/outputs/sample_los_cc.RData")
save(sample.whoc.cc, file="cost_per_case/outputs/sample_whoc_cc.RData")

# ### had to exit and reload for memory purposes
# load("cost_per_case/outputs/total_los_cost.RData")
# load("cost_per_case/outputs/total_costing.RData")

l = list(total.los.cost, total.costing)

##!! add list ID in here if poss
dt.final <- rbindlist(l, use.names=TRUE, fill=TRUE)

save(dt.final, file="cost_per_case/outputs/dtfinal.RData")

### creating averages for (1) LOS based cost, (2) direct based cost and (3) combination of both

## (1) los-based cost
mean.valuesL <- dt.final[,.(mean.cost_los = mean(los.cost, na.rm = TRUE)),
                              by=c("iso3c.x","syndrome", "class", "gram.stain")] 

lowL <- dt.final[,.(low.cost_los = quantile(los.cost,0.025, na.rm = TRUE)), 
                      by=c("iso3c.x","syndrome", "class", "gram.stain")] 

highL <- dt.final[,.(high.cost_los = quantile(los.cost,0.975, na.rm = TRUE)), 
                       by=c("iso3c.x","syndrome", "class", "gram.stain")] 

loscost.country <- merge(mean.valuesL, lowL, by = c("iso3c.x","syndrome", "class", "gram.stain"))

loscost.country <- merge(loscost.country, highL, by = c("iso3c.x","syndrome", "class", "gram.stain"))

## (2) extracted cost-based costs
mean.valuesC <- dt.final[,.(mean.cost_extractedC = mean(extracted.cost, na.rm = TRUE)),
                        by=c("iso3c.x","syndrome", "class", "gram.stain")] 

lowC <- dt.final[,.(low.cost_extractedC  = quantile(extracted.cost,0.025, na.rm = TRUE)), 
                by=c("iso3c.x","syndrome", "class", "gram.stain")] 

highC <- dt.final[,.(high.cost_extractedC  = quantile(extracted.cost,0.975, na.rm = TRUE)), 
                 by=c("iso3c.x","syndrome", "class", "gram.stain")] 

extractedcost.country <- merge(mean.valuesC, lowC, by = c("iso3c.x","syndrome", "class", "gram.stain"))

extractedcost.country <- merge(extractedcost.country, highC, by = c("iso3c.x","syndrome", "class", "gram.stain"))

## (3) both
### as we have 1 row for each result (either los or extracted cost)...
dt.final[ , cost := los.cost]
dt.final[is.na(cost), cost := extracted.cost]

mean.values <- dt.final[,.(mean.cost = mean(cost, na.rm = TRUE)),
                         by=c("iso3c.x","syndrome", "class", "gram.stain")]

low <- dt.final[,.(low.cost = quantile(cost,0.025, na.rm = TRUE)),
                 by=c("iso3c.x","syndrome", "class", "gram.stain")]

high <- dt.final[,.(high.cost = quantile(cost,0.975, na.rm = TRUE)),
                  by=c("iso3c.x","syndrome", "class", "gram.stain")]

cost.country <- merge(mean.values, low, by = c("iso3c.x","syndrome", "class", "gram.stain"))

cost.country <- merge(cost.country, high, by = c("iso3c.x","syndrome", "class", "gram.stain"))

## we want just 1 row for each country +combination (To get number of studies, level etc)
## getting just the los studies
los.temp <- dt.final[!is.na(los.cost)] %>%
  group_by(iso3c.x,syndrome,class, gram.stain) %>%
  filter(row_number() == 1)%>% ## take just 1 per COUNTRY combination
  select("iso3c.x", "syndrome", "class","gram.stain", "los.no.studies" ,"los.level") %>%
  as.data.table()

cost.temp <- dt.final[!is.na(extracted.cost)] %>%
  group_by(iso3c.x,syndrome,class, gram.stain) %>%
  filter(row_number() == 1)%>% ## take just 1 per COUNTRY combination
  select("iso3c.x", "syndrome", "class","gram.stain", "extracted.cost.no.studies" ,
         "extracted.cost.level") %>%
  as.data.table()

### merge all together 
results <- merge(los.temp,loscost.country, by = c("iso3c.x","syndrome", "class", "gram.stain"), all=TRUE)
results <- merge(results, cost.temp, by = c("iso3c.x","syndrome", "class", "gram.stain"), all=TRUE)
results <- merge(results, extractedcost.country, by = c("iso3c.x","syndrome", "class", "gram.stain"), all=TRUE)
results <- merge(results, cost.country, by = c("iso3c.x","syndrome", "class", "gram.stain"), all=TRUE)


## merge in with regions:
results <- merge(results, who_whoc_wb, by.x="iso3c.x", by.y="iso3c")

## reorder and rename columns for full excel
setcolorder(results, c("iso3c.x","whoc.region",
                            "wb.region"  ,"who.region",
                            "Income.group","gram.stain" ,
                            "class","syndrome", "los.level" ,
                            "los.no.studies", "mean.cost_los" ,"low.cost_los" ,
                            "high.cost_los"    ,
                            "extracted.cost.level","extracted.cost.no.studies",
                            "mean.cost_extractedC","low.cost_extractedC",
                            "high.cost_extractedC" ,
                            "mean.cost" ,"low.cost", "high.cost"))

save(results, file="cost_per_case/outputs/AMR_Results_Table_AMR_nolabels.RData")

results <- results %>% rename("Country (ISO3 Code)"="iso3c.x",
                                        "WHO-CHOICE Region"= "whoc.region",
                                        "World Bank Geographical Region"="wb.region",
                                        "World Health Region"="who.region",
                                        "World Bank Income Status"="Income.group",
                                        "Gram stain"="gram.stain",
                                        "Antibiotic Class"="class",
                                        "Syndrome"="syndrome", 
                                        "Regional Level of MA for LOS"="los.level",
                                       "Number of Studies in MA for LOS"="los.no.studies", 
                                       "Mean Cost - from Excess LOS"="mean.cost_los",
                                        "Low 95% UI Bound - from Excess LOS"="low.cost_los" ,
                                        "High 95% UI Bound - from Excess LOS"="high.cost_los",
                                        "Regional Level of MA for Extracted Costs"="extracted.cost.level",
                                        "Number of Studies in MA for Extracted Costs"="extracted.cost.no.studies",
                                       "Mean Cost - from Extracted Costs"="mean.cost_extractedC",
                                        "Low 95% UI Bound - from Exctraced Costs"="low.cost_extractedC",
                                        "High 95% UI Bound - from Extraced Costs"="high.cost_extractedC",
                                        "Mean Cost - Across Both"="mean.cost" ,
                                        "Low 95% UI Bound - Across Both"="low.cost", 
                                        "High 95% UI Bound - Across Both"="high.cost")                           

## rounding to the nearest numeric
results <- results %>%  mutate_if(is.numeric, round, digits=0)

save(results, file="cost_per_case/outputs/AMR_Results_Table.RData")
write.csv(results, file="cost_per_case/outputs/AMR_Results_Table.csv")


##### FORMATTING LOS ESTIMATES FOR RESULTS TABLES ######
load("cost_per_case/outputs/list.los.cc.RData")
total.los <- rbindlist(list.los.cc)

## (1) los estimates (in days)
mean.valuesLOS <- total.los[,.(mean_los = mean(TE.final, na.rm = TRUE)),
                         by=c("iso3c.x","syndrome", "class", "gram.stain")] 

lowLOS <- total.los[,.(low_los = quantile(TE.final,0.025, na.rm = TRUE)), 
                 by=c("iso3c.x","syndrome", "class", "gram.stain")] 

highLOS <- total.los[,.(high_los = quantile(TE.final,0.975, na.rm = TRUE)), 
                  by=c("iso3c.x","syndrome", "class", "gram.stain")] 

los.country <- merge(mean.valuesLOS, lowLOS, by = c("iso3c.x","syndrome", "class", "gram.stain"))

los.country <- merge(los.country, highLOS, by = c("iso3c.x","syndrome", "class", "gram.stain"))

los.country <- merge(los.country, total.los, by = c("iso3c.x","syndrome", "class", "gram.stain"))

los.country <- los.country %>% group_by(iso3c.x,syndrome,class, gram.stain) %>%
  filter(row_number() == 1)%>% ## take just 1 per country combination
  as.data.table()

## REMOVE NA VALUES
los.country <- los.country[!is.na(mean_los)] ## none dropped, but as a check 

los.country <- los.country[ , -c("TE.final","ID")]


setcolorder(los.country, c("iso3c.x","whoc.region",
                            "gram.stain" ,
                            "class","syndrome", "level" ,
                            "no.studies", "mean_los" ,"low_los" ,
                            "high_los"))
            
los.country <- los.country %>% rename("Country (ISO3 Code)"="iso3c.x",
                                       "WHO-CHOICE Region"= "whoc.region",
                                       "Gram stain"="gram.stain",
                                       "Antibiotic Class"="class",
                                       "Syndrome"="syndrome", 
                                       "Regional Level of MA for LOS"="level",
                                       "Number of Studies in MA for LOS"="no.studies", 
                                       "Mean Excess LOS"="mean_los",
                                       "Low 95% UI Bound Excess LOS"="low_los" ,
                                       "High 95% UI Bound Excess LOS"="high_los")       
save(los.country, file="cost_per_case/outputs/AMR_Results_LOS.RData")
write.csv(los.country, file="cost_per_case/outputs/AMR_Results_LOS.csv")
