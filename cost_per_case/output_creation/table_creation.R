#### TABLE CREATION ######

#### general data and packages ####
library(tidyverse)
library(data.table)
library(readxl)

load("data_all/who_whoc_wb.RData")

#### ALL OF THE RESULTS BY COUNTRY ######

load("cost_per_case/outputs/scenario2.results.RData")
load("cost_per_case/outputs/AMR_Results_Table.RData")
AMR <- results
load("cost_per_case/outputs/DRI_Results_Table.RData")
DRI <- results

AMR$AMR_or_DRI <- "AMR"
DRI$AMR_or_DRI <- "DRI"
results.long <- rbind(AMR,DRI)

### probably a more efficient way to do this but cleaning sc2 cols and
### then merging into results long is how being done for now:
sc2 <- sc2.results.long[ ,c("Country (ISO3 Code)" ,  
"Gram stain",
"Antibiotic Class", 
"Syndrome" ,
"adj.factor",                                 
"Scenario 2 Mean Cost",
"AMR_or_DRI" )]

results.all.gram <- merge(results.long, sc2, by= c("Country (ISO3 Code)" ,  
                             "Gram stain",
                             "Antibiotic Class", 
                             "Syndrome", "AMR_or_DRI" ), all.x=TRUE)

save(results.all.gram,file="cost_per_case/outputs/Results_Table_Gram_Country.RData")

######## BY BUG #################
bug_sydrome_matcher <- read_excel("cost_per_case/inputs/bug_sydrome_matcher.xlsx",
                                 sheet = "bug_sydrome_matcher")

### get definitions from the output data
antibiotics <- unique(results.all.gram$class)
antibiotics <- paste(antibiotics, collapse="|") ## collapse to pass through grepl()

## using WHO Priority resistance matches to filter bugs
## note if used Oxford handbook, would also need to potentially match 
## drug names to antibiotic classes to then match through
## update - now using matched by hand (based on those)
drug_matched <- bug_sydrome_matcher[grepl(antibiotics,bug_sydrome_matcher$to_match),]

## clean columns don't need
drug_matched <- drug_matched %>%
  select(-c("antibiotic_oxfordhandbook","OTHER",
            "antibiotic_WHO_PPL")) %>%
  as.data.table()
## melt bug level data
syndrome_matched <- melt(drug_matched, id.vars = c("to_match",
                                                   "bacteria",
                                                   "bacteria.code",
                                                   "gram.stain"),
                         measure.vars=c(   "BONE/JOINT" ,              
                                           "BSI", "GI",                       
                                           "INTRA-ABDOMINAL","URTI",                    
                                           "RTI (LRTI)","UTI",                      
                                           "SSI" ,"SSTI" ,                    
                                           "STI","COL/INF"))
syndrome_matched <- syndrome_matched[complete.cases(syndrome_matched),] ## remove NA rows

## using syndromes 
combo_results <- unique(results.all.gram[,c('Syndrome','Antibiotic Class','Gram stain')])

## matching spelling/labelling
syndrome_matched[variable == "BONE/JOINT" , variable := "B-J"]
syndrome_matched[variable == "INTRA.ABDOMINAL" , variable := "IAI"]
syndrome_matched[variable == "RTI (LRTI)" , variable := "RTI"]

## split multiple antibiotics for a particular bacteria
s <- strsplit(as.character(syndrome_matched$to_match), ',')
syndrome_matched <- data.frame(to_match=unlist(s), bacteria=rep(syndrome_matched$bacteria, sapply(s, FUN=length)),
                               bacteria.code=rep(syndrome_matched$bacteria.code, sapply(s, FUN=length)),
                               gram.stain=rep(syndrome_matched$gram.stain, sapply(s, FUN=length)),
                               syndrome=rep(syndrome_matched$variable, sapply(s, FUN=length)))
syndrome_matched$to_match <- trimws(syndrome_matched$to_match) ## remove leading whitespace
# rename to match
colnames(syndrome_matched)[1] <- "class"

### merging the two files
bug_matched <- merge(syndrome_matched, results.all.gram, by.x=c("syndrome",
                                                         "gram.stain",
                                                         "class"),
                                                        by.y=c("Syndrome",
                                                               "Gram stain",
                                                               "Antibiotic Class"),
                                                               all=FALSE)

bug_matched <-bug_matched[order(bug_matched$`AMR_or_DRI`),]

save(bug_matched, file="cost_per_case/outputs/Results_Table_Bug_Country.RData")
write.csv(bug_matched, file="cost_per_case/outputs/Results_Table_Bug_Country.csv")

###### BY REGION ###############

########### populations ###################
N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))

N <- N[Indicator.Code=="SP.POP.TOTL"] ## total population

### keep 2019 values
N <- N[ , c("Country.Code","X2019")]
setnames(N, "X2019", "npop")

### sample results table 
bug_matched <- as.data.table(bug_matched)

#### for LOS associated costs
bug_matched[ , cost.se.los := (`High 95% UI Bound - from Excess LOS`-
                                  `Low 95% UI Bound - from Excess LOS`)/3.92]

bug_matched[ , cost.te.los := `Mean Cost - from Excess LOS`]


#### for total cost (across types)
bug_matched[ , cost.se.both := (`High 95% UI Bound - Across Both`-
                             `Low 95% UI Bound - Across Both`)/3.92]

bug_matched[ , cost.te.both := `Mean Cost - Across Both`]

##### for Sceanrio 2 adjusted costs
bug_matched[ , cost.te.sc2 := `Scenario 2 Mean Cost`]

##### sampling
bug_matched[ , Country.Code := `Country (ISO3 Code)`]

who <- who_whoc_wb[ , c("iso3c","who.region")]

combo <- merge(bug_matched, N, by="Country.Code")
combo <- merge(combo, who, by.x="Country.Code", by.y="iso3c" )

costing.cc <- as.data.table(combo)
costing.cc <- costing.cc[!is.na(npop)]

#### breaking down by country for sampling ######
costing.cc[ , ID := c(1:nrow(costing.cc))]

n.samples <- 2

sample.costing.cc <- list()

for (i in 1:max(unique(costing.cc$ID))){
  
  ## go by each id
  dt.temp <- costing.cc[ID == i]
  
  ## sample
  sample.los <- rnorm(n=n.samples, mean=dt.temp$cost.te.los, sd=dt.temp$cost.se.los)
  sample.both <- rnorm(n=n.samples, mean=dt.temp$cost.te.both, sd=dt.temp$cost.se.both)
  
  # ## extracting out the variables
  sample.costing.cc[[i]] <- dt.temp$ID
  sample.costing.cc[[i]]$costing.los <- sample.los
  sample.costing.cc[[i]]$costing.both <- sample.both
  sample.costing.cc[[i]]$costing.sc2  <- dt.temp$cost.te.sc2
  names(sample.costing.cc)[[i]][1] <- "ID"
  sample.costing.cc[[i]]$npop <- dt.temp$npop
  sample.costing.cc[[i]]$who.region <- dt.temp$who.region
  sample.costing.cc[[i]]$Country.Code <- dt.temp$Country.Code                            
  sample.costing.cc[[i]]$syndrome <- dt.temp$syndrome                                
  sample.costing.cc[[i]]$gram.stain <- dt.temp$gram.stain                                
  sample.costing.cc[[i]]$class <-  dt.temp$class                                     
  sample.costing.cc[[i]]$bacteria <- dt.temp$bacteria                                  
  sample.costing.cc[[i]]$AMR_or_DRI  <- dt.temp$AMR_or_DRI                             

  
}

costing.sample <- rbindlist(sample.costing.cc)

save(costing.sample, file="costing.sample.RData")

  ### filter and average
  costing.average <- costing.sample %>%
    filter(!is.na(npop)) %>%
    group_by(V1, who.region, syndrome, gram.stain, AMR_or_DRI, bacteria) %>% 
    summarise(weighted_costing.both = weighted.mean(costing.both, npop),
              weighted_costing.los = weighted.mean(costing.los,npop),
              weighted_sc2 = weighted.mean(costing.sc2, npop))

  ### !!! need to finish this code to get lower and upper bounds for all the outcomes of interest
  ### AND Need to do copy this section at Gram-stain level for manuscript
  test <- costing.average %>%
    group_by(who.region, syndrome, gram.stain, AMR_or_DRI, bacteria) %>% 
    summarise(MED_weighted_costing.both = median(weighted_costing.both))
