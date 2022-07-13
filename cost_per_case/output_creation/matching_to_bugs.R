############# bug level matching ################

library(dplyr)
library(data.table)
library(xlsx)

#########################***** REGIONAL *****########
#### loading regional costing data
load("Data/costing_region_AMR.RData")
load("Data/costing_region_DRI.RData")

costing.region.AMR$flag <- "AMR"
costing.region.DRI$flag <- "DRI"

## combine
cost.output <- rbind(costing.region.AMR,
                     costing.region.DRI)

### load excel file of bugs and syndromes 
bug_sydrome_matcher <- read.xlsx("Data/bug_sydrome_matcher.xlsx",
                                 sheetName = "bug_sydrome_matcher")


### get definitions from the output data
antibiotics <- unique(cost.output$class)
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
                         measure.vars=c(   "BONE.JOINT" ,              
                                           "BSI", "GI",                       
                                           "INTRA.ABDOMINAL","UPRTI",                    
                                           "RTI..LRTI.","UTI",                      
                                           "SSI" ,"SSTI" ,                    
                                           "STI"))
syndrome_matched <- syndrome_matched[complete.cases(syndrome_matched),] ## remove NA rows

## using syndromes 
combo_amr <- unique(costing.region.AMR[,c('syndrome','class','gram.stain')])
combo_amr$AMR <- "AMR Values Estimated"
combo_dri <- unique(costing.region.DRI[,c('syndrome','class','gram.stain')])
combo_dri$DRI <- "DRI Values Estimated"
combo <- merge(combo_amr, combo_dri, by=c('syndrome','class','gram.stain'), all=TRUE)

## matching spelling/labelling
syndrome_matched[variable == "BONE.JOINT" , variable := "joint"]
syndrome_matched[variable == "INTRA.ABDOMINAL" , variable := "IAI"]
syndrome_matched[variable == "RTI..LRTI." , variable := "RTI"]

## split multiple antibiotics for a particular bacteria
s <- strsplit(as.character(syndrome_matched$to_match), ',')
syndrome_matched <- data.frame(to_match=unlist(s), bacteria=rep(syndrome_matched$bacteria, sapply(s, FUN=length)),
                               bacteria.code=rep(syndrome_matched$bacteria.code, sapply(s, FUN=length)),
                               gram.stain=rep(syndrome_matched$gram.stain, sapply(s, FUN=length)),
                               syndrome=rep(syndrome_matched$variable, sapply(s, FUN=length)))
syndrome_matched$to_match <- trimws(syndrome_matched$to_match) ## remove leading whitespace
# rename to match
colnames(syndrome_matched)[1] <- "class"
## save
# save(syndrome_matched, file="Data/external_bug_syndrome_matched.RData")

### mergine the two files

bug_matched <- merge(syndrome_matched, cost.output, by=c("syndrome",
                                                         "gram.stain",
                                                         "class"), all=FALSE)

bug_matched <-bug_matched[order(bug_matched$flag),]
# write.csv(bug_matched, file="Data/bug_matched_cost_region.csv")


#### for INF
bacteria_inf <- unique(bug_matched$bacteria)
dic_bug <- read.csv("Data/bug_gram.csv")
dic_bug <- dic_bug[, 1:3]
bacteria_inf <- subset(dic_bug, dic_bug$bacteria %in% bacteria_inf)

cost_inf <- subset(cost.output, cost.output$syndrome=="INF")

bacteria_cost_inf <- merge(bacteria_inf, cost_inf, by="gram.stain")

costing.output.region.inf <- rbind(bug_matched, bacteria_cost_inf)
# write.csv(costing.output.region.inf, file="Data/costing.output.region.inf.csv")



### doing the same for LOS 
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

bug_matched_los <- merge(syndrome_matched, los.output, by=c("syndrome",
                                                            "gram.stain",
                                                            "class"), all=FALSE)

bug_matched_los <-bug_matched_los[order(bug_matched_los$flag),]

# write.csv(bug_matched_los, file="Data/bug_matched_los_region.csv")

#####***** COUNTRY LEVEL *******##################
load("Data/susceptible_los_results.RData")
load("Data/los_output_cc.RData")
load("Data/los_output_cc_DRI.RData")
load("Data/AMR_Results_Table_AMR_nolabels.RData")
costing.AMR <- results ## rename to avoid confusion
load("Data/AMR_Results_Table_DRI_nolabels.RData")
costing.DRI <- results.long

los.output.cc$flag <- "AMR"
los.output.cc.DRI$flag <- "DRI"
los.output.cc.S$flag <- "S"

los.output.country <- rbind(los.output.cc,los.output.cc.DRI)
los.output.country <- rbind(los.output.country, los.output.cc.S)

costing.AMR$flag <- "AMR"
costing.DRI$flag <- "DRI"

costing.output.country <- rbind(costing.AMR,costing.DRI)

## makine into function
bug_match <- function(x){
  y <- merge(syndrome_matched, x, by=c("syndrome", "gram.stain", "class"), all=FALSE)
  y <-y[order(y$flag),]
  return(y)
}

los.output.country <- bug_match(los.output.country)
# write.csv(los.output.country, file="Data/los.output.country.csv")

costing.output.country <- bug_match(costing.output.country)
# write.csv(costing.output.country, file="Data/costing.output.country.csv")

#### matching to other resistances 
