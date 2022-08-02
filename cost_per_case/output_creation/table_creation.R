#### TABLE CREATION ######
######## AND DESCRIPTIVE STAT CREATION FOR PAPER

#### !!! for next iteration - integrate with meta-analysis scripts
#### to use the original samples created rather than having to re-sample

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
setnames(results.long, "World Health Region", "World Health Organization Region")
### !!! might have an error if rerun everything as have tried to correct for this earlier on in code now

save(results.long, file="cost_per_case/outputs/results_AMRandDRI.RData")

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
load("cost_per_case/outputs/Results_Table_Bug_Country.RData")
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
### !!! for future could integrate sc2 adjustment into sampling in this script

##### sampling
bug_matched[ , Country.Code := `Country (ISO3 Code)`]

who <- who_whoc_wb[ , c("iso3c","who.region")]

combo <- merge(bug_matched, N, by="Country.Code")
combo <- merge(combo, who, by.x="Country.Code", by.y="iso3c" )

costing.cc <- as.data.table(combo)
costing.cc <- costing.cc[!is.na(npop)]

#### breaking down by country for sampling ######
costing.cc[ , ID := c(1:nrow(costing.cc))]

## no. of samples 
n.samples <- 1000

sample.costing.cc <- list()

for (i in 1:max(unique(costing.cc$ID))){
  
  ## go by each id
  dt.temp <- costing.cc[ID == i]
  
  ## sample
  set.seed(1988)
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

save(costing.sample, file="cost_per_case/outputs/costing.sample.RData")

  ### filter and average
  costing.average <- costing.sample %>%
    filter(!is.na(npop)) %>%
    group_by(V1, who.region, syndrome, gram.stain, AMR_or_DRI, bacteria, class) %>% 
    summarise(weighted_costing.both = weighted.mean(costing.both, npop),
              weighted_costing.los = weighted.mean(costing.los,npop),
              weighted_sc2 = weighted.mean(costing.sc2, npop))
  
  costing.table.region.bug <- costing.average %>%
    group_by(who.region, syndrome, gram.stain, AMR_or_DRI, bacteria, class) %>% 
    summarise(AV_weighted_costing.both = mean(weighted_costing.both, na.rm=TRUE),
              LOW_weighted_costing.both = quantile(weighted_costing.both,0.025, na.rm = TRUE),
              HIGH_weighted_costing.both = quantile(weighted_costing.both,0.975, na.rm = TRUE),
              AV_weighted_costing.los = mean(weighted_costing.los, na.rm=TRUE),
              LOW_weighted_costing.los = quantile(weighted_costing.los,0.025, na.rm = TRUE),
              HIGH_weighted_costing.los = quantile(weighted_costing.los,0.975, na.rm = TRUE),
              AV_weighted_costing.sc2 = mean(weighted_sc2, na.rm=TRUE))

  save(costing.table.region.bug, file="cost_per_case/outputs/costing.table.region.bug.RData")  
  
  ###### region and Gram-stain level results #####
  
  ### filter and average
  costing.average.G <- costing.sample %>%
    filter(!is.na(npop)) %>%
    group_by(V1, who.region, syndrome, gram.stain, class, AMR_or_DRI) %>% 
    summarise(weighted_costing.both = weighted.mean(costing.both, npop),
              weighted_costing.los = weighted.mean(costing.los,npop),
              weighted_sc2 = weighted.mean(costing.sc2, npop))
  
  costing.table.region.G <- costing.average %>%
    group_by(who.region, syndrome, gram.stain, class, AMR_or_DRI) %>% 
    summarise(AV_weighted_costing.both = mean(weighted_costing.both, na.rm=TRUE),
              LOW_weighted_costing.both = quantile(weighted_costing.both,0.025, na.rm = TRUE),
              HIGH_weighted_costing.both = quantile(weighted_costing.both,0.975, na.rm = TRUE),
              AV_weighted_costing.los = mean(weighted_costing.los, na.rm=TRUE),
              LOW_weighted_costing.los = quantile(weighted_costing.los,0.025, na.rm = TRUE),
              HIGH_weighted_costing.los = quantile(weighted_costing.los,0.975, na.rm = TRUE),
              AV_weighted_costing.sc2 = mean(weighted_sc2, na.rm=TRUE))

  ### !!! add code to fix column headers to match paper
    
  save(costing.table.region.G, file="cost_per_case/outputs/costing.table.region.G.RData")
  write.csv(costing.table.region.G, file="cost_per_case/outputs/manuscript_results_cost.csv")
 
  ####### REGIONAL LOS ESTIMATES ###########
  load("cost_per_case/outputs/DRI_Results_LOS.RData")
  DRI.l <- los.country
  
  load("cost_per_case/outputs/AMR_Results_LOS.RData")
  AMR.l <- los.country
  
  AMR.l$AMR_or_DRI <- "AMR"
  DRI.l$AMR_or_DRI <- "DRI"
  results.long.los <- rbind(AMR.l,DRI.l)
  
  ## merge in WHO region 
  N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))
  
  N <- N[Indicator.Code=="SP.POP.TOTL"] ## total population
  
  ### keep 2019 values
  N <- N[ , c("Country.Code","X2019")]
  setnames(N, "X2019", "npop")

  #### for LOS 
  results.long.los[ , se.los := (`High 95% UI Bound Excess LOS`-
                                   `Low 95% UI Bound Excess LOS`)/3.92]
  
  results.long.los[ , te.los := `Mean Excess LOS`]
  
 ##### sampling
  results.long.los[ , Country.Code := `Country (ISO3 Code)`]
  
  who <- who_whoc_wb[ , c("iso3c","who.region")]
  
  combo <- merge(  results.long.los, N, by="Country.Code")
  combo <- merge(combo, who, by.x="Country.Code", by.y="iso3c" )
  
  costing.cc <- as.data.table(combo)
  costing.cc <- costing.cc[!is.na(npop)]
  
  #### breaking down by country for sampling ######
  costing.cc[ , ID := c(1:nrow(costing.cc))]
  
  n.samples <- 1000
  
  sample.costing.cc <- list()
  
  for (i in 1:max(unique(costing.cc$ID))){
    
    ## go by each id
    dt.temp <- costing.cc[ID == i]
    
    ## sample
    set.seed(1988)
    sample.los <- rnorm(n=n.samples, mean=dt.temp$te.los, sd=dt.temp$se.los)

    # ## extracting out the variables
    sample.costing.cc[[i]] <- dt.temp$ID
    sample.costing.cc[[i]]$los <- sample.los
    names(sample.costing.cc)[[i]][1] <- "ID"
    sample.costing.cc[[i]]$npop <- dt.temp$npop
    sample.costing.cc[[i]]$who.region <- dt.temp$who.region
    sample.costing.cc[[i]]$Country.Code <- dt.temp$Country.Code                            
    sample.costing.cc[[i]]$syndrome <- dt.temp$Syndrome                                
    sample.costing.cc[[i]]$gram.stain <- dt.temp$`Gram stain`                                
    sample.costing.cc[[i]]$class <-  dt.temp$`Antibiotic Class`                                     
    sample.costing.cc[[i]]$AMR_or_DRI  <- dt.temp$AMR_or_DRI                             
  }
  
  
  costing.sample <- rbindlist(sample.costing.cc)
  
  costing.average <- costing.sample %>%
    filter(!is.na(npop)) %>%
    group_by(V1, who.region, syndrome, gram.stain, AMR_or_DRI, class) %>% 
    summarise(weighted_los = weighted.mean(los, npop))
  
  los.region <- costing.average %>%
    group_by(who.region, syndrome, gram.stain, AMR_or_DRI, class) %>% 
    summarise(`Mean Excess LOS` = mean(weighted_los, na.rm=TRUE),
              `Low 95% UI Excess LOS` = quantile(weighted_los,0.025, na.rm = TRUE),
              `High 95% UI Excess LOS`= quantile(weighted_los,0.975, na.rm = TRUE))
  
  ### !!! add code to fix column headers to match paper
  
  save(los.region, file="cost_per_case/outputs/los.region.RData")
  write.csv(los.region, file="cost_per_case/outputs/manuscript_results_los.csv")
  
  ######## DESCRIPTIVE STATS #######
  
  #### INPUTS

  all <- read.csv("cost_per_case/inputs/lit_input_all.csv")
  
  length(which(all$los==1))
  
  lit_amr <- as.data.table(subset(all, all$review_marker=="AMR"))
  lit_dri <- as.data.table(subset(all, all$review_marker=="DRI"))
  
  length(unique(lit_amr$retrieval))
  length(unique(lit_dri$retrieval))
  
  ## make lower case 
  lit_amr[ , retrieval := tolower(retrieval)]
  lit_dri[ ,  retrieval := tolower(retrieval)]
  
  ## where from
  sum(grepl("cassini",lit_amr$retrieval))
  sum(grepl("cassini",lit_dri$retrieval))
  
  sum(grepl("founou",lit_amr$retrieval))
  sum(grepl("founou",lit_dri$retrieval)) 
  
  sum(grepl("zhen",lit_amr$retrieval)) 
  sum(grepl("zhen",lit_dri$retrieval))
  
  sum(grepl("wozniak",lit_amr$retrieval)) 
  sum(grepl("wozniak",lit_dri$retrieval))
  
  sum(grepl("naylor",lit_amr$retrieval)) 
  sum(grepl("naylor",lit_dri$retrieval))
  
  ## check adds
  22+4+37+2+12==nrow(lit_dri)
  113+11+72+16+35==nrow(lit_amr)
  
  length(unique(all$retrieval))
  
  ## didn't include colonisation
  all <- as.data.table(all)
  all.des <- all[syndrome!="COL"]
  
  length(unique(all.des$retrieval))
  length(which(all.des$review_marker=="AMR"))
  length(which(all.des$review_marker=="DRI"))
  
  length(which(all.des$los==1))
  length(which(all.des$los==0))
  
  
  ### final combinations of syndromes + Gram-stains
  load("cost_per_case/outputs/AMR_Results_Table_AMR_nolabels.RData")
   AMR <- results
   load("cost_per_case/outputs/AMR_Results_Table_DRI_nolabels.RData")
   DRI <- results
   rm(results)
   
  combo_amr <- unique(AMR[,c('syndrome','class','gram.stain')])
  combo_amr$AMR <- "AMR Values Estimated"
  combo_dri <- unique(DRI[,c('syndrome','class','gram.stain')])
  combo_dri$DRI <- "DRI Values Estimated"
  combo <- merge(combo_amr, combo_dri, by=c('syndrome','class','gram.stain'), all=TRUE)
  
  ## relabel things for manuscript
  combo[syndrome=="B-J", syndrome:="Bone & Joint"]
  combo[syndrome=="BSI", syndrome :="Blood Stream"]
  combo[syndrome=="COL/INF", syndrome :="Colonization and Infection OR Infection"]
  combo[syndrome=="IAI", syndrome :="Intra-abdominal"]
  combo[syndrome=="RTI", syndrome :="Respiratory Tract"]
  combo[syndrome=="SSI", syndrome :="Surgical Site"] 
  combo[syndrome=="SSTI", syndrome :="Skin and Soft Tissue"] 
  combo[syndrome=="UTI", syndrome :="Urinary Tract Infection"] 
  
  combo[gram.stain=="gp", gram.stain := "Gram-positive"]
  combo[gram.stain=="gn", gram.stain := "Gram-negative"]
  combo[gram.stain=="tb", gram.stain := "Tuberculosis"]
  
  write.csv(combo, file="cost_per_case/outputs/combo-syndromegroups.csv")

  
  
  #### level OF EVIDENCE ####
  
  load("cost_per_case/outputs/results_AMRandDRI.RData")
  datall <- results.long
  ## adding labels to match previous code
  datall$los.level <- results.long$`Regional Level of MA for LOS`
  datall$extracted.cost.level <- results.long$`Regional Level of MA for Extracted Costs`
  datall$los.no.studies <- results.long$`Number of Studies in MA for LOS`
  datall$extracted.cost.no.studies <- results.long$`Number of Studies in MA for Extracted Costs`
  
  datall$los.level.N <- 0
  datall[los.level=="global",los.level.N := 1]
  datall[los.level=="wbregion" ,los.level.N := 2]
  datall[los.level=="income" ,los.level.N := 3]
  datall[los.level=="whoc",los.level.N  := 4]
  
  datall$cost.level.N <- 0
  datall[extracted.cost.level=="global",cost.level.N := 1]
  datall[extracted.cost.level=="wbregion" ,cost.level.N := 2]
  datall[extracted.cost.level=="income" ,cost.level.N := 3]
  datall[extracted.cost.level=="whoc",cost.level.N  := 4]
  
  ## replace NA number of studies so that summation doesn't produce NA later on
  datall[is.na(los.no.studies), los.no.studies := 0]
  datall[is.na(extracted.cost.no.studies), extracted.cost.no.studies := 0]
  
  datall[ , evidence := (los.no.studies*los.level.N)+(extracted.cost.no.studies*
                                                        cost.level.N)]
  
  save(datall, file="cost_per_case/outputs/datall_forlevel.RData")
 
  ############## Table where has number of studies  ######
  
  load("cost_per_case/outputs/los_est_AMR.RData")
  los.AMR <- los.est
  load("cost_per_case/outputs/costing_est_AMR.RData")
  costing.AMR <- costing.est
  load("cost_per_case/outputs/los_est_DRI.RData")
  los.DRI <- los.est
  load("cost_per_case/outputs/costing_est_DRI.RData")
  costing.DRI <- costing.est
  
  los.AMR$type_input <- "los.AMR"
  los.DRI$type_input <- "los.DRI"
  costing.AMR$type_input <- "costing.AMR"
  costing.DRI$type_input <- "costing.DRI"
  
  temp <- list(los.AMR,los.DRI,costing.AMR,costing.DRI)
  inputs <- rbindlist(temp)
  rm(temp)
  
  mytable <- xtabs(~syndrome+class+type_input, data=inputs)
  evid <- as.data.table(mytable)
  evid <- dcast(evid,syndrome+class~type_input,value.var="N")
  evid <- as.data.table(evid)
  evid <- evid[ ,flag := costing.AMR+costing.DRI+los.AMR+los.DRI]
  evid <- evid[flag!=0]
  
  #### note I then added Joint to Bone and Joint
  write.csv(evid, file="cost_per_case/outputs/inputs_evidence_summary.csv")
  
  
  ######## REGIONAL STATS ######
  load("cost_per_case/outputs/costing.table.region.G.RData")
  
  outputs <- as.data.table(costing.table.region.G)
  outputs[LOW_weighted_costing.los<=0 &
            HIGH_weighted_costing.los<=0 , los.sig.flag:="SIG"]
  outputs[LOW_weighted_costing.los>=0 &
            HIGH_weighted_costing.los>=0 , los.sig.flag:="SIG"]
  outputs[LOW_weighted_costing.los<0 &
            HIGH_weighted_costing.los>0 , los.sig.flag := "NONSIG" ]
  
  outputs.los.sig <- outputs[los.sig.flag=="SIG"]
  
  outputs.los.sig.gram <- outputs.los.sig[gram.stain!="tb"]
  paper.gram <- outputs.los.sig.gram %>% 
    group_by(who.region, AMR_or_DRI) %>%
    filter(AV_weighted_costing.los == max(AV_weighted_costing.los))
  
  paper.gram <- paper.gram %>% 
    mutate_if(is.numeric, round) %>%
    mutate_if(is.numeric,funs(prettyNum(., big.mark=",")))
  
  paper.gram$result <- paste0(paper.gram$AV_weighted_costing.los, " (", paper.gram$LOW_weighted_costing.both,
                              " - ",paper.gram$HIGH_weighted_costing.both, ")")
  
  write.csv(paper.gram, file="cost_per_case/outputs/topcosts.csv")
  
  
  t3 <- outputs.los.sig.gram  %>%                                      # Top N highest values by group
    arrange(desc(AV_weighted_costing.los)) %>% 
    group_by(who.region, AMR_or_DRI) %>%
    slice(1:3)