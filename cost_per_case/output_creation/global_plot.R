library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)

##################### DRI #################################################
#### meta-analyses results don't have WHO region so need to extract that
load("data_all/who_whoc_wb.RData")
who <- who_whoc_wb[ , c("iso3c","who.region")]

#### LOADING POPULATION ESTIMATES
N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))
N <- N[Indicator.Code=="SP.POP.TOTL"]

### keep 2019 values
N <- N[ , c("Country.Code","X2019")]
setnames(N, "X2019", "npop")

load("cost_per_case/outputs/dtfinal_DRI.RData")
dt.final[ , cost := los.cost]
dt.final[is.na(cost), cost := extracted.cost]


all_samples <- merge(dt.final, N, by.x="iso3c.x", by.y="Country.Code")

rm(dt.final)
gc()

all_samples <- merge(all_samples, who, by.x="iso3c.x", by.y="iso3c" )

##create sample id variable
all_samples[, sample_id := seq_len(.N), by =c("iso3c.x","class","gram.stain","syndrome")]

all_samples <- na.omit(all_samples, cols="npop") 
all_samples[, weighted_TE:=weighted.mean(cost,npop), by=list(syndrome, class, gram.stain, sample_id)]

global_average <- all_samples[, .(mean_cost=mean(weighted_TE, na.rm = TRUE),
                                  low_cost=quantile(weighted_TE,0.025, na.rm = TRUE),
                                  high_cost = quantile(weighted_TE,0.975, na.rm = TRUE)),
                              by=list(syndrome, class, gram.stain)]


cost.output <- global_average

### load excel file of bugs and syndromes 
bug_sydrome_matcher <- read.csv("cost_per_case/inputs/bug_sydrome_matcher_unformatted.csv")


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
                                           "INTRA.ABDOMINAL","URTI",                    
                                           "RTI..LRTI.","UTI",                      
                                           "SSI" ,"SSTI" ,                    
                                           "STI"))
syndrome_matched <- syndrome_matched[complete.cases(syndrome_matched),] ## remove NA rows

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

bug_matched <- merge(syndrome_matched, cost.output, by=c("syndrome",
                                                         "gram.stain",
                                                         "class"), all=FALSE)

bacteria_inf <- unique(bug_matched$bacteria)
load("data_all/dic_bug.RData")
dic_bug <- dic_bug[, 1:3]
bacteria_inf <- subset(dic_bug, dic_bug$bacteria %in% bacteria_inf)

cost_inf <- subset(cost.output, cost.output$syndrome=="INF")

bacteria_cost_inf <- merge(bacteria_inf, cost_inf, by="gram.stain")

costing.output.region.inf <- rbind(bug_matched, bacteria_cost_inf)


df1 <- as.data.table(costing.output.region.inf)


## squashing exposure groups 
df1[bacteria=="Escherichia coli", bacteria := "E. coli"]
df1[bacteria=="Klebsiella pneumoniae", bacteria := "K. pneumo,"]
df1[bacteria=="Pseudomonas aeruginosa", bacteria := "P. aerugin,"]
df1[bacteria=="Acinetobacter baumannii", bacteria := "A. bauman,"]
df1[bacteria=="Enterococcus faecium", bacteria := "E. faecium"]
df1[bacteria=="Staphylococcus aureus", bacteria := "S. aureus"]
df1[bacteria=="Streptococcus pneumoniae", bacteria := "S. pneumo,"]

df1[class=="3g cephalosporins", class:="3GCs"]

df1[bacteria=="Mycobacterium tuberculosis", bacteria := "TB"]

## remove xdr as now don't have data

df2 <- subset(df1,class=="mdr")

df1 <- subset(df1,class!="xdr")
# df1 <- subset(df1,class!="mdr")

df1[ , Exposure := paste(class, bacteria)]

df1$Exposure_Group <- factor(df1$Exposure, levels =c( "3GCs E. coli",
                                                      "carbapenems E. coli",
                                                      "3GCs K. pneumo,",
                                                      "carbapenems K. pneumo,",
                                                      "3GCs P. aerugin," ,
                                                      "carbapenems P. aerugin,"  , 
                                                      "3GCs A. bauman," ,
                                                      "carbapenems A. bauman,"  , 
                                                      "penicillins S. aureus"  ,
                                                      "glycopeptides S. aureus"  ,    
                                                      "penicillins S. pneumo," ,
                                                      "penicillins E. faecium"    ,
                                                      "glycopeptides E. faecium"))

# ggplot(df1, aes(x=interaction(Exposure_Group), y=mean_cost, fill=syndrome)) + 
#   geom_bar(position=position_dodge(), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position=position_dodge(.9)) +
#   xlab("Resistance & Bacterial Exposure") +
#   ylab("Healthcare System Cost (2019 USD)") +
#   ggtitle("Global Averages") +
#   scale_fill_viridis_d()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   coord_cartesian(ylim=c(NA, 21000), expand = FALSE)


temp_slides_global2 <- df1 %>% complete(Exposure_Group, nesting(syndrome))
ggplot(temp_slides_global2, aes(x=interaction(Exposure_Group), y=mean_cost, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Hospital Cost per Case (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim=c(NA, 50000), expand = FALSE)

##################### AMR #################################################
#### meta-analyses results don't have WHO region so need to extract that
load("data_all/who_whoc_wb.RData")
who <- who_whoc_wb[ , c("iso3c","who.region")]

#### LOADING POPULATION ESTIMATES
N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))
N <- N[Indicator.Code=="SP.POP.TOTL"]

### keep 2019 values
N <- N[ , c("Country.Code","X2019")]
setnames(N, "X2019", "npop")

load("cost_per_case/outputs/dtfinal.RData")
dt.final[ , cost := los.cost]
dt.final[is.na(cost), cost := extracted.cost]


all_samples <- merge(dt.final, N, by.x="iso3c.x", by.y="Country.Code")

rm(dt.final)
gc()

all_samples <- merge(all_samples, who, by.x="iso3c.x", by.y="iso3c" )

##create sample id variable
all_samples[, sample_id := seq_len(.N), by =c("iso3c.x","class","gram.stain","syndrome")]

all_samples <- na.omit(all_samples, cols="npop") 
all_samples[, weighted_TE:=weighted.mean(cost,npop), by=list(syndrome, class, gram.stain, sample_id)]

global_average <- all_samples[, .(mean_cost=mean(weighted_TE, na.rm = TRUE),
                                  low_cost=quantile(weighted_TE,0.025, na.rm = TRUE),
                                  high_cost = quantile(weighted_TE,0.975, na.rm = TRUE)),
                              by=list(syndrome, class, gram.stain)]


cost.output <- global_average

### load excel file of bugs and syndromes 
bug_sydrome_matcher <- read.csv("cost_per_case/inputs/bug_sydrome_matcher_unformatted.csv")


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
                                           "INTRA.ABDOMINAL","URTI",                    
                                           "RTI..LRTI.","UTI",                      
                                           "SSI" ,"SSTI" ,                    
                                           "STI"))
syndrome_matched <- syndrome_matched[complete.cases(syndrome_matched),] ## remove NA rows

## matching spelling/labelling
syndrome_matched[variable == "BONE.JOINT" , variable := "B-J"]
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

bug_matched <- merge(syndrome_matched, cost.output, by=c("syndrome",
                                                         "gram.stain",
                                                         "class"), all=FALSE)

bacteria_inf <- unique(bug_matched$bacteria)
load("data_all/dic_bug.RData")
dic_bug <- dic_bug[, 1:3]
bacteria_inf <- subset(dic_bug, dic_bug$bacteria %in% bacteria_inf)

cost_inf <- subset(cost.output, cost.output$syndrome=="COL/INF")

bacteria_cost_inf <- merge(bacteria_inf, cost_inf, by="gram.stain")

costing.output.region.inf <- rbind(bug_matched, bacteria_cost_inf)


df1 <- as.data.table(costing.output.region.inf)


## squashing exposure groups 
df1[bacteria=="Escherichia coli", bacteria := "E. coli"]
df1[bacteria=="Klebsiella pneumoniae", bacteria := "K. pneumo,"]
df1[bacteria=="Pseudomonas aeruginosa", bacteria := "P. aerugin,"]
df1[bacteria=="Acinetobacter baumannii", bacteria := "A. bauman,"]
df1[bacteria=="Enterococcus faecium", bacteria := "E. faecium"]
df1[bacteria=="Staphylococcus aureus", bacteria := "S. aureus"]
df1[bacteria=="Streptococcus pneumoniae", bacteria := "S. pneumo,"]

df1[class=="3g cephalosporins", class:="3GCs"]

df1[bacteria=="Mycobacterium tuberculosis", bacteria := "TB"]

## remove xdr as now don't have data

df2 <- subset(df1,class=="mdr")

df1 <- subset(df1,class!="xdr")
# df1 <- subset(df1,class!="mdr")

df1[ , Exposure := paste(class, bacteria)]

df1$Exposure_Group <- factor(df1$Exposure, levels =c( "3GCs E. coli",
                                                      "carbapenems E. coli",
                                                      "3GCs K. pneumo,",
                                                      "carbapenems K. pneumo,",
                                                      "3GCs P. aerugin," ,
                                                      "carbapenems P. aerugin,"  , 
                                                      "3GCs A. bauman," ,
                                                      "carbapenems A. bauman,"  , 
                                                      "penicillins S. aureus"  ,
                                                      "glycopeptides S. aureus"  ,    
                                                      "penicillins S. pneumo," ,
                                                      "glycopeptides S. pneumo,",
                                                      "penicillins E. faecium"    ,
                                                      "glycopeptides E. faecium",
                                                      "mdr TB"))

# ggplot(df1, aes(x=interaction(Exposure_Group), y=mean_cost, fill=syndrome)) + 
#   geom_bar(position=position_dodge(), stat="identity",
#            colour="black", # Use black outlines,
#            size=.3) +      # Thinner lines
#   geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
#                 size=.3,    # Thinner lines
#                 width=.2,
#                 position=position_dodge(.9)) +
#   xlab("Resistance & Bacterial Exposure") +
#   ylab("Healthcare System Cost (2019 USD)") +
#   ggtitle("Global Averages") +
#   scale_fill_viridis_d()+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   coord_cartesian(ylim=c(NA, 21000), expand = FALSE)


temp_slides_global2 <- df1 %>% complete(Exposure_Group, nesting(syndrome))
ggplot(temp_slides_global2, aes(x=interaction(Exposure_Group), y=mean_cost, fill=syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=low_cost, ymax=high_cost),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Hospital Cost per Case (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim=c(NA, 50000), expand = FALSE)


#### running using global unit cost from combining_cost_cases_NEW
UNITcost_averted_global <- read.csv("outputs/unitcostresults_global_TEMP.csv")
UNITcost_averted_global <- as.data.table(UNITcost_averted_global)
UNITcost_averted_global[ ,Exposure_Group := paste(Antibiotic.class,Pathogen)]
UNITcost_averted_global <- UNITcost_averted_global %>% complete(Exposure_Group, 
                                                                nesting(Infectious.syndrome))
ggplot(UNITcost_averted_global, aes(x=interaction(Exposure_Group), y=AV_total_UNITcosting, 
                                    fill=Infectious.syndrome)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=LOW_total_UNITcosting, ymax=HIGH_total_UNITcosting),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("Resistance & Bacterial Exposure") +
  ylab("Hospital Cost per Case (2019 USD)") +
  ggtitle("Global Averages") +
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_cartesian(ylim=c(NA, 100000), expand = FALSE)
options(scipen=999) 
