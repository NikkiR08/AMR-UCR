################ MANIPULATING MSH COSTS ###################
library(fuzzyjoin)
library(tidyr)

## loading files and functions #####

load("data_all/who_whoc_wb.RData") ## who_whoc_wb groupings
source("general_functions/inflation.R") ## sourcing inflation code and data files

## reading in MSH download
cost.data <- read.csv("antibiotic/inputs/MSH_price_access.csv")

## reading in AWARE 2019 list
aware <- read.csv("antibiotic/inputs/WHO-AWARE-2019-simple.csv")

## merge using joining & account for spelling differences
names(cost.data)[names(cost.data) == 'Variable'] <- 'Antibiotic'

full.data <- stringdist_full_join(aware, cost.data, by ="Antibiotic",
                                  
                                  method = "jw", distance_col = "fuzzy.no",
                                  
                                  max_dist = 0.2)

## split into perfectly matched and non-perfectly matched
## the non-perfectly matched were searched by included antibiotic and a list of reasonable matches
# were made and coded to be kept
matched.data <- subset(full.data, fuzzy.no==0)
query.data <- subset(full.data, fuzzy.no!=0)

## make to csv file to hand check
write.csv(query.data, file ="antibiotic/outputs/MSH-AWARE-handcheck-PRE.csv")

## load post hand check file
## !!! note this means this file is not automatically updated if changes are made 
# in above code
query.checked <- read.csv("antibiotic/outputs/MSH-AWARE-handcheck-POST.csv")

## subset those checked as a match & get same colnames to match across data.frames
keep.checked <- query.checked %>% 
                        filter(hand.flag=="y" & Supplementary.Info!="dispersible" & 
                                 Supplementary.Info!="Dispersible") %>% ## remove dispersible from checked version 
                          select(-X,-hand.flag)

## removing empty columns to combine both matched and hand checked: 
matched.data <- matched.data %>%
  filter(Supplementary.Info!="dispersible" & 
           Supplementary.Info!="Dispersible") %>% ## remove from matched version
  select(-c(X,X.1,X.2,X.3,X.4,X.5,X.6,X.7,X.8))

## combine 
all.post <- rbind(matched.data, keep.checked)

## adapting and cleaning df for inflation process and final tables
all.post$Formulation <- paste(all.post$Dosage.Form, all.post$Strength)
all.post <- all.post %>% filter(Route.of.Admin=="PO"|
                                  Route.of.Admin=="INJ") %>% ## filter to oral and injectable routes
  rename( WHO_antibiotic = Antibiotic.x, 
                                MSH_antibiotic = Antibiotic.y ,
                                MSH_cost =  Supplier.Median..US..) %>%
                            filter(!is.na(MSH_cost)) %>%## remove those with no cost value
                        distinct(MSH_antibiotic, Strength, Dosage.Form,
                                 Route.of.Admin, Buyer.Median..US..,.keep_all = TRUE) %>%  ##remove duplicate matches of MSH (since that's what we're using as our antibiotic definitions)
                        select(WHO_antibiotic, Class, MSH_antibiotic, MSH_cost,
                        Formulation, Route.of.Admin, Category) %>%
                         as.data.table(all.post)
## inflate 
who_whoc_wb <- as.data.table(who_whoc_wb)
## need each country to have each antibiotic
who_whoc_wb_all <- who_whoc_wb[rep(who_whoc_wb[,.I],nrow(all.post))]
abx_all <- all.post[rep(all.post[,.I],nrow(who_whoc_wb))]

## order the who_whoc_wb_all by country
who_whoc_wb_all<- who_whoc_wb_all[order(iso3c)]
## combine to get the abx data for each country
abx_all <- cbind(who_whoc_wb_all,abx_all)

## getting the same columns to use in the inflation function
abx_all[ , cost_year := 2015] ## 2015 base year for MSH
abx_all[ , cost_currency := "USD"]
## get local currency units matched

costing.abx.currency <- merge(abx_all, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)

# converting costs
for (i in 1:nrow(costing.abx.currency)){
  costing.abx.currency[i, abx.adj := cost_adj_abx(2019,costing.abx.currency[i], 
                                                  "MSH_cost",inf_xch_4function)]
}

### !!! would be nice in any future iterations to see how often USA rather than 
### own country GDP differences were used in the calculations

## save
save(costing.abx.currency, file="antibiotic/outputs/costing_abx_MSH.RData")

