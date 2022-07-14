################## MAPPING DRI DATA
############## PACKAGES #################

library(stringr)
library(dplyr)
library(data.table)
library(countrycode)
library(wbstats)
library(openxlsx)

############ READ IN FILES ##############

lit <- read.csv("cost_per_case/inputs/lit_input_all.csv")  ## literature review DET

## REMOVE JUST COLONISED FOR NOW
lit <- subset(lit, lit$syndrome!="COL")

## filter by DRI
lit <- subset(lit, review_marker=="DRI")

lit$row_id <- c(1:nrow(lit))

## WHO and WHOC regions hand copied from WHO website
dic_who <- read.csv("data_all/who_region.csv")
dic_whoc <- read.csv("data_all/who_choice_region.csv")

### World bank classifications from WB stats
dic_wb <- read.csv("data_all/wb_region_income.csv")

### Drug and Bug mapping (Note only for our exposures of interest)
### !!! would need to be expanded out if updated with other bacteria and drugs of interest
dic_bug <- read.csv("data_all/bug_gram.csv") ## bacteria dictionary
dic_drug <- read.csv("data_all/abx_class.csv") ## antibiotic dictionary

#######**** MAPPING REGION ****#####
# ## remove na rows from wb dictionary csv
# dic_wb <- subset(dic_wb, Economy != "")
# 
# ## relabel column headers to be more specific
# colnames(dic_wb)[colnames(dic_wb) == 'Region'] <- 'wb.region'
# colnames(dic_who)[colnames(dic_who) == 'Region.Name'] <- 'who.region'
# colnames(dic_whoc)[colnames(dic_whoc) == 'Subgroup'] <- 'whoc.region'
# 
# ## using country code package to standardise merging variable
# dic_wb$iso3c <- countrycode(dic_wb$Economy, origin="country.name", destination="iso3c")
# dic_who$iso3c <-countrycode(dic_who$Country, origin="country.name", destination="iso3c")
# dic_whoc$iso3c <-countrycode(dic_whoc$Country, origin="country.name", destination="iso3c")
# 
# ## remove columns not needed
# dic_wb <- dic_wb[ , c("iso3c","wb.region","Income.group")]
# dic_who <- dic_who[ , c("iso3c","who.region")]
# dic_whoc <- dic_whoc[ , c("iso3c","whoc.region")]
# 
# ## remove ones with no match
# dic_wb <- subset(dic_wb, !is.na(iso3c)) ##  currently Channel Islands, Kosovo (from WB))
# dic_who <- subset(dic_who, !is.na(iso3c))
# dic_whoc <- subset(dic_whoc, !is.na(iso3c)) ## currently Yugoslavia (from WHOchoice)
# 
# ## merge with who grouping
# who_whoc <- merge(dic_who, dic_whoc, by.x="iso3c", by.y="iso3c",all=TRUE)
# who_whoc_wb <- merge(who_whoc, dic_wb, by.x="iso3c", by.y="iso3c",all=TRUE)
# 
# ## check unmatched countries
# ## drop ones not in WHO list of countries (as requested)
# who_whoc_wb <- subset(who_whoc_wb, !is.na(who.region))
# 
# no_match <- subset(who_whoc_wb, is.na(who.region)| is.na(whoc.region) | is.na(wb.region))
# no_match$country <- countrycode(no_match$iso3c, origin="iso3c", destination="country.name")
# # ## drop for now but !!! otherwise can be added by hand
# who_whoc_wb <- subset(who_whoc_wb, !is.na(who.region) & !is.na(whoc.region) & !is.na(wb.region))
# # ## output linked csv
# write.csv(who_whoc_wb, file = "data_all/who_whoc_wb.csv")
# save(who_whoc_wb, file = "data_all/who_whoc_wb.RData")
load("data_all/who_whoc_wb.RData")

#######**** MAPPING BACTERIA ****####
# combine them 
bug <- merge(lit, dic_bug, by.x="bacteria.code", by.y="bacteria.code")
### !!! you can check here whether anything from the literature
# gets dropped by adding "all.x=TRUE" and seeing if difference in nrows(bug)

## remove duplicates where e.g. efa coded multiple times
## remove bacteria
bug <- as.data.table(bug)
bug[bacteria=="Enterococcus faecalis and faecium", bacteria:="Enterococcus"]
bug[bacteria=="Enterococcus faecium", bacteria:="Enterococcus"]
bug <- unique(bug)

## change formatting to all lower-case
bug <- bug  %>% 
  mutate_at(vars(c("exposed.R")), ~ str_to_lower(.))

#######**** MAPPING ANTIBIOTICS ****####
## make all lower case
dic_drug <- dic_drug %>% 
  mutate_at(vars(c("Drug.protein","Linked.antibiotic")), ~ str_to_lower(.))

## group ones that already are in our class and ones that need to be grouped
## take the ones we want from the drug dictionary
classy <- unique(dic_drug$Linked.antibiotic)
classy <- paste(classy,collapse="|")

table.input <- as.data.table(bug) ## make bug data.table
table.input[ , chf:= 0L] ## class flag (whether already class not abx)
table.input[grepl(classy,table.input$exposed.R), chf := 1]

### split into those by class and those not
class.y <- subset(table.input, chf==1)
class.n <- subset(table.input, chf==0)

## match the ones currently not at a class level
class.nm <- merge(class.n, dic_drug, by.x="exposed.R", by.y="Drug.protein",all=FALSE)
## !!! can check here by changing all.x=TRUE to see if any are being dropped because they don't match
## note e.g. penicillins has to be plural/match exactly or will be dropped here

## rename columns to match (adding an extra column in class.y so they match in terms of width)
class.y$class <- class.y$exposed.R
colnames(class.nm)[colnames(class.nm)=="Linked.antibiotic"] <- "class"

## combine the newly matched dataset with the one which was already defined by class
bug_class <- union(class.y, class.nm)
### !!! can check additional numbers (likely from proteins --> multiple antibiotics)
# gn <- subset(bug_class, gram.stain=="gn")
# unique(gn$class) ## do the same for gram poisitive
# ## checking ones not matched 
# test <- bug_class[(gram.stain=="gp" & (class=="penicillins"|class=="glycopeptides"))|
#                          (gram.stain=="gn" & (class=="3g cephalosporins"|class=="carbapenems"))|
#                     gram.stain=="tb"]
# explorar <- merge(test,lit, by="row_id",all.y=TRUE)
# subset(explorar, is.na(bacteria.code.x))

## removing combinations not interested in/extracted for (e.g. penicillin resistance for GN mapped from ESBL)
bug_class <- bug_class[(gram.stain=="gp" & (class=="penicillins"|class=="glycopeptides"))|
                         (gram.stain=="gn" & (class=="3g cephalosporins"|class=="carbapenems"))|
                    gram.stain=="tb"]

# # ## save dictionaries for bugs and drugs as RData
# save(dic_bug, file="data_all/dic_bug.RData")
# save(dic_drug, file="data_all/dic_drug.RData")

#########**** COMBINING ALL OF THE ABOVE ****#####
## convert input country to iso3c code using country code package
bug_class$iso3c <-countrycode(bug_class$country, origin="country.name", destination="iso3c") 

bug_class[country=="England",iso3c := "GBR"] ## otherwise unmatches
bug_class_region <- merge(bug_class, who_whoc_wb, by.x="iso3c", by.y="iso3c",all.x=TRUE)

## !!! currently it's just Europe & England not matched on this data
##  but would need to update this bit of code if also had e.g. 
## "Middle East & North Africa" or another region results extracted had similar issues

EuSA <- bug_class_region[country=="Europe"]
EuSA[ , wb.region := "Europe & Central Asia"]
EuSA <- merge(EuSA, who_whoc_wb, by="wb.region", allow.cartesian = TRUE)
EuSA <- EuSA %>%
  select(!ends_with(".x")) %>% ## removing unneeded columns
  group_by(retrieval, microbe, exposed.R, avexp, who.region.y,whoc.region.y,Income.group.y) %>%
  filter(row_number() == 1)%>% ## take just 1 per study + who.region/region combination
  as.data.table()
EuSA[ , iso3c.y := "EUSA"] ## put country code to NA so don't mistake evidence maps later
colnames(EuSA)[colnames(EuSA) == 'who.region.y'] <- 'who.region' ## match column names to data.table with main results
colnames(EuSA)[colnames(EuSA) == 'whoc.region.y'] <- 'whoc.region'
colnames(EuSA)[colnames(EuSA) == 'Income.group.y'] <- 'Income.group'
colnames(EuSA)[colnames(EuSA) == 'iso3c.y'] <- 'iso3c'

## remove "Low income" group as only 1/51 so might skew low income group results if all Europe results mapped to low income countries
EuSA <- EuSA[Income.group!="Low income"]
EuSA <- EuSA[Income.group!="Lower middle income"]
## set whoc regions to NA as not specifically targetting an individual WHOC region (e.g. EURO B) but rather the whole of Europe
EuSA <- EuSA[ , whoc.region := NA]

## deduplicate (!!! next iteration try to figure out why duplicates earlier)
EuSA <- unique(EuSA)

# save(EuSA, file="cost_per_case/outputs/eusa.RData")

bug_class_region <- bug_class_region[!is.na(iso3c)]

bug_class_region <- bug_class_region %>% 
  select(!ends_with(".x")) %>% ## removing unneeded columns to match EuSA
 as.data.table()

bug_class_region <- rbind(bug_class_region, EuSA)

## clean up to only keep key columns
bug_class_region <- bug_class_region[ , c("row_id","iso3c", "bacteria.code",
                                          "syndrome","n_exposed",
                                          "n_nonexposed","los",
                                          "cost_year","cost_currency", "measure",
                                          "se.measure", "avexp","lowexp",
                                          "highexp","otherexp","avnon",
                                          "lownon","highnon","othernon","p",
                                          "both_los_cost","gram.stain",
                                          "class","who.region","whoc.region","wb.region",
                                          "Income.group","minexp","maxexp",
                                          "minnon","maxnon", "unique_id_4both")]

los.est <- subset(bug_class_region, los==1)
costing.est <- subset(bug_class_region, los==0)

# # ## !!! check which ones are still missing data - need to do this by hand and check
# test <- los.est[is.na(who.region)|is.na(whoc.region)|is.na(Income.group)]
# test
# ## Taiwan is missing regional data as not listed as WHO country when list downloaded, merging in for this case for income level meta-analysis
# subset(dic_wb, Code=="TWN")

los.est[iso3c=="TWN", wb.region:="East Asia & Pacific"]
los.est[iso3c=="TWN", Income.group:="High income"]
# 
# test2 <- costing.est[is.na(who.region)|is.na(whoc.region)|is.na(Income.group)]
# test2
# unique(test2$iso3c)
# subset(dic_wb, Code=="HKG") ## Hong Kong
# 
costing.est[iso3c=="HKG", wb.region:="East Asia & Pacific"]
costing.est[iso3c=="HKG", Income.group:="High income"]

### FOR DRI RUN SEEMINGLY JUST EUSA WHICH COMES UP 

# #save data
save(los.est, file="cost_per_case/outputs/los_est_DRI.RData")
save(costing.est, file="cost_per_case/outputs/costing_est_DRI.RData")


