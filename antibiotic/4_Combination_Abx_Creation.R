######### COMBINING AND COMPARING MSH AND HILL AND GOTHAM ESTIMATES
library(tidyverse) 
library(data.table)

### reading in data
load("Data/antibiotics/costing_abx_MSH.RData")
load("Data/antibiotics/costing_abx_HILL.RData")
load("Data/antibiotics/costing_abx_Gotham.RData")

### !! in future iterations, would be more efficient to match 
## then apply to the different countries and inflat

costing.abx.currency <- costing.abx.currency [,c("iso3c" ,"who.region",
                                                 "whoc.region", "wb.region" ,
                                                 "Income.group", "MSH_antibiotic","Formulation",
                                                 "abx.adj","Route.of.Admin")] 

costing.abx.hill <- costing.abx.hill[ ,c("iso3c", "Medicine", "Unit",
                                         "generic.adj", "UK.adj" , "SA.adj",          
                                          "India.adj")]

costing.abx.Gotham <- costing.abx.Gotham[ ,c("iso3c", "Medicine", "Unit",
                                         "generic.adj", "UK.adj" , "SA.adj",          
                                         "India.adj")]


## adapting the unit/formulation columns to match
costing.abx.currency <- costing.abx.currency %>%
       mutate(Formulation = str_replace(Formulation, "\\s", "|")) %>% 
       separate(Formulation, into = c("Form", "Dose"), sep = "\\|") %>%
       filter(Form!="DISC",Form!="OTIC",Form!="OPHT", Form!="SUSPEN") %>% ## removed non-oral/injectable - although should already be removed from earlier code for MSH
        as.data.table() ## note no suspen for injection in MSH so removing suspen for oral in above code
  
costing.abx.hill <- costing.abx.hill %>% 
  mutate(Unit = str_replace(Unit, "\\s", "|")) %>% 
  separate(Unit, into = c("Form", "Dose"), sep = "\\|") %>%
  as.data.table()

# for now just to get headings:
costing.abx.Gotham <- costing.abx.Gotham %>% 
  mutate(Unit = str_replace(Unit, "\\s", "|")) %>% 
  separate(Unit, into = c("Form", "Dose"), sep = "\\|") %>%
  as.data.table()

## lower case 
costing.abx.currency[ , MSH_antibiotic := tolower(MSH_antibiotic)]
costing.abx.currency[ , Route.of.Admin := tolower(Route.of.Admin)]
costing.abx.currency[ , Form := tolower(Form)]
costing.abx.currency[ , Dose := tolower(Dose)]
costing.abx.currency[ , Dose := gsub(" ", "", Dose, fixed = TRUE)] ## remove whitespace

costing.abx.hill[ , Dose := tolower(Dose)]
costing.abx.hill[ , Form := tolower(Form)]
costing.abx.hill[ , Dose := gsub(" ", "", Dose, fixed = TRUE)]
costing.abx.hill[ , Medicine := tolower(Medicine)]

costing.abx.Gotham[ , Dose := tolower(Dose)]
costing.abx.Gotham[ , Form := tolower(Form)]
costing.abx.Gotham[ , Dose := gsub(" ", "", Dose, fixed = TRUE)]
costing.abx.Gotham[ , Medicine := tolower(Medicine)]

# ## Gotham a bit more complicated splits so will go through by hand to split
# write.csv(costing.abx.Gotham, file="Data/antibiotics/temp_midclean_Gotham_PRE.csv")

## just corrected first "batch" for first country
# !! note this wont update if update the above code as did some cleaning by hand
# as it wasn't consistent over different rows
# can copy over the formulation/dose/names from latest "post" csv and resave 
costing.abx.Gotham.NEW <- read.csv("Data/antibiotics/temp_midclean_Gotham_POST.csv")

## remove one that slipped through first hand checks (dactinomycin - not an antibiotic):
costing.abx.Gotham.NEW <- as.data.table(costing.abx.Gotham.NEW)
costing.abx.Gotham.NEW <- costing.abx.Gotham.NEW[Medicine!="dactinomycin"]
costing.abx.Gotham.NEW[ , Dose := gsub(" ", "", Dose, fixed = TRUE)] ## removing white space

## replace 0 values in prices for NA across the board (Assumed rounding issue in PDFs)
costing.abx.currency <- na_if(costing.abx.currency,0)
costing.abx.hill <- na_if(costing.abx.hill,0)
costing.abx.Gotham.NEW <- na_if(costing.abx.Gotham.NEW,0)
rm(costing.abx.Gotham)## removing to reduce confusion

## now need to repeat the hand-adapted new Forms and Doses for the other countries:
n.uniqueabx <- length(which(costing.abx.Gotham.NEW$iso3c=="AFG"))
temp.form <- costing.abx.Gotham.NEW[1:n.uniqueabx,Form]
temp.dose <- costing.abx.Gotham.NEW[1:n.uniqueabx,Dose]

n.uniqueiso <- length(unique(costing.abx.Gotham.NEW$iso3c)) ## no. of unique countries

costing.abx.Gotham.NEW[ , Form := rep(temp.form,n.uniqueiso) ]
costing.abx.Gotham.NEW[ , Dose := rep(temp.dose,n.uniqueiso) ]

## !!! assumption oral (Hill) equivalent to TAB-CAP (MSH)
costing.abx.hill[ , Form := "tab-cap"]
## assumptions for Gotham mapping
costing.abx.Gotham.NEW[Form=="powder", Form := "suspen"]
costing.abx.Gotham.NEW[Form=="solution", Form := "suspen"]
costing.abx.Gotham.NEW[Form=="injection", Form := "vial"]

## state route of admin
costing.abx.hill[ , Route.of.Admin := "po"]
costing.abx.Gotham.NEW[ , Route.of.Admin := "inj"]

## test for matches - when all = TRUE
costing.abx <- merge(costing.abx.currency, costing.abx.hill,
                     by.x=c("iso3c","MSH_antibiotic","Form","Route.of.Admin"),
                     by.y=c("iso3c","Medicine","Form","Route.of.Admin"), all = TRUE)

# mis <- costing.abx[is.na(abx.adj)]
# mis <- mis[iso3c=="USA"]  ## any country will do
### note there are ones that are both in MSH and Hill that aren't matched/matched by hand
# in this iteration as not in AWARE list but could be useful to extend in next iteration

## fixes for non-matched in first round of matching 
costing.abx.currency[MSH_antibiotic=="clindamycin (base)", MSH_antibiotic := "clindamycin"]
costing.abx.currency[MSH_antibiotic=="cloxacillin sodium", MSH_antibiotic := "cloxacillin"]
## for 200mg-250mg and 400-500mg chose smallest dosage to match to hill
costing.abx.currency[MSH_antibiotic=="metronidazole" & Dose=="200-250mg", Dose := "200mg"]
costing.abx.currency[MSH_antibiotic=="metronidazole" & Dose=="400-500mg", Dose := "400mg"]

costing.abx <- merge(costing.abx.currency, costing.abx.hill,
                     by.x=c("iso3c","MSH_antibiotic","Form"),
                     by.y=c("iso3c","Medicine","Form"), 
                     all.x = TRUE, all.y=FALSE)

## doing the same with Gotham et al
## see which ones are missing/non-matched
# costing.abxT <- merge(costing.abx, costing.abx.Gotham.NEW,
#                      by.x=c("iso3c","MSH_antibiotic","Form","Route.of.Admin.x"),
#                      by.y=c("iso3c","Medicine","Form","Route.of.Admin"), all = TRUE)
# 
# # mis <- costing.abx[is.na(abx.adj)]
# # mis <- mis[iso3c=="USA"]  ## any country will do
# # mis <- mis[ , c("MSH_antibiotic","Form","Dose")]
# rm(costing.abxT)
### going through by hand and checking mis compared to MSH list
# 	clindamycin injection (Gotham) to match MSH vial
costing.abx.Gotham.NEW[Medicine=="clindamycin", Form := "vial"]

## !!! now matching suspen to vial/ampoule for this iteration so focus 
# on route of administration rather than form
costing.abx <- merge(costing.abx, costing.abx.Gotham.NEW,
                     by.x=c("iso3c","MSH_antibiotic","Route.of.Admin.x"),
                     by.y=c("iso3c","Medicine","Route.of.Admin"), 
                     all.x = TRUE, all.y=FALSE)

## removing any duplicates (E.g. if multiple matches with WHO antibiotic data) as now using MSH antibiotic values
costing.abx <- distinct(costing.abx) 

### now need to check dosages match
dose.check <- costing.abx[iso3c=="SOM"] ## any country will do (as they should be the same)
## oral:
dose.oral <- dose.check[Route.of.Admin.x=="po"]
dose.oral.conf <- dose.oral[Dose.x==Dose.y]
dose.oral.nonconf <- dose.oral[Dose.x!=Dose.y]
length(which(is.na(dose.oral$generic.adj.x)))
## by visual inspection of dose.oral.nonconf, they are all actually different
# in dose so can be removed 
# !!! need to check this if updating code/data
dose.oral <- costing.abx[Route.of.Admin.x=="po" & Dose.x==Dose.y]
rm(dose.oral.conf)
rm(dose.oral.nonconf)

## injection:
dose.inj <- dose.check[Route.of.Admin.x=="inj"]
## lots of NAs because of non-matching of "suspen"
length(which(is.na(dose.inj$generic.adj.y))) 
dose.inj.conf <- dose.inj[Dose.x==Dose]
dose.inj.nonconf <- dose.inj[Dose.x!=Dose]
dose.inj.nonconf <- dose.inj.nonconf[, -c("generic.adj.x","UK.adj.x",
                                          "SA.adj.x","India.adj.x")]
### changes that need to be made to costing.abx
## updating 1g to match 1000mg
costing.abx[Dose.x=="1g", Dose.x := "1000mg"]
costing.abx[Dose=="2mg", Dose := "2mg/ml"]
costing.abx[Dose=="1mL/150mg", Dose := "150mg/ml"]

## by visual inspection of dose.oral.nonconf, they are all now actually different
# in dose so can be removed 
# !!! need to check this if updating code/data
dose.inj <- costing.abx[Route.of.Admin.x=="inj" & Dose.x==Dose]
rm(dose.inj.conf)
rm(dose.inj.nonconf)

## combine
abx_combo_inputs<- rbind(dose.oral, dose.inj) ## note this just leaves tab-cap and vials
# other versions of the calculations after this might need adapting if there are other forms included (e.g. ampoule)

save(abx_combo_inputs, file="Data/antibiotics/abx_combo_inputs.RData")
