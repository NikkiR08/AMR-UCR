####### READING IN AND MANIPULATING DATA FROM Gotham ET AL
library(tabulizer)
library(tidyverse) 
library(fuzzyjoin)

## loading files and functions
load("data_all/who_whoc_wb.RData") ## who_whoc_wb groupings
source("general_functions/inflation.R") ## sourcing inflation code and data files

# ### scraping & sorting the Gotham et al 2018 data
# PATH = "Data/antibiotics/Gotham_Appendix_2019.pdf"
# 
# lst_Gotham_scrape <- extract_tables(PATH, pages=8:14,output="data.frame",method="stream" )
# 
# df_Gotham_1 <- as.data.frame(lst_Gotham_scrape[[1]])
# df_Gotham_2 <- as.data.frame(lst_Gotham_scrape[[2]])
# df_Gotham_3 <- as.data.frame(lst_Gotham_scrape[[3]])
# df_Gotham_4 <- as.data.frame(lst_Gotham_scrape[[4]])
# df_Gotham_5 <- as.data.frame(lst_Gotham_scrape[[5]])
# df_Gotham_6 <- as.data.frame(lst_Gotham_scrape[[6]])
# df_Gotham_7 <- as.data.frame(lst_Gotham_scrape[[7]])
# 
# ## split column 3 in the first df
# df_Gotham_1 <- df_Gotham_1 %>% 
#   mutate(X.3 = str_replace(X.3, "\\s", "|")) %>% 
#   separate(X.3, into = c("X.3new", "X.4"), sep = "\\|") 
# 
# ## make it so they have the same number of columns
# df_Gotham_1 <- add_column(df_Gotham_1,rep(NA,nrow(df_Gotham_1)) ,.after = "X.4")
# df_Gotham_7  <- df_Gotham_7 %>% 
#                  select(-X.1)
# 
# ## combine
# lst_G <- list(df_Gotham_1,df_Gotham_2,df_Gotham_3,df_Gotham_4,df_Gotham_5,
#               df_Gotham_6, df_Gotham_7)
# Gotham_all <- rbindlist(lst_G, use.names=FALSE) ## !! note as some rows were taken as df headers
# ### in scraping need to check these by hand when at hand checking stage
# 
# ## clean as much as possible on R before hand checks (as no subgroups by antibiotics this time)
# Gotham_all <-Gotham_all %>% 
#   select(-`rep(NA, nrow(df_Gotham_1))`) %>%
#   rename(Medicine = X,
#          Unit = X.1,
#          UK.unit.price = X.2, ##!! note this is removing 15.9 which should be the first row 
#          SA.unit.price= X.3new,
#          India.unit.price= X.4,
#          generic.price = Estimated) %>%
#   filter(!is.na(Unit) & Unit!="" & Unit!="Unit") ## clearing up rows
# 
# 
# Gotham_all$Unit <- gsub("\\(|)","",Gotham_all$Unit) ## removing brackets around units to match MSH
# Gotham_all$Unit <- sub("(\\d+)", "\\1 ", Gotham_all$Unit) ## adding space between some units and measures (not perfect)
# Gotham_all$UK.unit.price <- parse_number(Gotham_all$UK.unit.price)
# Gotham_all$SA.unit.price <- parse_number(Gotham_all$SA.unit.price)
# Gotham_all$India.unit.price <- parse_number(Gotham_all$India.unit.price)
# Gotham_all$generic.price <- parse_number(Gotham_all$generic.price)
# 
# 
# ## to do the rest of the manipulations by hand and check numbers against pdf
# # !!! so note the read back in will not update automatically if other code above changed
# write.csv(Gotham_all, file="antibiotic/outputs/Gotham_PRE.csv" )
## compared in excel against antibiotics from AWARE/MSH csv file for inclusion 

Gotham_post <- read.csv("antibiotic/outputs/Gotham_POST.csv")

## remove non-antibiotics of interest
Gotham_post <- subset(Gotham_post, hand.flag.keep=="y")

## inflation
who_whoc_wb <- as.data.table(who_whoc_wb)
Gotham_post <- as.data.table(Gotham_post)
## need each country to have each antibiotic
who_whoc_wb_all <- who_whoc_wb[rep(who_whoc_wb[,.I],nrow(Gotham_post))]
abx_all <- Gotham_post[rep(Gotham_post[,.I],nrow(who_whoc_wb))]

## order the who_whoc_wb_all by country
who_whoc_wb_all<- who_whoc_wb_all[order(iso3c)]
## combine to get the abx data for each country
abx_all <- cbind(who_whoc_wb_all,abx_all)

## getting the same columns to use in the inflation function
abx_all[ , cost_year := 2016]
abx_all[ , cost_currency := "USD"]

## get local currency units matched
costing.abx.Gotham <- merge(abx_all, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)

pb = txtProgressBar(min = 1, max = nrow(costing.abx.Gotham), initial = 0, style = 3)

# converting costs
for (i in 1:nrow(costing.abx.Gotham)){
  costing.abx.Gotham[i, generic.adj := cost_adj_abx(2019,costing.abx.Gotham[i], 
                                                  "generic.price",inf_xch_4function)]
  costing.abx.Gotham[i, UK.adj := cost_adj_abx(2019,costing.abx.Gotham[i], 
                                             "UK.unit.price",inf_xch_4function)]
  costing.abx.Gotham[i, SA.adj := cost_adj_abx(2019,costing.abx.Gotham[i], 
                                             "SA.unit.price",inf_xch_4function)]
  costing.abx.Gotham[i, India.adj := cost_adj_abx(2019,costing.abx.Gotham[i], 
                                                "India.unit.price",inf_xch_4function)]
  ### !! although note that this is inefficient as e.g. those not in the final "UK" proxy group
  ### will not be using UK costs but for simplicity of having all costs in the same units in one data frame
  ### this is done to all costs 
  setTxtProgressBar(pb,i)  
}

save(costing.abx.Gotham, file="antibiotic/outputs/costing_abx_Gotham.RData")
