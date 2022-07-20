####### READING IN AND MANIPULATING DATA FROM HILL ET AL
library(tabulizer)
library(tidyverse) 

## loading files and functions
load("data_all/who_whoc_wb.RData") ## who_whoc_wb groupings
source("general_functions/inflation.R") ## sourcing inflation code and data files

# ### scraping & sorting the Hill et al 2018 data
# PATH = "antibiotic/inputs/Hill_Appendix_2018.pdf"
# 
# lst_hill_scrape <- extract_tables(PATH, pages = 3:5, output="data.frame",method="stream" )
# 
# df_hill_1 <- as.data.frame(lst_hill_scrape[[1]])
# df_hill_2 <- as.data.frame(lst_hill_scrape[[2]])
# df_hill_3 <- as.data.frame(lst_hill_scrape[[3]])
# 
# ### cleaning to match up to bind
# 
# hill_1 <- df_hill_1 %>% 
#               select(-X.1, -X.2) %>%
#   mutate(X = str_replace(X, "\\s", "|")) %>% ## separating out medicine and unit
#   separate(X, into = c("Medicine", "Unit"), sep = "\\|") %>%
#   rename(SA.unit.price = X.3,
#          India.unit.price= X.4,
#          generic.price = X.5) %>%
#   filter(row_number()>41) ## antibiotics are below row 41 in this extraction
# 
# 
# hill_2 <- df_hill_2 %>% 
#   select(-X.2) %>%
#   rename(Medicine = X,
#          Unit = X.1,
#          SA.unit.price = X.3,
#          India.unit.price= X.4,
#          generic.price = X.5) %>%
#   filter(row_number()>4) ## antibiotics are below row 4 in this extraction
# 
# 
# hill_3 <- df_hill_3 %>% 
#   select(-X.2,-X.3) %>%
#   rename(Medicine = X,
#          Unit = X.1,
#          UK.unit.price = X15.9Â.., ##!! note this is removing 15.9 which should be the first row 
#          SA.unit.price= X.4,
#          India.unit.price= X.5,
#          generic.price = X.6) 
# 
# hill_3 <- rbind(c(NA,NA,15.9,NA,NA,NA), hill_3) ## create the row that was deleted
# 
# hill <- rbind(hill_1,hill_2)
# hill <- rbind(hill, hill_3)
# 
# 
# hill$Unit <- gsub("\\(|)","",hill$Unit) ## removing brackets around units to match MSH
# hill$Unit <- sub("(\\d+)", "\\1 ", hill$Unit) ## adding space between some units and measures (not perfect)
# hill$UK.unit.price <- parse_number(hill$UK.unit.price)
# hill$SA.unit.price <- parse_number(hill$SA.unit.price)
# hill$India.unit.price <- parse_number(hill$India.unit.price)
# hill$generic.price <- parse_number(hill$generic.price)
# 
# ## to do the rest of the manipulations by hand and check numbers against pdf
# # !!! so note the read back in will not update automatically if other code above changed
# write.csv(hill, file="antibiotic/outputs/Hill_PRE.csv" )

hill_post <- read.csv("antibiotic/outputs/Hill_POST.csv")

## make dollar not cents
hill_post$UK.unit.price <- hill_post$UK.unit.price/100
hill_post$SA.unit.price <- hill_post$SA.unit.price/100
hill_post$India.unit.price <- hill_post$India.unit.price/100
hill_post$generic.price <- hill_post$generic.price/100

## inflation
who_whoc_wb <- as.data.table(who_whoc_wb)
hill_post <- as.data.table(hill_post)

## need each country to have each antibiotic
who_whoc_wb_all <- who_whoc_wb[rep(who_whoc_wb[,.I],nrow(hill_post))]
abx_all <- hill_post[rep(hill_post[,.I],nrow(who_whoc_wb))]

## order the who_whoc_wb_all by country
who_whoc_wb_all<- who_whoc_wb_all[order(iso3c)]
## combine to get the abx data for each country
abx_all <- cbind(who_whoc_wb_all,abx_all)

## getting the same columns to use in the inflation function
abx_all[ , cost_year := 2016]
abx_all[ , cost_currency := "USD"]
## get local currency units matched
costing.abx.hill <- merge(abx_all, currency_country, by="iso3c", all.x=TRUE, all.y=FALSE)

pb = txtProgressBar(min = 1, max = nrow(costing.abx.hill), initial = 0, style = 3)

# converting costs
for (i in 1:nrow(costing.abx.hill)){
  costing.abx.hill[i, generic.adj := cost_adj_abx(2019,costing.abx.hill[i], 
                                                  "generic.price",inf_xch_4function)]
  costing.abx.hill[i, UK.adj := cost_adj_abx(2019,costing.abx.hill[i], 
                                                      "UK.unit.price",inf_xch_4function)]
  costing.abx.hill[i, SA.adj := cost_adj_abx(2019,costing.abx.hill[i], 
                                                      "SA.unit.price",inf_xch_4function)]
  costing.abx.hill[i, India.adj := cost_adj_abx(2019,costing.abx.hill[i], 
                                                      "India.unit.price",inf_xch_4function)]
  ### !! although note that this is inefficient as e.g. those not in the final "UK" proxy group
  ### will not be using UK costs but for simplicity of having all costs in the same units in one data frame
  ### this is done to all costs 
  setTxtProgressBar(pb,i)  
}

save(costing.abx.hill, file="Data/antibiotics/costing_abx_HILL.RData")