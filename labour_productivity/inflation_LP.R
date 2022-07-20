### inflation data creation #######
library(wbstats)
library(data.table)

#### !!! next iteration would be good to combine with other 
### inflation script in general_functions


## loading in data from wbstats 
# gdpdeflator_data <- wb_data(indicator = "NY.GDP.DEFL.ZS")
# # # save(gdpdeflator_data, file="data_all/wb_GDP_deflator.RData")
# load("data_all/wb_GDP_deflator.RData")
# ## merge in with mapping of countries to get regional averages
# inflation_source <- merge(gdpdeflator_data, data_input, by="iso3c")
# inflation_source <- as.data.table(inflation_source)
# inflation_source <- inflation_source[, country_flag:=.GRP,by=iso3c]
# save(inflation_source, file="data_all/inflation_source.RData")

load("data_all/inflation_source.RData")

inflation_adjust_cost_custom <- function(from_year,
                                         to_year,
                                         cost_dt_row,
                                         column_ref,
                                         inflation_dt) {
  iso <- as.character(cost_dt_row$iso3c) 
  from_cost <- cost_dt_row[[column_ref]]
  temp <- inflation_dt[iso3c==iso]
  ## check if have country values in the inflation data set
  test <- inflation_dt[date == from_year]
  test2 <- inflation_dt[date == to_year]
    if (iso %in% test$iso3c==TRUE & iso %in% test2$iso3c){
       from_index <- temp[temp$date == from_year, "NY.GDP.DEFL.ZS"] 
       to_index <- temp[temp$date == to_year, "NY.GDP.DEFL.ZS"]
       to_cost <- from_cost * (to_index/from_index)
    } else {
        to_cost <- NA
    }
  return(to_cost)
}

#### how to use 
# source("inflation.R")
# 
# for (i in 1:nrow(I0)){
#   I0[i, I2019 := inflation_adjust_cost_custom(I0$date[i],2019,I0[i,],
#                                               "NE.GDI.FTOT.KD",inflation_source)]
# }
