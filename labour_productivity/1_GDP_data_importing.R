############# IMPORTING OF DATA FROM PACKAGES AND CSVS
## LOADING THE PACKAGES
library(pwt9)
library(wbstats)
library(Rilostat)
library(WDI)
library(dplyr)
library(forecast)
library(data.table)

# ## indicator dictionaries
# wbdic <- wbcache(lang="en")
# wbind <- wbdic$indicators
# ilodic <-  get_ilostat_toc()

####### base table
data_input <- read.csv("data_all/who_whoc_wb.csv") ## getting list of all countries

# #### Penn World Tabel data #####
# data("pwt9.1")
# pwt_data <- pwt9.1 %>%
#   select(year, isocode, rgdpna, rtfpna,
#          rnna, hc,labsh, delta) %>%
#   filter(year >= 2005,
#          year <= 2020) %>%
#   na.omit() %>%
#   as.data.table()
# save(pwt_data, file="labour_productivity/inputs/pwt.RData")

######## ILO data  #########
# LFP <- get_ilostat(id = 'EAP_2WAP_SEX_AGE_RT_A', segment = 'indicator')
# save(LFP, file="labour_productivity/inputs/ilo.RData")


####### World Bank Data ##########
# I0 <- wb_data(indicator = c("NE.GDI.FTOT.KD","NY.GDP.MKTP.KD"))
# I0 <- I0 %>%
#   select(iso3c, country, date, NE.GDI.FTOT.KD, NY.GDP.MKTP.KD) %>%
#   na.omit() %>%
#   as.data.table()
# save(I0, file="labour_productivity/inputs/I0.RData")

#### GDP per capita - just to check values 
# ypc <- wb_data(indicator = c("NY.GDP.PCAP.KD"))
# ypc <- ypc %>%
#   select(iso3c, country, date, "NY.GDP.PCAP.KD") %>%
#   na.omit() %>%
#   as.data.table()
# save(ypc, file="labour_productivity/inputs/ypc.RData")

########### Labour Data ###################
N <- as.data.table(read.csv("data_all/Population-EstimatesData_092020.csv"))

N <- N[Indicator.Code=="SP.POP.TOTL"| ## total population
           Indicator.Code=="SP.POP.1564.TO.ZS"] ## working age % of whole population

## reshape data so year are a variable not column names:
yearsreshape <- colnames(N)
yearsreshape <- yearsreshape[-(1:4)]
N <- melt(N, measure.vars = dput(as.character(yearsreshape)),
           variable.name = "year", value.name = "value") ## melt columns to a year variable
N$year = as.numeric(gsub("\\X", "", N$year)) ## remove x value across them and turn numeric
N <- N[year %in% c(2019:2035)]  ### get the years we want
N <- N[ , c("Country.Code","Indicator.Code","year","value")]
 
### creation of the growth rates
N <-  dcast(N, Country.Code + year ~ Indicator.Code, value.var="value", fill=0)
N <- N %>%
  group_by(Country.Code) %>% # For each country calculate...
  mutate(g_n = (SP.POP.TOTL - lag(SP.POP.TOTL))/lag(SP.POP.TOTL), # growth rate of population
         g_w = (SP.POP.1564.TO.ZS - lag(SP.POP.1564.TO.ZS))/lag(SP.POP.1564.TO.ZS),
         W_N = SP.POP.1564.TO.ZS/100) %>% 
      as.data.table() 
N <- N[ , -c("SP.POP.1564.TO.ZS")]  
colnames(N) <- c("country","year","N","g_n","g_wn","W_N")

load("labour_productivity/inputs/ilo.RData")
LFP <- as.data.table(LFP)
LFP <- LFP[classif1=="AGE_5YRBANDS_TOTAL"& sex=="SEX_T"]
LFP[ , LFP := obs_value/100]
LFP <- LFP[time %in% c(2019:2035)]
LFP <- LFP %>%
  group_by(ref_area) %>% # For each country calculate...
  mutate(g_lfp = (LFP - lag(LFP))/lag(LFP),
         g_lfp_av= mean(g_lfp, na.rm=TRUE)) %>% # growth rate of lfp
  filter(time == 2019)%>%
           as.data.table() 
LFP <- LFP[ ,c("ref_area","time","LFP","g_lfp_av")]
colnames(LFP) <- c("country","year","LFP","g_lfp_av")

###### Investment & Capital #########
load("labour_productivity/inputs/pwt.RData")
PWT <- pwt_data %>% 
  group_by(isocode) %>% # For each country calculate...
  mutate(K_Y = rnna/rgdpna, ## capital to output ratio
         g_h = (hc-lag(hc))/lag(hc), ## growth in human capital
         g_a = (rtfpna-lag(rtfpna))/lag(rtfpna), # growth in tfp
         g_h_av = mean(g_h, na.rm=TRUE),  # average over period
          g_a_av = mean(g_a, na.rm=TRUE)) %>% # average over period
        as.data.table() 

temp <- PWT[ , -c("labsh","delta")]
beta_delta <- PWT[year==as.numeric(max(PWT$year))] ## get latest year (the all have last year 2017 otherwise would have to use other code)
beta_delta <- beta_delta[ , c("isocode","labsh","delta")] ## keep columns needed,  note these are rates/shares
PWT <- merge(temp, beta_delta, by="isocode")
names(PWT)[names(PWT)=="isocode"] <- "country"
PWT <- PWT[year==as.numeric(max(PWT$year))] ## latest year available !!! would want to update with new data
rm(beta_delta)
rm(temp)

## I/Y
# get the latest year available
load("labour_productivity/inputs/I0.RData")
I0 <- I0 %>% 
  group_by(iso3c) %>%
  as.data.table()
 
I0[ , I_Y := NE.GDI.FTOT.KD/NY.GDP.MKTP.KD] ##Base level Gross Fixed capital formation to GDP ratio
I0 <- I0[ ,-c("NE.GDI.FTOT.KD","country")]
colnames(I0) <- c("country","year","GDP","I_Y")

I0 <- I0 %>%
  group_by(country) %>%
mutate(g_IY = (I_Y-lag(I_Y))/lag(I_Y),
       g_IY_av = mean(g_IY, na.rm=TRUE)) %>%
         slice(which.max(year)) %>% ## get latest year for each (different across countries)
          as.data.table()

### if only one year assumed 0 growth !!!
I0[is.na(g_IY_av), g_IY_av := 0]
## split up for those 2019 and those not
I0_ready <- I0[year==2019]
I0_old <- I0[year<2019]
# minimum year = 2010 - set all to additional rows of 10 row now
### model I0_old forward to 2019
I0_old <- I0_old %>% 
  slice(rep(1:n(), each = 10)) %>%
  as.data.table()

countries <- unique(I0_old$country)
temp_l <- list()
for (i in 1:length(countries)){
  iso <- countries[[i]]
  temp_l[[i]] <- I0_old[country==iso]
  years <- as.data.frame(temp_l[[i]])
  years <- length(years$year)
  for (j in 2:years){
    temp_l[[i]]$I_Y[j] <-(temp_l[[i]]$I_Y[j-1]+
                            (temp_l[[i]]$I_Y[j-1]*temp_l[[i]]$g_IY_av[[j]]))
    temp_l[[i]]$year[j] <-(temp_l[[i]]$year[j-1])+1
  }
}
i_forecast <- as.data.table(do.call(rbind, temp_l))
I0_old <- i_forecast[year==2019]### keep just 2019
I0 <- rbind(I0_ready, I0_old) ## bind together

#### combine them all together#####
macro_data <- merge(LFP, I0, by="country")
macro_data <- macro_data[ , -c("year.x","year.y")] ## removing these as constants/base values 
macro_data <- merge(macro_data,PWT, by="country")
macro_data <- macro_data[ , -c("year")]
macro_data <- merge(macro_data, N, by="country")

### REMOVE NON 2010 ABSOLUTE VALUES TO REDUCE LIKELIHOOD OF USAGE
### AS GDP ETC IN CONSTANT 2010 USD$

macro_data <- macro_data[ ,-c("rgdpna" ,  "rtfpna"  , "rnna"  ,"hc" ,
                              "LFP")]

save(macro_data, file="labour_productivity/outputs/macro_data.RData")
