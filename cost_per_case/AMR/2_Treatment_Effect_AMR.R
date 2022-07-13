###################### TREATMENT EFFECT ESTIMATION
#######################################################################

############## PACKAGES #################

library(stringr)
library(dplyr)
library(data.table)
library(meta)
library(tabulizer) ## used with a previous version of R
library(tidyr)

########## SEPARATE FUNCTIONS #########
as.numeric.factor <- function(x) {as.numeric(as.character(x))}

############ READ IN FILES ##############
load("cost_per_case/inputs/wan_table1.RData")
load("cost_per_case/inputs/wan_table2.RData")

load("cost_per_case/outputs/los_est_AMR.RData")
load("cost_per_case/outputs/costing_est_AMR.RData")


######## PREVIOUS READ IN AND CONVERSION#######
# ## read in Wan 2014 tables
# PATH = "cost_per_case/inputs/wan_2014.pdf"
# lst <- extract_tables(PATH, encoding="UTF-8")
# 
# 
# ## separate tables into primary, secondary and tertiary hospital costs
# lst1 <-lst[[3]]   ## Table 1
# lst2 <- lst[[4]] ## Table 2
# 
# Table1<- as.data.frame(lst1)
# Table1 <- Table1[2:11,]
# colnames(Table1) <- NULL
# ## reshape
# a <- Table1[ ,1:2]
# b <- Table1[ ,3:4]
# c <- Table1[, 5:6]
# d <- Table1[, 7:8]
# e <- Table1[, 9:10]
# 
# Table1 <- rbindlist(list(a,b,c,d,e))
# colnames(Table1) <- c("n","table1")
# 
# Table2<- as.data.frame(lst2)
# Table2 <- Table2[4:13,]
# 
# 
# temp <- as.data.table(Table2)
# temp[ , c("i","ii","iii","iv","v") := tstrsplit(V1, " ", fixed=TRUE)]
# temp[ , c("vi","vii") := tstrsplit(V2," ", fixed=TRUE)]
# 
# a <- temp[ ,13:14]
# b <- temp[ ,15:16]
# c <- temp[,17:18]
# d <- temp[, c(19,7)]
# e <- temp[, c(10,12)]
# 
# colnames(a) <- NULL
# colnames(b) <- NULL
# colnames(c) <- NULL
# colnames(d) <- NULL
# colnames(e) <- NULL
# 
# Table2 <- rbindlist(list(a,b,c,d,e))
# 
# colnames(Table2) <- c("n","table2")
# 
# Table1[ , n := as.numeric.factor(n)]
# Table1[ , table1 := as.numeric.factor(table1)]
# 
# Table2[ , n := as.numeric.factor(n)]
# Table2[ , table2 := as.numeric.factor(table2)]
# 
# save(Table1,file="cost_per_case/inputs/wan_table1.RData")
# save(Table2, file="cost_per_case/inputs/wan_table2.RData")

######******CREATING LOS TE ESTIMATES******######
TE_creator <- function(los.est){

los.est[ , n_exposed := as.numeric.factor(n_exposed)]
los.est[ , n_nonexposed := as.numeric.factor(n_nonexposed)]
los.est[ , avexp := as.numeric.factor(avexp)]
los.est[ , avnon := as.numeric.factor(avnon)]
los.est[ , lowexp := as.numeric.factor(lowexp)]
los.est[ , highexp := as.numeric.factor(highexp)]
los.est[ , otherexp := as.numeric.factor(otherexp)]
los.est[ , lownon := as.numeric.factor(lownon)]
los.est[ , highnon := as.numeric.factor(highnon)]
los.est[ , othernon := as.numeric.factor(othernon)]
los.est[ , measure := as.character(measure)]
los.est[ , se.measure := as.character(se.measure)]

### adapting notation to fit meta r package namings etc.
los.est[ , n.e := n_exposed]
los.est[ , n.c := n_nonexposed]
los.est[ , n.t := n.e + n.c]
los.est[, pval := p]

### adding new variables needed from Wan
## for Excess (using n.t):
los.est <- merge(los.est, Table1, by.x ="n.t", by.y="n", all.x=TRUE, all.y=FALSE)
## for exposed (using n.e):
e <- Table1
colnames(e) <- c("n","table1_exp")
los.est <- merge(los.est, e, by.x="n.e",by.y="n",all.x=TRUE,all.y=FALSE)
## for nonexposed (using n.c):
c <- Table1
colnames(c) <- c("n","table1_non")
los.est <- merge(los.est, c, by.x="n.c",by.y="n",all.x=TRUE,all.y=FALSE)
rm(e)
rm(c)

## create Q
los.est[ , Qcase := round((n.t-1)/4)]
los.est[ , Qcase_exp := round((n.e-1)/4)]
los.est[ , Qcase_non := round((n.c-1)/4)]
los.est <- merge(los.est, Table2, by.x ="Qcase", by.y="n", all.x=TRUE, all.y=FALSE)
## for exposed (using n.e):
e <- Table2
colnames(e) <- c("n","table2_exp")
los.est <- merge(los.est, e, by.x="Qcase_exp",by.y="n",all.x=TRUE,all.y=FALSE)
## for nonexposed (using n.c):
c <- Table2
colnames(c) <- c("n","table2_non")
los.est <- merge(los.est, c, by.x="Qcase_non",by.y="n",all.x=TRUE,all.y=FALSE)
rm(e)
rm(c)

##### EXCESS ALREADY GIVEN #####

excess <- los.est[measure=="excess"|measure=="excess_median"]
excess[, TE := avexp] ## ASSUMING SYMMETRICAL DISTRIBUTIONS - WILL BE REPLACED WITH MORE/BETTER INFO IF THERE IS SOME AVAILABLE 
## THROUGH BELOW CALCULATIONS
excess[se.measure=="se", seTE := otherexp]
excess[se.measure=="pvalue", seTE := TE/abs(qnorm(pval/2))]
excess[se.measure=="95ci", seTE := (highexp-lowexp) / 3.92]
excess[se.measure=="sd", seTE := otherexp/sqrt(n.t)] ## if SD given

## median + IRQ + range ## no rows as of yet
#### want to make sure it's just median in future?!!!
excess[se.measure=="IQR.range", LB := ((minexp+lowexp+avexp+highexp)/4)+(((4*maxexp)-minexp-lowexp-avexp-highexp)/(4*n.t))]
excess[se.measure=="IQR.range", UB := ((lowexp+avexp+highexp+maxexp)/4)+(((4*minexp)-lowexp-avexp-highexp-maxexp)/(4*n.t))]
excess[se.measure=="IQR.range", TE := (LB+UB)/2]
excess[se.measure=="IQR.range" & n.t > 50, sdTE := ((maxexp-minexp)/
                                                           (4*qnorm((n.t-0.375)/
                                                                      (n.t+0.25))))+
                                                           ((highexp-lowexp)/
                                                           (4*qnorm(((0.75*(n.t))-0.125)/
                                                                      (n.t+0.25))))]
excess[se.measure=="IQR.range" & n.t <= 50, sdTE:= 0.5*(((maxexp-minexp)/table1)+
                                                          ((highexp-lowexp)/table2))]

## median + IQR
excess[se.measure=="IQR", TE := (lowexp+avexp+highexp)/3]
excess[se.measure=="IQR" & n.t <=50, 
       sdTE := (highexp-lowexp)/table2]
excess[se.measure=="IQR" & n.t >50, 
       sdTE := (highexp-lowexp)/2*(qnorm(((0.75*n.t)-0.125)/
                                             (n.t+0.25)))]
# ## median + range
excess[measure=="excess_median"& se.measure=="range", TE := ((lowexp+(2*avexp)+highexp)/4)+((lowexp-(2*avexp)+highexp)/(4*n.t))]
excess[measure=="excess" & se.measure=="range", TE := avexp]
# ## mean + range (SE calculation same for mean or median + range)
excess[se.measure=="range" & n.t <= 50,
       sdTE := (highexp - lowexp)/table1]
excess[se.measure=="range" & n.t > 50,
       sdTE := (highexp-lowexp)/2*(qnorm((n.t-0.375)/
                                           (n.t+0.25)))]

########### ESTIMATING TE DIFFERENCE FROM EXPOSED V NONEXPOSED ######
### Convert all to mean and SD using Wan et al (2014) formulae
diff2calc <- los.est[measure!="excess"& measure!="excess_median"]

## mean and SE values given
diff2calc[measure=="mean", mean_exp := avexp]
diff2calc[se.measure=="se", se_exp := otherexp]
diff2calc[measure=="mean", mean_non := avnon]
diff2calc[se.measure=="se", se_non := othernon]

## confidence intervals for both - assumes normal distribution
diff2calc[se.measure=="95ci", mean_exp := avexp]
diff2calc[se.measure=="95ci", se_exp := (highexp-lowexp) / 3.92]
diff2calc[se.measure=="95ci", mean_non := avnon]
diff2calc[se.measure=="95ci", se_non := (highnon-lownon) / 3.92]

## SD given
diff2calc[se.measure=="sd", mean_exp := avexp]
diff2calc[se.measure=="sd", sd_exp := otherexp]
diff2calc[se.measure=="sd", mean_non := avnon]
diff2calc[se.measure=="sd", sd_non := othernon]

##############CASE DATA #####
## median + IRQ + range
diff2calc[se.measure=="IQR.range", LB := ((minexp+lowexp+avexp+highexp)/4)+(((4*maxexp)-minexp-lowexp-avexp-highexp)/(4*n.e))]
diff2calc[se.measure=="IQR.range", UB := ((lowexp+avexp+highexp+maxexp)/4)+(((4*minexp)-lowexp-avexp-highexp-maxexp)/(4*n.e))]
diff2calc[se.measure=="IQR.range", mean_exp := (LB+UB)/2]
diff2calc[se.measure=="IQR.range" & n.e > 50, sd_exp := ((maxexp-minexp)/
                                                           (4*qnorm((n.e-0.375)/
                                                                      (n.e+0.25))))+
                                                           ((highexp-lowexp)/
                                                           (4*qnorm((0.75(n.e)-0.125)/
                                                                      (n.e+0.25))))]
diff2calc[se.measure=="IQR.range" & n.e <= 50, 
          sd_exp:= 0.5*(((maxexp-minexp)/
                           table1_exp)+((highexp-lowexp)/
                                             table2_exp))]
## median + IQR
diff2calc[se.measure=="IQR", mean_exp := (lowexp+avexp+highexp)/3]
diff2calc[se.measure=="IQR" & n.e <=50, 
          sd_exp := (highexp-lowexp)/table2_exp]
diff2calc[se.measure=="IQR" & n.e >50, 
          sd_exp := (highexp-lowexp)/2*(qnorm(((0.75*n.e)-0.125)/
                                                  (n.e+0.25)))]
## median + range
diff2calc[measure=="median" & se.measure=="range", 
          mean_exp := ((lowexp+(2*avexp)+highexp)/4)+((lowexp-(2*avexp)+highexp)/(4*n.e))]
diff2calc[se.measure=="range" & n.e <= 50, 
          sd_exp := (highexp - lowexp)/table1_exp]
diff2calc[se.measure=="range" & n.e > 50, 
          sd_exp := (highexp-lowexp)/2*(qnorm((n.e-0.375)/
                                                (n.e+0.25)))]

###################### CONTROL DATA ########
## median + IRQ + range
diff2calc[se.measure=="IQR.range", LB := ((minnon+lownon+avnon+highnon)/4)+(((4*maxnon)-minnon-lownon-avnon-highnon)/(4*n.c))]
diff2calc[se.measure=="IQR.range", UB := ((lownon+avnon+highnon+maxnon)/4)+(((4*minnon)-lownon-avnon-highnon-maxnon)/(4*n.c))]
diff2calc[se.measure=="IQR.range", mean_non := (LB+UB)/2]
diff2calc[se.measure=="IQR.range" & n.c > 50, sd_non := ((maxnon-minnon)/
                                                                 (4*qnorm((n.c-0.375)/
                                                                                  (n.c+0.25))))+
                  ((highnon-lownon)/
                           (4*qnorm((0.75(n.c)-0.125)/
                                            (n.c+0.25))))]
diff2calc[se.measure=="IQR.range" & n.c <= 50, 
          sd_non:= 0.5*(((maxnon-minnon)/
                                 table1_non)+((highnon-lownon)/
                                                      table2_non))]
## median + IQR
diff2calc[se.measure=="IQR", mean_non := (lownon+avnon+highnon)/3]
diff2calc[se.measure=="IQR" & n.c <=50, 
          sd_non := (highnon-lownon)/table2_non]
diff2calc[se.measure=="IQR" & n.c >50, 
          sd_non := (highnon-lownon)/2*(qnorm(((0.75*n.c)-0.125)/
                                                      (n.c+0.25)))]
## median + range
diff2calc[measure=="median" & se.measure=="range",
                                mean_non := ((lownon+(2*avnon)+highnon)/4)+
                                        ((lownon-(2*avnon)+highnon)/(4*n.c))]
diff2calc[se.measure=="range" & n.c <= 50, 
          sd_non := (highnon - lownon)/table1_non]
diff2calc[se.measure=="range" & n.c > 50, 
          sd_non := (highexp-lowexp)/2*(qnorm((n.c-0.375)/
                                                (n.c+0.25)))]

diff2calc[measure=="mean", mean_exp := avexp]
diff2calc[measure=="mean", mean_non := avnon]
# with lack of other data..assuming symmetrical & same outcome measure (mean difference)
diff2calc[measure=="median" & se.measure=="pvalue", mean_exp := avexp]
diff2calc[measure=="median" & se.measure=="pvalue", mean_non := avnon]

##### convert SD to SE ######
excess[is.na(seTE) & !is.na(sdTE), seTE := sdTE/sqrt(n.t)]

## getting errors from se and sd measure as character 
diff2calc[  , sd_exp := as.numeric(sd_exp)]
diff2calc[  , se_exp := as.numeric(se_exp)]

diff2calc[is.na(se_exp) & !is.na(sd_exp), se_exp := sd_exp/sqrt(n.e)]
diff2calc[is.na(se_non) & !is.na(sd_non), se_non := sd_non/sqrt(n.c)]

###### calculate mean of difference & SE of difference #######
diff2calc[ , TE := mean_exp - mean_non]
diff2calc[ , seTE := sqrt(((se_exp)^2)+((se_non)^2))]
diff2calc[(measure=="median"|measure=="mean") & se.measure=="pvalue", seTE := TE/abs(qnorm(pval/2))]

#### create data combining the different groups

## clean up the columsn for each
excess <- excess[ , c("iso3c","bacteria.code","syndrome","los","gram.stain",
                      "class","who.region","whoc.region","wb.region","Income.group",
                      "n.e","n.c","n.t","TE","seTE", "cost_year","cost_currency","row_id", "unique_id_4both")]
diff2calc <- diff2calc[ , c("iso3c","bacteria.code","syndrome","los","gram.stain",
                            "class","who.region","whoc.region","wb.region","Income.group",
                            "n.e","n.c","n.t","TE","seTE","cost_year","cost_currency","row_id", "unique_id_4both")]
## bind
los.TE <- rbindlist(list(excess,diff2calc))

return(los.TE)
}


##*** had to assume symmetry to get quartiles 1 and 3 from median and IQR
los.est[se.measure=="IQR+", lowexp:=avexp-(otherexp/2)]
los.est[se.measure=="IQR+", highexp:=(otherexp/2)+avexp]
los.est[se.measure=="IQR+", lownon:=avnon-(othernon/2)]
los.est[se.measure=="IQR+", highnon:=(othernon/2)+avnon]
los.est[se.measure=="IQR+", se.measure:="IQR"]


los.TE <- TE_creator(los.est)
## naylor_151 is na for n_exposed and n_nonexposed because there are no sample sizes extracted
## but in that instance n isn't used in calculations

# ##!!! testing TE creation
# test <- subset(los.TE, is.na(TE))

save(los.TE, file="cost_per_case/outputs/los_TE.RData")

######******CREATING COSTING TE ESTIMATES******######

costing.TE <- TE_creator(costing.est)
## naylor_151 is na for n_exposed and n_nonexposed because there are no sample sizes extracted

# ##!!! testing TE creation
# test <- subset(costing.TE, is.na(TE))

save(costing.TE, file="cost_per_case/outputs/costing_TE.RData")



