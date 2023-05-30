##### testing who_whoc_wb

who_whoc_wb <- read_csv("data_all/who_whoc_wb.csv")
who_region_updated <- read_csv("data_all/who_region_updated.csv")

who_region_updated <- subset(who_region_updated,!is.na(WHO_REGION))

who1 <- unique(who_whoc_wb$iso3c)
who2 <- unique(who_region_updated$DimensionMemberCode)

print(setdiff(who2,who1))

####!!! if rerunning analyses try adding these ones into dictionary:
# ### > print(setdiff(who2,who1))
# [1] "ME1" "AIA" "PSE" "SDF"
