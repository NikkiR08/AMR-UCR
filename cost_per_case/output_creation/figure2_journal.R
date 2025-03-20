
############ additional code for new figure 2 in journal submission

library(data.table)
library(rworldmap)
library(dplyr)

# Updated Mapping Function with Even Narrower Legend
mapping_function_1 <- function(x, y){
  # Adjust margins to give more space to the map
  par(mar = c(2, 1, 3, 1))  # (bottom, left, top, right)
  
  # Generate the map without the legend
  theMap <- mapCountryData(x, nameColumnToPlot = y, addLegend = FALSE,
                           colourPalette = "terrain",
                           mapTitle = "(A) Cost-per-case based on length of stay")
  
  # Adjust legend labels
  labs <- theMap$cutVector %>% exp() %>% round(-2)
  
  # Add a smaller and narrower legend manually
  do.call(addMapLegend, c(theMap,
                          legendWidth = 0.5,  # Reduce legend width further
                          legendMar = 5,  # Reduce margin for legend
                          legendShrink = 0.5,  # Shrink the legend color scale
                          legendIntervals = "page",
                          legendLabels = "all"))  # Keep legend vertical
  
  print(labs)
}

mapping_function_2 <- function(x, y){
  # Adjust margins for better map scaling
  par(mar = c(2, 1, 3, 1))  # (bottom, left, top, right)
  
  # Generate the map without the legend
  theMap <- mapCountryData(x, nameColumnToPlot = y, addLegend = FALSE,
                           colourPalette = "terrain",
                           mapTitle = "(B) Cost-per-case based on length of stay and cost estimates")
  
  # Adjust legend labels
  labs <- theMap$cutVector %>% exp() %>% round(-2)
  
  # Add a smaller and narrower legend manually
  do.call(addMapLegend, c(theMap,
                          legendWidth = 0.5,  # Reduce legend width further
                          legendMar = 5,  # Reduce margin for legend
                          legendShrink = 0.5,  # Shrink the legend color scale
                          legendIntervals = "page",
                          legendLabels = "all"))  # Keep legend vertical
  
  print(labs)
}

# Load Data
load("cost_per_case/outputs/scenario2_results_4plot.RData")

# Select relevant data
sc2.results.long.keep <- sc2.results.long.keep[AMR_or_DRI == "DRI"]
sc2.results.long.keep.sig <- sc2.results.long.keep[`Low 95% UI Bound - from Excess LOS` >= 0]

sc2.results.sum <- sc2.results.long.keep.sig[, lapply(.SD, median, na.rm = TRUE),
                                             by = c("Country (ISO3 Code)", "AMR_or_DRI"),
                                             .SDcols = c("Mean Cost - from Excess LOS")]

joinData1 <- joinCountryData2Map(sc2.results.sum,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "Country (ISO3 Code)")

# set up for saving tiff
tiff("Figure2.tiff", width = 10, height = 14, units = "in", res = 300)

# Set plotting area for two vertically stacked maps
par(mfrow = c(2, 1))  

# Plot the first map with improved spacing
mapping_function_1(joinData1, "Mean Cost - from Excess LOS")

# Load the second dataset
load("cost_per_case/outputs/scenario2_results_4plot.RData")

sc2.results.long.keep <- sc2.results.long.keep[AMR_or_DRI == "DRI"]
sc2.results.long.keep.sig <- sc2.results.long.keep[`Low 95% UI Bound - Across Both` >= 0]

sc2.results.sum <- sc2.results.long.keep.sig[, lapply(.SD, median, na.rm = TRUE),
                                             by = c("Country (ISO3 Code)", "AMR_or_DRI"),
                                             .SDcols = c("Mean Cost - Across Both")]

joinData1 <- joinCountryData2Map(sc2.results.sum,
                                 joinCode = "ISO3",
                                 nameJoinColumn = "Country (ISO3 Code)")

# Plot the second map with improved spacing
mapping_function_2(joinData1, "Mean Cost - Across Both")

# close Tiff
dev.off()

# Reset default layout
par(mfrow = c(1,1))
