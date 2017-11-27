# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Determine crop for each location based on field-strip map

# ---- Setup ----
library(rgdal)
library(geojsonio)
library(jsonlite)
library(spatialEco)

#y <- read.csv("Input/HY2014GB_aggregate_all_data_171122.csv")
refPoints <- geojson_read("Input/CookEast_GeoreferencePoints_171117.json", what = "sp")

# Convert csv gain mass data to spatial points and specify the datum
df <- refPoints@data

# Set crops - this is based on Y2014 in Input/CAF_strips.shp
df$Crop <- NA
df[df$Strip == 6 & df$Field == "A",]$Crop = "SW"
df[df$Strip == 5 & df$Field == "A",]$Crop = "SW"
df[df$Strip == 4 & df$Field == "A",]$Crop = "SL"
df[df$Strip == 3 & df$Field == "A",]$Crop = "SL"
df[df$Strip == 2 & df$Field == "A",]$Crop = "SL"
df[df$Strip == 1 & df$Field == "A",]$Crop = "SL"

df[df$Strip == 6 & df$Field == "B",]$Crop = "WW"
df[df$Strip == 5 & df$Field == "B",]$Crop = "WW"
df[df$Strip == 4 & df$Field == "B",]$Crop = "WW"
df[df$Strip == 3 & df$Field == "B",]$Crop = "WW"
df[df$Strip == 2 & df$Field == "B",]$Crop = "WW"
df[df$Strip == 1 & df$Field == "B",]$Crop = "WW"

df[df$Strip == 1 & df$Field == "C",]$Crop = "SW"
df[df$Strip == 2 & df$Field == "C",]$Crop = "SL"
df[df$Strip == 3 & df$Field == "C",]$Crop = "SB"
df[df$Strip == 4 & df$Field == "C",]$Crop = "WW"
df[df$Strip == 5 & df$Field == "C",]$Crop = "SW"
df[df$Strip == 6 & df$Field == "C",]$Crop = "SW"
df[df$Strip == 7 & df$Field == "C",]$Crop = "SW"
df[df$Strip == 8 & df$Field == "C",]$Crop = "SW"

refPoints@data = df

write.csv(refPoints, "Output/HY2014_Crops.csv")