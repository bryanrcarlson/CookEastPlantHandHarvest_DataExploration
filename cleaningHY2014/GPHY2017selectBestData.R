# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Select "best" data from various conflicting values

# ---- Setup ----
library(rgdal)
library(geojsonio)
#library(jsonlite)
#library(spatialEco)

y <- read.csv("Input/HY2014GB_aggregate_all_data_171122.csv")
georef <- geojson_read("Input/CookEast_GeoreferencePoints_171127.json", what = "sp")

t <- merge(y, georef, by.x = c("Col", "Row2"), by.y = c("Column", "Row2"), all.x = TRUE)