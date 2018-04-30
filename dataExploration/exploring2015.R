# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Explore yield data from hand harvest datasets for QC

# Set WD, define constants
setwd("C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CookEastPlantHandHarvest\\1999-2016\\Working\\R")
area.harvested <- 2.4384
harvest.year <- 2015
source("graphing-funcs.R")

# Read input data
strips <- readOGR("Working/CookEastStrips2013", "Field_Plan_Final")
boundary <- readOGR("Input/CookEastArea", "CafCookEastArea")
d2015 <- read.csv("Input/HY2015GP_GrainWeightOnly_171020.csv", stringsAsFactors = TRUE)

# Original strips polygon of Cook East has area with no georef points (and no yield), so remove them
georef.only <- raster::intersect(boundary, strips)

#GB
map_yield(d2015, area.harvested, georef.only, 
          extract_georef_field_and_strip(NULL, c(6), c(1,4,5,6,7,8), georef.only), 
          harvest.year, "GB")

#SB
map_yield(d2015,  area.harvested, georef.only, 
          extract_georef_field_and_strip(NULL, c(5), NULL, georef.only), 
          harvest.year, "SB")

#SW
map_yield(d2015, area.harvested, georef.only,
          extract_georef_field_and_strip(NULL, NULL, c(3), georef.only), 
          harvest.year, "SW")

#WW
map_yield(d2015, area.harvested, georef.only, 
          extract_georef_field_and_strip(c(1,2,3,4,5,6), c(4), NULL, georef.only), 
          harvest.year, "WW")

map_yield(d2015, area.harvested, georef.only, 
          extract_georef_field_and_strip(NULL, c(1,2,3), c(2), georef.only), 
          harvest.year, "SC")