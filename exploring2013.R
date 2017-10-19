# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Explore yield data from hand harvest datasets for QC

# Set WD, define constants
setwd("C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CookEastPlantHandHarvest\\1999-2016\\Working\\R")
area.harvested <- 2.4384
harvest.year <- 2013
source("graphing-funcs.R")

# Read input data
strips <- readOGR("Working/CookEastStrips2013", "Field_Plan_Final")
boundary <- readOGR("Input/CookEastArea", "CafCookEastArea")
d <- read.csv("Input/HY2013GP_GrainWeightOnly_171012.csv", stringsAsFactors = TRUE)

# Original strips polygon of Cook East has area with no georef points (and no yield), so remove them
georeff.only <- raster::intersect(boundary, strips)

#GB
map_yield(d, area.harvested, georeff.only, 
          extract_georef_field_and_strip(c(5), c(1,2,3,4), NULL, georeff.only), 
          harvest.year, "GB")

#SB
map_yield(d,  area.harvested, georeff.only, 
          extract_georef_field_and_strip(c(6), NULL, NULL, georeff.only), 
          harvest.year, "SB")

#SW
map_yield(d, area.harvested, 
          georeff.only,extract_georef_field_and_strip(c(2,3,4), c(5, 6), NULL, georeff.only), 
          harvest.year, "SW")

#WW
map_yield(d, area.harvested, georeff.only, 
          extract_georef_field_and_strip(c(1), NULL, c(1, 2, 3, 4, 5, 6, 7, 7, 8), georeff.only), 
          harvest.year, "WW")