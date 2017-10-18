# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Explore yield data from hand harvest datasets for QC

# Set WD, define constants
setwd("C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CookEastPlantHandHarvest\\1999-2016\\Working\\R")
area.harvested <- 2.4384
source("graphing-funcs.R")

# Read input data
strips <- readOGR("Working/CookEastStrips2013", "Field_Plan_Final")
boundary <- readOGR("Input/CookEastArea", "CafCookEastArea")
d <- read.csv("Input/HY2015GP_GrainWeightOnly_171018.csv", stringsAsFactors = TRUE)

# Original strips polygon of Cook East has area with no georef points (and no yield), so remove them
georef.only <- raster::intersect(boundary, strips)

#GB
a.strips <- NULL
b.strips <- c(6)
c.strips <- c(1,4,5,6,7,8)
map_yield(d, area.harvested, georef.only, extract_georef_field_and_strip(a.strips, b.strips, c.strips, georef.only), "GB")

#SB
a.strips <- NULL
b.strips <- c(5)
c.strips <- NULL
map_yield(d,  area.harvested, georef.only, extract_georef_field_and_strip(a.strips, b.strips, c.strips, georef.only), "SB")

#SW
a.strips <- NULL
b.strips <- NULL
c.strips <- c(3)
map_yield(d, area.harvested, georef.only,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georef.only), "SW")

#WW
a.strips <- c(1,2,3,4,5,6)
b.strips <- c(4)
c.strips <- NULL
map_yield(d, area.harvested, georef.only, extract_georef_field_and_strip(a.strips, b.strips, c.strips, georef.only), "WW")
