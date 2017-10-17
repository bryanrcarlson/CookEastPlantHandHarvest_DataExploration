library(sp)
library(rgdal)
library(dismo)
library(gstat)

#' Creates a IDW map of yield values from grain weight
#' @param d A dataframe with grain mass data in WGS84 datum and columns: FieldID	Column	Row2	ID2	Latitude	Longitude	Year	SampleID	Crop	GrainWeightWet
#' @param area.harvested A number for the area harvested in m2, grain mass will be divided by this number to get mass/area
#' @param boundary A SpatialPolygonsDataFrame of the boundary to be analyzed
#' @param strips A SpatialPolygonsDataFrame that is composed of one or more strips of which the crops reside
#' @param crop.name Optional param, if specified the single crop will be displayed
map_yield <- function(d, area.harvested, boundary, strips, crop.name = "") {
  # Clean data
  d <- d[!(is.na(d$GrainWeightWet) | d$GrainWeightWet==""), ]
  if(crop.name != "" && crop.name %in% d$Crop)
  {
    d <- d[(d$Crop == crop.name), ]
  }
  
  # Append a column with per area yield
  d['Yield'] <- d$GrainWeightWet / area.harvested
  
  # Print shapefile info
  #ogrInfo(polygon.shpdir, polygon.shpname)
  
  # Read boundary shapefile
  #boundary <- readOGR(polygon.shpdir, polygon.shpname)
  
  # Convert csv gain mass data to spatial points and specify the datum
  dsp <- SpatialPoints(d[,6:5], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
  dsp <- SpatialPointsDataFrame(dsp, d)
  
  # Project spatial data to UTM Zone 11N
  TA <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  dta <- spTransform(dsp, TA)
  stripsta <- spTransform(strips, TA)
  boundaryta <- spTransform(boundary, TA)
  
  # Create proximity polygons, or nearest neighbor interpolation
  v <- voronoi(dta)
  
  # Confine to given boundary
  ca <- aggregate(stripsta)
  vca <- intersect(v, ca)
  
  # Rasterize it
  r <- raster(stripsta, res=5)
  vr <- rasterize(vca, r, 'Yield')
  
  # Control extent (from: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS3_MakingMaps_part1_mappingVectorData.html)
  # Change these parameters
  #scale.parameter = 1.5  # scaling paramter. less than 1 is zooming in, more than 1 zooming out. 
  #xshift = -0.1  # Shift to right in map units. 
  #yshift = 0.2  # Shift to left in map units. 
  #original.bbox = boundaryta[1]@bbox  # Pass bbox of your Spatial* Object. 
  #
  #edges = original.bbox
  #edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1,]) + xshift
  #edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2,]) + yshift
  
  bbox = boundaryta[1]@bbox 
  
  # Inverse distance weighted interpolation
  gs <- gstat(formula = Yield~1, locations=dta)
  idw <- interpolate(r, gs)
  idwr <- mask(idw, vr)
  plot(boundaryta)
  plot(idwr, col = grey.colors(n = 8, 0, 1), add = T)
  
  # Add points and polygons to figure
  points(dta, col=c("white", "chartreuse", "blue", "yellow", "red", "green", "sandybrown")[dta$Crop])
  lines(stripsta)
  
  # Print legend
  legend(x="topright", legend = levels(dta$Crop), col=c("white", "chartreuse", "blue", "yellow", "red", "green", "sandybrown"), pch=1)
  title("Cook East yield for HY2013")
  mtext(paste("Area harvested = ", toString(area.harvested), ", Crop = ", crop.name, " units = g/m2"))
}

#' Creates a IDW map of yield values from grain weight
#' @param data.path A string of the path to csv file with grain mass data in WGS84 datum and columns: FieldID	Column	Row2	ID2	Latitude	Longitude	Year	SampleID	Crop	GrainWeightWet
#' @param area.harvested A number for the area harvested in m2, grain mass will be divided by this number to get mass/area
#' @param polygon.shpdir A string for the relative folder containing a polygon ESRI shapefile
#' @param polygon.shpname A string for the name, without extension, of the polygon ESRI shapefile
#map_yield_path <- function(data.path, area.harvested, polygon.shpdir, polygon.shpname, crop.name = "") {
#  # Print shapefile info
#  ogrInfo(polygon.shpdir, polygon.shpname)
#  
#  # Read boundary shapefile
#  boundary <- readOGR(polygon.shpdir, polygon.shpname)
#  
#  map_yield(data.path, area.harvested, boundary)
#}

extract_georef_field_and_strip <- function(a.strips, b.strips, c.strips, polygons) {
  #b.fields <- polygons[polygons$Field %in% fields, ]
  #b.strips <- b.fields[b.fields$Strip %in% strips, ]
  strips <- polygons[
      (polygons$Field == "A" & polygons$Strip %in% a.strips) |
      (polygons$Field == "B" & polygons$Strip %in% b.strips) |
      (polygons$Field == "C" & polygons$Strip %in% c.strips), ]
}

# Set WD, define constants
setwd("C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CookEastPlantHandHarvest\\1999-2016\\Working\\R")
area.harvested <- 2.4384

# Read input data
strips <- readOGR("Working/CookEastStrips2013", "Field_Plan_Final")
boundary <- readOGR("Input/CookEastArea", "CafCookEastArea")
d <- read.csv("Input/HY2013GP_GrainWeightOnly_171012.csv", stringsAsFactors = TRUE)

# Original strips polygon of Cook East has area with no georef points (and no yield), so remove them
georeff.only <- raster::intersect(boundary, strips)

#GB
a.strips <- c(5)
b.strips <- c(1,2,3,4)
c.strips <- NULL
map_yield(d, area.harvested, georeff.only, extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "GB")

#SB
a.strips <- c(6)
b.strips <- NULL
c.strips <- NULL
map_yield(d,  area.harvested, georeff.only, extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "SB")

#SW
a.strips <- c(2,3,4)
b.strips <- c(5, 6)
c.strips <- NULL
map_yield(d, area.harvested, georeff.only,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "SW")

#WW
a.strips <- c(1)
b.strips <- NULL
c.strips <- c(1, 2, 3, 4, 5, 6, 7, 7, 8)
map_yield(d, area.harvested, georeff.only, extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "WW")