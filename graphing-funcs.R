# Author: Bryan Carlson
# Contact: bryan.carlson@ars.usda.gov
# Purpose: Mapping and utility functions to aid in QC of hand harvested yield data

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

#' Extracts polygons that correspond to specified fields and strips
#' @param a.strips A list of numbers that correspond to the strip ID in field A that are to be included
#' @param b.strips A list of numbers that correspond to the strip ID in field B that are to be included
#' @param c.strips A list of numbers that correspond to the strip ID in field C that are to be included
#' @param polygons A SpatialPolygonsDataFrame that contains all strips and fields in CookEast
extract_georef_field_and_strip <- function(a.strips, b.strips, c.strips, polygons) {
  strips <- polygons[
    (polygons$Field == "A" & polygons$Strip %in% a.strips) |
      (polygons$Field == "B" & polygons$Strip %in% b.strips) |
      (polygons$Field == "C" & polygons$Strip %in% c.strips), ]
}