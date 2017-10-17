library(sp)
library(rgdal)
library(dismo)
library(gstat)

#' Creates a IDW map of yield values from grain weight
#' @param data.path A string of the path to csv file with grain mass data in WGS84 datum and columns: FieldID	Column	Row2	ID2	Latitude	Longitude	Year	SampleID	Crop	GrainWeightWet
#' @param area.harvested A number for the area harvested in m2, grain mass will be divided by this number to get mass/area
#' @param polygon A SpatialPolygonsDataFrame of the boundary to be analyzed
map_yield <- function(data.path, area.harvested, boundary, crop.name = "") {
  # Read data and clean nulls
  d <- read.csv(data.path, stringsAsFactors = TRUE)
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
  boundaryta <- spTransform(boundary, TA)
  
  # Create proximity polygons, or nearest neighbor interpolation
  v <- voronoi(dta)
  
  # Confine to given boundary
  ca <- aggregate(boundaryta)
  vca <- intersect(v, ca)
  
  # Rasterize it
  r <- raster(boundaryta, res=5)
  vr <- rasterize(vca, r, 'Yield')
  
  # Inverse distance weighted interpolation
  gs <- gstat(formula = Yield~1, locations=dta)
  idw <- interpolate(r, gs)
  idwr <- mask(idw, vr)
  plot(idwr, col = grey.colors(n = 8, 0, 1))
  #, ColorRampPalette=colorRampPalette("white", "black")
  # Plot points on top of IDW raster
  
  
  points(dta, col=c("white", "chartreuse", "blue", "black", "red", "green", "sandybrown")[dta$Crop])
  
  # Print legend
  legend(x="topright", legend = levels(dta$Crop), col=c("white", "chartreuse", "blue", "black", "red", "green", "sandybrown"), pch=1)
  title("Cook East yield for HY2013")
  mtext(paste("Area harvested = ", toString(area.harvested)))
}

#' Creates a IDW map of yield values from grain weight
#' @param data.path A string of the path to csv file with grain mass data in WGS84 datum and columns: FieldID	Column	Row2	ID2	Latitude	Longitude	Year	SampleID	Crop	GrainWeightWet
#' @param area.harvested A number for the area harvested in m2, grain mass will be divided by this number to get mass/area
#' @param polygon.shpdir A string for the relative folder containing a polygon ESRI shapefile
#' @param polygon.shpname A string for the name, without extension, of the polygon ESRI shapefile
map_yield_path <- function(data.path, area.harvested, polygon.shpdir, polygon.shpname, crop.name = "") {
  # Print shapefile info
  ogrInfo(polygon.shpdir, polygon.shpname)
  
  # Read boundary shapefile
  boundary <- readOGR(polygon.shpdir, polygon.shpname)
  
  map_yield(data.path, area.harvested, boundary)
}

extract_georef_field_and_strip <- function(a.strips, b.strips, c.strips, polygons) {
  #b.fields <- polygons[polygons$Field %in% fields, ]
  #b.strips <- b.fields[b.fields$Strip %in% strips, ]
  strips <- polygons[
      (polygons$Field == "A" & polygons$Strip %in% a.strips) |
      (polygons$Field == "B" & polygons$Strip %in% b.strips) |
      (polygons$Field == "C" & polygons$Strip %in% c.strips), ]
}

setwd("C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CookEastPlantHandHarvest\\1999-2016\\Working\\R")

b <- readOGR("Working/CookEastStrips2013", "Field_Plan_Final")
boundary <- readOGR("Input/CookEastArea", "CafCookEastArea")

georeff.only <- raster::intersect(boundary, b)

#GB
a.strips <- c(5)
b.strips <- c(1,2,3,4)
c.strips <- NULL
map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "GB")

#SB
a.strips <- NULL
b.strips <- C(5)
c.strips <- NULL
map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "SB")

#SW
a.strips <- c(2,3,4)
b.strips <- c(5, 6)
c.strips <- NULL
map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "SW")

#WW
a.strips <- c(1)
b.strips <- NULL
c.strips <- c(1, 2, 3, 4, 5, 6, 7, 7, 8)
map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "WW")




map_yield_path("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384, "Input/CookEastArea", "CafCookEastArea")


## ============= PLAYGROUND =================
## --- Loading polygons in shp file ---
library(sp)
library(rgdal)
setwd("C:\\OneDrive\\OneDrive - Washington State University (email.wsu.edu)\\Projects\\CookEastPlantHandHarvest\\1999-2016\\Working\\R")
ogrInfo("Working/CookEastStrips2013", "Field_Plan_Final")
b <- readOGR("Working/CookEastStrips2013", "Field_Plan_Final")
boundary <- readOGR("Input/CookEastArea", "CafCookEastArea")
georeff.only <- raster::intersect(boundary, b)
georeff.A.only <- subset(georeff.only, Field == "A")
plot(georeff.A.only)
map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384, georeff.A.only, crop.name = "SW")
nb <- subset(b, Crop == "SB")
fields <- c("A", "B")
strips <- c(1, 2)
b.fields <- georeff.only[georeff.only$Field %in% fields, ]
b.strips <- b.fields[b.fields$Strip %in% strips, ]
fields.strips <- matrix(FALSE, nrow=8, ncol=3)
plot(extract_georef_field_and_strip(c("A", "C"), c(1, 2, 3, 4, 5, 6, 7, 8), georeff.only))

a.strips <- c(1)
b.strips <- NULL
c.strips <- c(1, 2, 3, 4, 5, 6, 7, 7, 8)

b.strips.a <- georeff.only[
  (georeff.only$Field == "A" & georeff.only$Strip %in% a.strips) |
  (georeff.only$Field == "B" & georeff.only$Strip %in% b.strips) |
  (georeff.only$Field == "C" & georeff.only$Strip %in% c.strips), ]

plot(extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only))

map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384,extract_georef_field_and_strip(a.strips, b.strips, c.strips, georeff.only), "WW")

plot(b.fields)
plot(b.strips)
plot(georeff.only)
