library(sp)
library(rgdal)
library(dismo)
library(gstat)

#' Creates a IDW map of yield values from grain weight
#' @param data.path A string of the path to csv file with grain mass data in WGS84 datum and columns: FieldID	Column	Row2	ID2	Latitude	Longitude	Year	SampleID	Crop	GrainWeightWet
#' @param area.harvested A number for the area harvested in m2, grain mass will be divided by this number to get mass/area
#' @param polygon.shpdir A string for the relative folder containing a polygon ESRI shapefile
#' @param polygon.shpname A string for the name, without extension, of the polygon ESRI shapefile
map_yield <- function(data.path, area.harvested, polygon.shpdir, polygon.shpname, crop.name = "") {
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
  ogrInfo(polygon.shpdir, polygon.shpname)
  
  # Read boundary shapefile
  boundary <- readOGR(polygon.shpdir, polygon.shpname)
  
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

map_yield("Input/HY2013GP_GrainWeightOnly_171012.csv", 2.4384, "Input/CookEastArea", "CafCookEastArea")

## ============= PLAYGROUND =================
#d <- read.csv("Input/HY2013GP_GrainWeightOnly_171012.csv")
## clean nulls
#d <- d[!(is.na(d$GrainWeightWet) | d$GrainWeightWet==""), ]
#d['WetYield1.828m2'] <- d$GrainWeightWet / 1.828
#d['WetYield2.4384m2'] <- d$GrainWeightWet / 2.4384
#
## --- from: https://www.nceas.ucsb.edu/scicomp/usecases/ReadWriteESRIShapeFiles ---
#
## optionally report shapefile details
#ogrInfo("Input/Polylines", "CookEastBoundary")
#ogrInfo("Input/CookEastArea", "CafCookEastArea")
#
## read in shapefiles
##CookEastBoundary <- readOGR("Input/Polylines", "CookEastBoundary")
#CookEastBoundary <- readOGR("Input/CookEastArea", "CafCookEastArea")
#
## note that readOGR will read the .prj file if it exists
#print(proj4string(CookEastBoundary))
#
## generate map
#plot(CookEastBoundary, axes=TRUE, border="gray")
## -----------------
#
## --- from: http://rspatial.org/analysis/rst/4-interpolation.html ---
#
#dsp <- SpatialPoints(d[,6:5], proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
#dsp <- SpatialPointsDataFrame(dsp, d)
#
#cuts <- c(0,200,400,600,800,100,1200,1400)
#blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
#pols <- list("sp.polygons", CookEastBoundary)
#spplot(dsp, 'GrainWeightWet', cuts=cuts, col.regions=blues(5), sp.layout=pols, pch=20, cex=2)
#
#TA <- CRS("+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#dta <- spTransform(dsp, TA)
#cata <- spTransform(CookEastBoundary, TA)
#
#plot(cata)
#v <- voronoi(dta)
#plot(v)
#
#ca <- aggregate(cata)
#vca <- intersect(v, ca)
#spplot(vca, 'GrainWeightWet', col.regions=rev(get_col_regions()))
#
#r <- raster(CookEastBoundary, res=5)
#vr <- rasterize(vca, r, 'GrainWeightWet')
#plot(vr)
#
#gs <- gstat(formula = GrainWeightWet~1, locations=dta)
#idw <- interpolate(r, gs)
#idwr <- mask(idw, vr)
#plot(idwr)
#
## -------------------------
