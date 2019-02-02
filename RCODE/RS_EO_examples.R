
## SIBECOL 2019
## Remote Sensing / Earth Observation workshop
## 
## João Gonçalves / CIBIO-InBIO, U Porto
##
##

## ___________________________________________________________________________________ ##
##
## PART I - MODIStsp for downloading and pre-processing MODIS time series data
##
## ___________________________________________________________________________________ ##


# https://github.com/ropensci/MODIStsp

# install.packages("MODIStsp")
# 
# library(devtools)
# install_github("ropensci/MODIStsp")

library(MODIStsp)

MODIStsp()


## ___________________________________________________________________________________ ##
##
## PART II - A simple example of MODIS data processing in R using the raster package
##
## ___________________________________________________________________________________ ##


library(raster)
library(sp)


# Create a list of file paths to create a RasterStack object
modTS_FilePaths <- list.files("./DATA/MODIS_Valencia", pattern = ".tif$", full.names = TRUE)

# MOD13Q1 16-day 250m NDVI time series from 2001 to 2016
modTS <- stack(modTS_FilePaths)

# Print the RasterStack object metadata
print(modTS)

# Generate dates for the composites with a 16-day interval
dates <- do.call("c", lapply(2001:2016, function(i) {
  start <- as.Date(paste0(i, "-01-01"))
  end <- as.Date(paste0(i, "-12-31"))
  seq(start, end, 16)
}))

# Modify the names of each NDVI image layer
names(modTS) <- paste("NDVI",dates,sep="_")

# Plot six images in the series (post-fire)
plot(modTS[[267:272]])


## ---------------------------------------------------------------------------- ##


# Selected point for Andilla (Valencia)
xyAnd <- matrix(c(696042, 4405356),1,2,byrow = TRUE)
ptAnd <- SpatialPoints(xy, crs(modTS))

# Extract the data for the selected point
# Returns a matrix with ncols = nlayers and nlines = npoints
ndviPtAnd <- extract(modTS, ptAnd)
ndviTS <- ts(ndviPtAnd[1, ], start = c(2001,1), end=c(2016,23), frequency=23)

# Plot the original time series for the point
plot(ndviTS, main="NDVI 2001-2016 / Andilla (Valencia)")

# Seasonal and Trend Decomposition by Loess
# Additive decomposition of a time series: seasonal + trend + remainder
# 
ndviSTL <- stl(ndviTS, s.window="periodic", robust = TRUE)

# Plot the three components
plot(ndviSTL, main="STL NDVI 2001-2016 / Andilla (Valencia)")


## ---------------------------------------------------------------------------- ##


# Calculate the NDVI annual average
modTS_avg <- stackApply(modTS, indices = rep(1:16,each=23), fun = mean)

# print the output RasterStack object
print(modTS_avg)

# Plot the data for years 2008 to 2016
plot(modTS_avg[[8:16]])

# Extract data to a selected point
ndviAvg <- extract(modTS_avg, ptAnd)

# Plot the point data
plot(ndviAvgTS)
plot(x=2001:2016, y=ndviAvg[1,], type="l")

# Calculate the inter-annual average
modAvgTotal <- calc(modTS_avg, fun = mean)

# Calculate the anomalies for all years: yr_i - inter-annual mean
# Raster package allows for direct use of algebraic ops
modAvgAnom <- modTS_avg - modAvgTotal

# Plot 2013 - 2015 annual average anomalies
plot(modAvgAnom[[13:15]])

# Extract data for Andilla point
ndviAnom <- extract(modAvgAnom, ptAnd)

# Plot the annual anomaly values
plot(x=2001:2016, y=ndviAnom[1,], type="l", 
     xlab="Year", ylab="NDVI annual average anomaly", 
     main="NDVI annual average anomaly 2001-2016 / Andilla (Valencia)")
points(x=2001:2016, y=ndviAnom[1,])
abline(h = 0, lty=2)


