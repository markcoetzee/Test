
library(leaflet)

#Load library
library(raster)
library(rgdal)

library('sf')

# Load shapefile
shapeName <- read_sf("income2013.shp")
shapeName <- read_sf("age-and-income-in-2013-by-talb2013.shp")

plot(shapeName)

plot(st_geometry(shapeName["Median_age"]))
plot(st_geometry(nc)[1], col = 'red', add = TRUE)

library(readxl)

household_income_2013 <- read_xls("quickstats-income-tables.xls", sheet = 29, skip = 7)[-c(1:2),]
colnames(household_income_2013) <- c("Territorial Authority Area", colnames(household_income_2013[-1]))

household_income_2013


