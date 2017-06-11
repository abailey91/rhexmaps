
source("buildHexagons.R")
source("buildHexGrid.R")
source("draw_hexmap.R")


library(rgdal)
library(leaflet)
library(rgeos)
library(geosphere)
library(alphahull)
library(data.table)
library(dplyr)
library(tidyr)


#load in shape data
shapes_data <- readOGR(dsn="UKFullExtent/.")

shapes_data@data$country <- "GreatBritain"

#merge 3 shapes into one
shapes_data_gb <- gUnaryUnion(shapes_data,id=shapes_data@data$country)

#draw hexagon map
#debug(draw_hexmap)
draw_hexmap(shape_spatdf = shapes_data_gb,hex_size = 0.05,hex_orientation = "pointy",buildMap = T,tileProvider = providers$CartoDB.Positron,cullingMethod = "contains")

