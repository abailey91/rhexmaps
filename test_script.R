
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
uk_polygon <- draw_hexmap(shape_spatdf = shapes_data_gb,hex_size = 0.1,hex_orientation = "pointy",buildMap = F,tileProvider = providers$CartoDB.Positron,cullingMethod = "intersects")

#calculate centroids of hexagons
centroids <- as.data.frame(centroid(uk_polygon))
colnames(centroids) <- c("Longitude","Latitude")



#loop through hexagons and postcodes to build a sum of popupulation in each hexagon
population <- data.frame(matrix(0,nrow=nrow(centroids),ncol=1))


postcodes$Population[is.na(postcodes$Population)] <- 0

sum_population <- postcodes %>%
  group_by(postcode_sector) %>%
  summarise(Population=sum(Population))

postcode_sector_latlng <- left_join(postcode_sector_latlng,sum_population,by=c("postcode_sector"="postcode_sector"))

postcodes_spatial <- SpatialPointsDataFrame(postcode_sector_latlng[,c("Longitude","Latitude")],data = postcode_sector_latlng)

proj4string(postcodes_spatial)  <- proj4string(uk_polygon) 


for(i in 1:nrow(centroids)){
    population[i,1] <- sum(postcode_sector_latlng$Population[rgeos::gContains(uk_polygon[i,1],postcodes_spatial,byid=T)])
}

uk_polygon@data$Population <- population[,1]

binpallete <- colorNumeric(palette = "Greens",uk_polygon$Population)

leaflet() %>%
  addTiles("CartoDB.Positron") %>%
  addPolygons(data=uk_polygon,fillColor =~binpallete(Population),weight=0.01,fillOpacity = 1,label=htmlEscape(~format(Population,big.mark = ","))) %>%
  addMarkers(data=postcode_sector_latlng,lng=~Longitude,lat=~Latitude,label=htmlEscape(~postcode_sector),clusterOptions = markerClusterOptions(),group = "Postcode Sectors") %>%
  addLayersControl(
    overlayGroups=c("Postcode Sectors"),
    options = layersControlOptions(collapsed = FALSE)
  )


  
  
  
  


