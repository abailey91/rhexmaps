
#https://bl.ocks.org/timcraft/5867015 - election map example in D3 - looks like grid had been hard coded



drawNationalHexGrid <- function(n_row,ncol,hex_size){
  
  
  
}

grid_id <- read.csv("national_grid.csv",stringsAsFactors = F)
grid_id$row <- ((grid_id$x-1)*13)+grid_id$y  


national_grid <- buildHexGrid(n_row = 7,n_col = 13,size=1,orientation = "flat")

national_grid@data$national_grid_ref <- grid_id$id
national_grid@data$national_grid_ref[10] <- "NA"

national_grid_fin <- national_grid[!(national_grid@data$national_grid_ref==""),]

#library(leaflet)
library(htmltools)
leaflet() %>%
  addTiles() %>%
  addPolygons(data=national_grid_fin,fillColor = national_grid_fin@data$Color,label=htmlEscape(national_grid_fin@data$national_grid_ref))




######## postcode mapping

postcodes$postcode_sector <- substr(postcodes$Postcode,1,nchar(postcodes$Postcode)-2)

postcodes <- dplyr::filter(postcodes,`In Use?`=="Yes")

#length(unique(postcodes$postcode_sector))

#average lat longs in postcode sectors
av_lat <- postcodes %>%
  group_by(postcode_sector) %>%
  summarise(Latitude=mean(Latitude))

av_lng <- postcodes %>%
  group_by(postcode_sector) %>%
  summarise(Longitude=mean(Longitude))

#merge av lat and av lng
postcode_sector_latlng <- left_join(av_lat,av_lng,by=c("postcode_sector"="postcode_sector"))

#remove odd zero points
postcode_sector_latlng <- dplyr::filter(postcode_sector_latlng,Longitude!=0, Latitude!=0)

#bottom left point
min_lat <- min(postcode_sector_latlng$Latitude)
min_lng <- min(postcode_sector_latlng$Longitude)

min_point <- data.frame(lng=min_lng,lat=min_lat,stringsAsFactors = F)

#find point closest to bottom left of bounding box
distances <- distm(postcode_sector_latlng[,c("Longitude","Latitude")],min_point)
min_index <-which.min(distances)#,arr.index=T)


#postcode_sector_latlng$lat_distance <- round(postcode_sector_latlng$Latitude-postcode_sector_latlng$Latitude[min_index]/postcode_sector_latlng$Latitude[min_index],1)
#postcode_sector_latlng$lng_distance <- round(postcode_sector_latlng$Longitude-postcode_sector_latlng$Latitude[min_index]/postcode_sector_latlng$Longitude[min_index],1)

postcode_sector_latlng$lat_distance <- round(postcode_sector_latlng$Latitude,1)
postcode_sector_latlng$lng_distance <- round(postcode_sector_latlng$Longitude,1)

plot(postcode_sector_latlng[,c("lng_distance","lat_distance")])


#plot(hexbin(postcode_sector_latlng[,c("lng_distance","lat_distance")],xbins = 100))


size <- 0.05

#check for a point circle collision
postcode_sector_latlng$circle_distance <- sqrt(
  ((postcode_sector_latlng$Longitude[min_index]-postcode_sector_latlng$Longitude)^2) +
    ((postcode_sector_latlng$Latitude[min_index]-postcode_sector_latlng$Latitude)^2)
)

postcode_sector_latlng 


#grid ref idea to postcodes
postcodes$national_grid_ref <- substr(postcodes$GridRef,1,2)








#filter dataset to a single grid square
filtered_postcodes <- dplyr::filter(postcodes,national_grid_ref=="SU")


filtered_postcodes$Postcode_sector <- substr(filtered_postcodes$Postcode,1,nchar(filtered_postcodes$Postcode)-2) 

#average lat longs in postcode sectors
av_lat <- filtered_postcodes %>%
  group_by(Postcode_sector) %>%
  summarise(Latitude=mean(Latitude))

av_lng <- filtered_postcodes %>%
  group_by(Postcode_sector) %>%
  summarise(Longitude=mean(Longitude))


#merge av lat and av lng
postcode_sector_latlng <- left_join(av_lat,av_lng,by=c("Postcode_sector"="Postcode_sector"))

ggplot(data=postcode_sector_latlng,aes(x=Longitude,y=Latitude)) + geom_point()

#round latlongs to nearest

postcode_sector_latlng$Latitude <- round(postcode_sector_latlng$Latitude,2)
postcode_sector_latlng$Longitude <- round(postcode_sector_latlng$Longitude,2)


plot(hexbin(postcode_sector_latlng[,c("Longitude","Latitude")]))


max(distm)
