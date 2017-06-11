#builds a hexagon polygon
buildPointyTopHexagon <- function(startingPoint,center, size){
  
  points <- data.frame(matrix(0,nrow = 6,ncol=2))
  
  for(i in 1:6){
    angle_deg <- 60 * (i-1) + 30
    
    angle_rad = pi / 180 * angle_deg
    
    
    points[i,1] <- startingPoint[1] + (center[1] + size * cos(angle_rad))
    points[i,2] <- startingPoint[2] + (center[2] + size * sin(angle_rad))
  }
  
  colnames(points) <- c("Lat","Lng")
  
  return((points))
}

#builds a hexagon polygon
buildFlatTopHexagon <- function(startingPoint,center, size){
  
  points <- data.frame(matrix(0,nrow = 6,ncol=2))
  
  for(i in 1:6){
    angle_deg <- 60 * (i-1)
    
    angle_rad = pi / 180 * angle_deg
    
    
    points[i,1] <- startingPoint[1] + (center[1] + size * cos(angle_rad))
    points[i,2] <- startingPoint[2] + (center[2] + size * sin(angle_rad))
  }
  
  colnames(points) <- c("Lat","Lng")
  
  return((points))
}


################## Build Grid ####################

library(rgdal)
library(leaflet)
library(rgeos)
library(geosphere)
library(alphahull)
library(data.table)
library(dplyr)
library(tidyr)
#install.packages("alphahull")


#load in shape data
shapes_data <- readOGR(dsn="Shapes/.")

#compute bounding box
#centroids <- gCentroid(shapes_data,byid=T)

#centroids@bbox

size <- 1
n_row <-  abs(round(shapes_data@bbox["y","max"],0)-round(shapes_data@bbox["y","min"],0))*10*size
n_col <-  abs(round(shapes_data@bbox["x","max"],0)-round(shapes_data@bbox["x","min"],0))*10*size

startPoint <- c(round(shapes_data@bbox["x","min"],0),round(shapes_data@bbox["y","min"],0))*10*size



#Build the hexgrid of the shapes bounding box

buildHexGrid <- function(n_row,n_col,size,orientation="pointy"){
#calculate number of required hexagons  
n_hexagons <- n_row*n_col

poly_list <- vector("list",n_hexagons)

#build bottom row
for(z in 1:(n_row)){
  for(x in 1:(n_col)){
    
    outer_radius <- size 
    inner_radius <- outer_radius * (sqrt(3)/2) #(sqrt(3)/2) * outer_radius
    
    
    if(orientation=="pointy"){
    dat <- data.frame(buildPointyTopHexagon(startPoint,c(
      (x + z * 0.5 - z %/% 2) * (inner_radius*2),
      z*(outer_radius*1.5),
      ((z-1)*n_col)+x),
      size),
      ((z-1)*n_col)+x
    )
    }else if(orientation=="flat")
    {
      dat <- data.frame(buildFlatTopHexagon(startPoint,c(
        z*(outer_radius*1.5),
        (x + z * 0.5 - z %/% 2) * (inner_radius*2),
        ((z-1)*n_col)+x),
        size),
        ((z-1)*n_col)+x
      )
    }
    
    colnames(dat) <- c("Lat","Lng","ID")
    
    poly_list[[((z-1)*n_col)+x]]<- dat
    
  }
}

positions <- do.call("rbind",poly_list)

positions$Lat <- positions$Lat/10*size
positions$Lng <- positions$Lng/10*size

#build hexagon spatial polygons 
df_to_spp <- positions %>%
  group_by(ID) %>%
  do(poly=select(., Lat, Lng) %>%Polygon()) %>%
  rowwise() %>%
  do(polys=Polygons(list(.$poly),.$ID)) %>%
  {SpatialPolygons(.$polys)}

#plot the grid to check
#plot(df_to_spp)

df_to_spp <- SpatialPolygonsDataFrame(df_to_spp,data = data.frame(id=1:length(df_to_spp),stringsAsFactors = F))

return(df_to_spp)
}

test <- buildHexGrid(10,10,1,"flat")
plot(test)


########################################################################

#Create a convex hull of the UK to clip hexagons


#####
#calculate a concave hull based on postcode sector
#load postcodes data
postcodes <- fread("postcodes.csv",stringsAsFactors = F)
postcode_latlng <- postcodes[,c("Postcode","In Use?","Latitude","Longitude")]

postcode_latlng <- dplyr::filter(postcode_latlng,`In Use?`=="Yes")

postcode_latlng <- postcode_latlng[!duplicated(postcode_latlng),]

#postcode_latlng$Postcode_sector <- strsplit(postcode_latlng$Postcode,split = " ",fixed=T)[,1]#substr(postcode_latlng$Postcode,1,nchar(postcode_latlng$Postcode)-2) 
postcode_latlng$postcode_area_district <- do.call("rbind",strsplit(postcode_latlng$Postcode,split = " ",fixed=T))[,1]

#average lat longs in postcode sectors
av_lat <- postcode_latlng %>%
  group_by(postcode_area_district) %>%
  summarise(Latitude=mean(Latitude))

av_lng <- postcode_latlng %>%
  group_by(postcode_area_district) %>%
  summarise(Longitude=mean(Longitude))


#merge av lat and av lng
postcode_sector_latlng <- left_join(av_lat,av_lng,by=c("postcode_area_district"="postcode_area_district"))

postcode_sector_latlng <- postcode_sector_latlng[!duplicated(postcode_sector_latlng),]

coords <- postcode_sector_latlng[,c("Longitude","Latitude")]
coords <- coords[!duplicated(coords),]

convex <- alphahull::ahull(coords,alpha = 0.9)
convex <- ah2sp(convex)

#plot the final map
leaflet() %>% 
  addTiles() %>%
  addPolygons(data=convex)

#4800/nrow(postcode_sector_latlng)

############################################################

#work out what hexagons fall within the shape and delete all others

hex_grid <- buildHexGrid(n_row,n_col,size,"flat")

final_hexagons <- hex_grid[rgeos::gContains(uk_shape,hex_grid,byid=T)[,1],]

hexagon_centroids <- as.data.frame(centroid(final_hexagons))
colnames(hexagon_centroids) <- c("Longitude","Latitude")

#find closest postcode and match to hexagon
distance_matrix <- distm(postcode_sector_latlng[,c("Longitude","Latitude")],hexagon_centroids)

#find min in row
postcode_sectors <- data.frame(postcode_sector=postcode_sector_latlng$postcode_area_district[as.matrix(apply( t(distance_matrix), 1, which.min))],
                               id=final_hexagons@data$id,
                               stringsAsFactors = F)

final_hexagons@data <-postcode_sectors


#remove dupe postcodes
#final_hexagons <- final_hexagons[!duplicated(final_hexagons@data$postcode_sector),]

#############################################################

library(htmltools)


#plot the final map
leaflet() %>% 
  addTiles("CartoDB.Positron") %>%
  addPolygons(data=final_hexagons,fillColor = "red",weight = 0.1)#label=~htmlEscape(postcode_sector))
  

plot(final_hexagons)


library(ggplot2)

ggpoints <- fortify(final_hexagons, region = "id")

ggpoints <- merge(ggpoints,final_hexagons@data,by="id")

ggplot(ggpoints,aes(x=long, y=lat, group = group)) +
  geom_polygon()
