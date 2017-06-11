library(dplyr)

bbox <- gEnvelope(uk_polygon)

draw_hexmap <- function(shape_spatdf,hex_size,hex_orientation="flat",buildMap=F,tileProvider=providers$OpenStreetMap,cullingMethod="intersects"){
  
  #check if bounding box exists - if not make it
  if("bbox" %in% slotNames(shape_spatdf)==F){
    stop("No bbox element found in shapefile")
  }  
  
     
     
  #compute size of the grid and the starting coordinate for the grid
  n_row <-  abs(round(shape_spatdf@bbox["y","max"],0)-round(shape_spatdf@bbox["y","min"],0))/hex_size#*10*size
  n_col <-  abs(round(shape_spatdf@bbox["x","max"],0)-round(shape_spatdf@bbox["x","min"],0))/hex_size#*10*size
  
  box_width <- abs(shape_spatdf@bbox["x","max"]-shape_spatdf@bbox["x","min"])
  box_height <- abs(shape_spatdf@bbox["y","max"]-shape_spatdf@bbox["y","min"])
  
  startPoint <- c(shape_spatdf@bbox["x","min"]-(box_width/2),shape_spatdf@bbox["y","min"]-(box_height/2))#*10*size
  
  #build base hexgrid
  #debug(buildHexGrid)
  hex_grid <- buildHexGrid(n_row,n_col,hex_size,startPoint,hex_orientation)
  
  #get rid of hexagons that don't intersect with the chosen shapefile
  if(cullingMethod=="intersects")
    final_hexagons <- hex_grid[rgeos::gIntersects(shape_spatdf,hex_grid,byid=T)[,1],]
  else
    final_hexagons <- hex_grid[rgeos::gContains(shape_spatdf,hex_grid,byid=T)[,1],]
  
  #final_hexagons <- hex_grid
  
  if(buildMap==T){
  #plotleaflet map of clipped hexgrid
  leaflet() %>% 
    addProviderTiles(tileProvider) %>%
    addPolygons(data=final_hexagons,fillColor = "red",weight = 0.1) #%>%
    #addPolylines(data=shape_spatdf)
    
  }else{
    return(final_hexagons)
  }
}

#"CartoDB.Positron"
#debug(draw_hexmap)
#draw_hexmap(uk_shape,10,buildMap = T)

#boundingboxes <- uk_shape@bbox[[1]]
#byID=T