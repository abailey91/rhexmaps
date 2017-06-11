buildHexGrid <- function(n_row,n_col,size,startPoint,orientation="pointy"){
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
  
  positions$Lat <- positions$Lat#/size
  positions$Lng <- positions$Lng#/size
  
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
