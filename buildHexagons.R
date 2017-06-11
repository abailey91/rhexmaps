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
