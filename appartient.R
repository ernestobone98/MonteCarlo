appartient <- function(points, polygone){
  
  bo <- matrix(nrow = 2, ncol = 2, dimnames=list(c("min", "max"), c("x","y")))              # Matrice pour stocker mins et maxs 
  bo[,] <- c(polygone[1,1],0,polygone[1,2],0)
  pin <- vector()
  
  for (i in 1:length(polygone[,1])) {                                                       # On obtiens les mins et maxs
    # du polygone
    if(bo[1,1] > polygone[i,1]) bo[1,1] <- polygone[i,1]
    if(bo[2,1] < polygone[i,1]) bo[2,1] <- polygone[i,1]
    
    if(bo[1,2] > polygone[i,2]) bo[1,2] <- polygone[i,2]
    if(bo[2,2] < polygone[i,2]) bo[2,2] <- polygone[i,2]
  }
  
  for (j in 1:length(points[,1])) {                                                         # Consulter le rapport
    
    if (points[j,1] < bo[1,1] || points[j,1] > bo[2,1] || points[j,2] < bo[1,2] || points[j,2] > bo[2,2]) pin[j] <- FALSE
    else pin[j] <- TRUE
    
  }
  print(pin)
  
}