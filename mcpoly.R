mc.poly <-  function(n, polygone){
  
  bo <- matrix(nrow = 2, ncol = 2, dimnames=list(c("min", "max"), c("x","y")))              # Matrice pour stocker mins et maxs 
  bo[,] <- c(polygone[1,1],0,polygone[1,2],0)
  pin <- vector()
  dp <- matrix(ncol = 2)
  hp <- matrix(ncol = 2)
  cnt_hp <- 1
  cnt_dp <- 1
  
  for (i in 1:length(polygone[,1])) {                                                       # On obtiens les mins et maxs
    # du polygone
    if(bo[1,1] > polygone[i,1]) bo[1,1] <- polygone[i,1]
    if(bo[2,1] < polygone[i,1]) bo[2,1] <- polygone[i,1]
    
    if(bo[1,2] > polygone[i,2]) bo[1,2] <- polygone[i,2]
    if(bo[2,2] < polygone[i,2]) bo[2,2] <- polygone[i,2]
  }
  
  for (j in 1:n) {
    
    x <- runif(n = 1, min = bo[1,1], max = bo[2,1])
    y <- runif(n = 1, min = bo[1,2], max = bo[2,2])
    # print(x)
    # print(y)
    if (x < bo[1,1] || x > bo[2,1] || y < bo[1,2] || y > bo[2,2]){
      pin[j] <- FALSE
      hp[cnt_hp,1] <- x
      hp[cnt_hp,2] <- y
      cnt_hp <- cnt_hp + 1
    } 
    else{
      pin[j] <- TRUE
      dp[cnt_dp,1] <- x
      dp[cnt_dp,2] <- y
      cnt_dp <- cnt_dp + 1
    }
    
  }
  
  plot(dp, cex = 0.4, col = 'red', xlab = 'x', ylab = 'y')
  
  print(pin)
}