creer_polygone <- function (x,y) {
  p <- matrix(c(x, x[1], y, y[1]), ncol=2,dimnames=list(c(), c("x","y")))
  plot(p[,1], p[,2], xlab = "x", ylab = "y", type = "l", col="black") 
  polygon(p)
  print(p)
}

poly_surprise <- function(){
  x <- c(0,0,9,11,11,9,8,11,9,6,3,3,8,9,9,8,2,2)
  y <- c(0,12,12,10,7,5,5,0,0,5,5,7,7,8,9,10,10,0)
  creer_polygone(x,y)
}