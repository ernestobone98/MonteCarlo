reg_poly <- function (n, r = 1) {
  px <- vector(length = n+1)                         # Coordonnées en x
  py <- vector(length = n+1)                         # Coordonnées en y
  an <- 2*pi/n                                       # Il faut travailler avec radians
  
  for (i in 1:(n+1)) {
    ad <- cos(an*(i-1)/2)*r                          # Pour les detailles consulter le rapport
    L <- 2*sqrt( r**2 - ad**2 )
    px[i] = L * cos((an/2)*(i-1)) ## x               # On fait la convertion de radians à degrés
    py[i] = L * sin((an/2)*(i-1)) ## y
  }
  # print(px)
  # print(py)
  plot(px,py, type = 'n')
  polygon(px,py)
  # text(px, py, 1:(n+1))
}