##################### Fonction qui fait l'estimation de pie ############################################
mc.pi <- function(n){

  zc <- 0                                       # Compteur pour obtenir l'aire du cercle
  pie <- 0                                      # On initialise une variable pie en 0
  
  for (j in 1:n) {                              # Ici on tire les points aléatoires
    
    x <- runif(1)                               # On génère des nombres aléatoires dans les variables temporaires 
    y <- runif(1)
    if((x**2 + y**2) < 1) zc <- zc + 1
  }
  
  pie <- ((4*zc) / n)                           # On calcule pie (consulter le rapport pour obtenir les détails)
  
  return(pie)
}

###################### Fonction qui fait l'estimation de pie et renvoie un graph ##############################
mc2.pi <- function(n){

  zc <- 0                                       # Compteur pour obtenir l'aire du cercle
  pie <- 0                                      # On initialise une variable pie en 0
  xci <- vector()                               # On garde les points du cercle pour faire le graphique
  yci <- vector() 
  xca <- vector()                               # On garde les points du carré pour faire le graphique
  yca <- vector()
  
  for (j in 1:n) {                              # On fait Monte Carlo
    
    xt <- runif(1)   
    yt <- runif(1)
    
    if((xt**2 + yt**2) < 1){                    # Si c'est vrai alors le point est dans le cercle
      zc <- zc + 1                              # Donc le compteur augmente
      xci[j] <- xt
      yci[j] <- yt
    }
    else{                                       # Si ce n'est pas vrai alors le point est dehors
      xca[j] <- xt
      yca[j] <- yt
    }
  }
  
  pie <- ((4*zc) / n)                           # On calcule pie
  
  plot(x = xci, y = yci, cex = 0.4, col = 'red', xlab = 'x', ylab = 'y')
  points(x = xca, y = yca, col = 'blue', cex = 0.4)
  
  return(pie)
  
}