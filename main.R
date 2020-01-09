mcpro.pi <- function(){
  ##################### On fait Monte Carlo plusieurs fois pour remplir la matrice PIE ###########################
  t <- 50
  p <- 5
  j <- 1:p
  n <- 10**j
  zc <- 0                                             # Compteur pour obtenir la zone du cercle
  PIE <- matrix(nrow = t, ncol = p)                   # il faut mettre les estimations de pie
  ro <- 1                                             # variable pour remplir la matrice (lignes)
  col <- 1                                            # variable pour remplir la matrice (colonnes)
  tE <- vector(mode = "double", length = p)
  
  for (col in j) {                                    # On remplit la matrice avec les valeurs estimées
    start_time <- Sys.time()                          # Variable qui nous aidera à calculer le temps moyen
    while (ro <= t) {
      
      for (i in 1:n[col]) {                           # Ici on tire les points aleatoires (Monte Carlo)
        
        x <- runif(1)
        y <- runif(1)
        
        if((x**2 + y**2) < 1) zc <- zc + 1            # Si c'est vrai alors le point est dans le cercle
      }
      
      PIE[ro,col] <- ((4*zc) / n[col])                # On stock le valeur de pie obtenue dans la matrice
      zc <- 0                                         # Il faut réinisialiser le compteur
      ro <- ro + 1                                    # On se deplace à la prochaine ligne
    }
    end_time <- Sys.time()
    tE[col] <- (end_time - start_time) / t            # On calcule le temps moyen et on le stock dans tE
    ro <- 1                                           # Il faut réinisialiser la valeur des lignes
    
  }
  cat("---------------- Matrice des estimations de pi ---------------------\n")
  print(PIE)
  cat("\n")
  
  
  ##################### Maintenant On va calculer l'erreur relative ###########################
  
  ERR <- matrix(nrow = t, ncol = p)
  for (i in 1:t) {
    for (k in j) {
      ERR[i,k] <- abs(PIE[i,k]/pi - 1)
    }
  }
  
  par(mfrow=c(1,2),mar=c(4,4,2,2)+0.1)
  boxplot(ERR, main='Erreur relative sur PI', log='y', xlab='#points', ylab='Rel. Error')
  plot(10**j, tE, type='b', main='Temps moyen d\'une simulation', log='x', xlab='#points', ylab='Time')
  cat("---------------- Matrice des erreurs relatives ---------------------\n")
  print(ERR)
  cat("\n")
  cat("---------- Temps moyen de calcul de pi avec n = 10**j ---------------\n")
  print(tE)
  
}