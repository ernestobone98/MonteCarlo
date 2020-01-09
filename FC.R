#simulation avec n = 10**j et j = 1:p
mcs.pi <- function(){
  ######### On fait Monte Carlo plusieurs fois pour remplir la matrice PIE #################
  t <- 50
  p <- 5
  j <- 1:p
  n <- 10**j
  zc <- 0   # Comptoir pour obtenir la zone du circle
  PIE <- matrix(nrow = t, ncol = p) # il faut mettre les estimations de pie
  ro <- 1   #variable pour remplir la matrice (lignes)
  col <- 1  #variable pour remplir la matrice (colonnes)
  tE <- vector(mode = "double", length = p)
  
  for (ro in 1:t) {   # On remplit la matrice avec les valeurs estimés
    
    while (col <= p) {
      start_time <- Sys.time()
      for (i in 1:n[col]) {  # Ici on tire le points aleatoires (Monte Carlo)
        
        x <- runif(1)
        y <- runif(1)
        
        if((x**2 + y**2) < 1) zc <- zc + 1   # Si c'est vrai alors le point est dans le cercle
      }
      end_time <- Sys.time()
      
      PIE[ro,col] <- ((4*zc) / n[col])   # On stock le valeur de pie obtenu dans la matrice
      zc <- 0   # Il faut réinisialiser le compteur
      if (ro == 1){
        tE[col] <- end_time - start_time  # On obtien le vector de temps d'execution
      }
      col <- col + 1  # On se deplace à la prochaine colonne
    }
    col <- 1    # Il faut réinisialiser le valeur des colonnes
    
  }
  print(PIE)
  print(tE)
}