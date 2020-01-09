boite <- function(pol_en_matrice){
  
  bo <- matrix(nrow = 2, ncol = 2, dimnames=list(c("min", "max"), c("x","y")))  # Cette matrice contiendra les maxs/mins du poly
  bo[,] <- c(pol_en_matrice[1,1],0,pol_en_matrice[1,2],0)                       # Il faut initialiser les mins avec des valeurs quelconques

  for (i in 1:length(pol_en_matrice[,1])) {                                     # Boucle de 1 à la quantité de lignes du polygone
                                                                                # Maintenant on obtiens les maxs et mins
    if(bo[1,1] > pol_en_matrice[i,1]) bo[1,1] <- pol_en_matrice[i,1]
    if(bo[2,1] < pol_en_matrice[i,1]) bo[2,1] <- pol_en_matrice[i,1]
    
    if(bo[1,2] > pol_en_matrice[i,2]) bo[1,2] <- pol_en_matrice[i,2]
    if(bo[2,2] < pol_en_matrice[i,2]) bo[2,2] <- pol_en_matrice[i,2]
  }
  
  print(bo)
}

points_aleatoires <- function(n, bo){                                           # Fonction pour générer des points aléatoires
  M <- matrix(nrow = n ,ncol = 2, dimnames = list(c(), c("x","y")))             # Matrice M pour les enregistrer
  
  for (i in 1:n) {                                                              # Génération de points de 0 aux maxs
    M[i,1] <- runif(n = 1, min = 0, max = bo[2,1])
    M[i,2] <- runif(n = 1, min = 0, max = bo[2,2])
  }
  print(M)
}