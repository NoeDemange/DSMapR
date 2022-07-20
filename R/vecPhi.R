#' vecPhi
#'
#' @param matrice une matrice
#'
#' @return un vecteur
#' @export
#'
#' @useDynLib DSMapR, .registration = TRUE
vecPhi <- function(matrice){
  #permet de calculer le coefficient Phi de la premiere ligne avec la suivante
  T <- table(matrice[1,], matrice[2,])#fait un tableau de contingence
  Tab2 = T/sum(T)#fait la moyenne
  a = Tab2[1, 1]
  b = Tab2[1, 2]
  c = Tab2[2, 1]
  d = Tab2[2, 2]
  Phi = (a - (a + b) * (a + c))/sqrt((a + b) * (c + d) * (a + c) * (b + d)) #calcul de Phi
  Vphi <- vector()
  Vphi <-c(Phi)
  #permet de calculer le coefficient Phi de la deuxieme ligne a l'avant derniere ligne.
  Vphi <- append(Vphi, cppcorrNeighbor(matrice))
  #permet de calculer le coefficient Phi de la derniere ligne avec la precedente
  T <- table(matrice[nrow(matrice)-1,], matrice[nrow(matrice),])
  Tab2 = T/sum(T)
  a = Tab2[1, 1]
  b = Tab2[1, 2]
  c = Tab2[2, 1]
  d = Tab2[2, 2]
  Phi = (a - (a + b) * (a + c))/sqrt((a + b) * (c + d) * (a + c) * (b + d))
  Vphi <- append(Vphi, Phi)
  Vphi
}
