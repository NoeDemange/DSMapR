#' vecECPrunTest
#'
#' @param matrice une matrice
#' @param pval valeur de p-value
#' @param N nombre minimum d'observation, voir 'min.size' de la fonction e.divisive du package ecp
#'
#' @return un vecteur
#' @export
#'
#' @useDynLib DSMapR, .registration = TRUE
vecECPrunTest <- function(matrice, pval = 0.01,N = 3){
  # creation d'une matrice bordee avec des colonnes de 0 pour eviter les effets de bords
  ZeroMat <- matrix( rep( 0, len=(ncol(matrice)%/%3)*nrow(matrice)), nrow = nrow(matrice))
  datamatOrdered2 <- matrice
  datamatOrdered2 <- cbind(datamatOrdered2, ZeroMat)
  datamatOrdered2 <- cbind(ZeroMat, datamatOrdered2)
  #fonction C++ qui calcul le nombre de bloc sur une ligne par analyse de points de changement multiples
  RunECP <- e_divisivecpp(datamatOrdered2,N,pval)
  #modification pour obtenir une bonne coloration
  RunECP <- RunECP - 2
  RunECP[RunECP<2] <- max(RunECP)
  RunECP <- 1/(RunECP%/%2)
  RunECP
}
