#' vecRunsTest
#'
#' @param matrice une matrice
#' @param alternative voir 'alternative' de la fonction e.divisive du package ecp
#'
#' @return un vecteur
#' @export
#'
#' @useDynLib DSMapR, .registration = TRUE
vecRunsTest <- function(matrice, alternative = "two.sided"){
  #fait le Run test en appelant un script c++
  RunTV <- cpprunstest(as.matrix(matrice),alternative)
  #traitement des resultats
  RunTV[RunTV > 0] <- 0
  vect <-(-1)*RunTV
  vect
}
