#' vecARorder
#'
#' @param dist un objet de type dist
#' @param seriation une seriation pour organiser la matrice de distance
#'
#' @return un vecteur
#' @export
#'
#' @useDynLib DSMapR, .registration = TRUE
#'@importFrom seriation get_order
vecARorder <- function(dist, seriation){
  TMdMat <- as.matrix(dist)
  TMdMat <- TMdMat[seriation::get_order(seriation), seriation::get_order(seriation)]
  Tmat <- matrix(1, ncol(TMdMat),ncol(TMdMat))
  Tmat[which(row(Tmat)>col(Tmat))] <- -1
  TMdMat2 <- TMdMat*Tmat
  TMdMat3 <- TMdMat2 - TMdMat2[,1]

  Viar <- cppARorder(TMdMat3)
  vec <- sqrt(abs(Viar))

  vec
}
