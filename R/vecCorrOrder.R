#' vecCorrOrder
#'
#' @param dist un objet de type dist
#' @param seriation une seriation pour organiser la matrice de distance
#'
#' @return un vecteur
#' @export
#'
#' @importFrom seriation get_order
vecCorrOrder <- function(dist, seriation){
    dMat <- as.matrix(dist)
    #ordonner la matice de distance selon les resultats de Dendser sur cette matrice de distance
    serdMat <- dMat[seriation::get_order(seriation), seriation::get_order(seriation)]
    mat <- matrix(1, ncol(serdMat),ncol(serdMat))
    mat[which(row(mat)>col(mat))] <- -1
    serdMat2 <- serdMat*mat
    serdMat3 <- serdMat2 - serdMat2[,1]
    OrdVec2 <- as.matrix(seq(0,(nrow(dMat)-1)))
    cor <- cor(OrdVec2, t(serdMat3), method = "spearman") #correlation
    vec<- abs(as.vector(cor))
    vec
}
