#' TransMeanSeuil
#'
#' @param VecIndStruc un vecteur issu d'une methode d'indice de structuration
#' @param Ssup une valeur seuil superieur
#' @param Sinf une valeur seuil inferieur
#' @param methodSplit chaine de caractere "transition" ou "mean"
#' @param GrouporSeuil chaine de caractere "group" ou "seuil"
#' @param Pval une p-value pour la fonction e.divisive du package ecp
#' @param minsize un nombre minimum d'observation pour la fonction e.divisive du package ecp
#' @param conf.level un niveau de confiance pour le test de student
#'
#' @return une liste contenant
#' @export
#'
#' @importFrom ecp e.divisive
#' @importFrom stats t.test
TransMeanSeuil <- function(VecIndStruc, Ssup = NULL, Sinf = NULL, #les seuils utilises pour l'encadrement
                           methodSplit = "transition", #mean ou transition
                           GrouporSeuil = "group",
                           Pval = 0.05, minsize = 3, #e.divisive
                           conf.level = 0.95 #t.test
                          ){
  #e.divisive fonction du package ECP, analyse point de transition
  Split <- ecp::e.divisive(as.matrix(VecIndStruc), min.size=minsize, sig.lvl=Pval)$cluster
  cat("Nombre de split trouve par analyse de points de transition", nlevels(as.factor(Split)),"\n")

  #test de student t.test() de chaque 2 groupes successifs trouver par l'analyse des points de transitions
  if(methodSplit == "mean"){
    i = 1
    Msplit <- Split
    while(i<nlevels(as.factor(Msplit))){
      res <- stats::t.test(VecIndStruc[which(Msplit[]==i)],VecIndStruc[which(Msplit[]==i+1)])
      if(res$p.value > 1-conf.level){ #si les moyennes ne sont pas significativement differentes on regroupe les 2 groupes
        for(j in i+2:nlevels(as.factor(Msplit))-1){
          Msplit[which(Msplit[]==j)] <- j-1
        }
        if(i!=1){i = i-1}
      }
      else i=i+1
    }
    FSplit <- as.factor(Msplit)
    cat("Nombre de split trouve par analyse des moyennes", nlevels(as.factor(Msplit)),"\n")
  } else {FSplit <- as.factor(Split)}



  ###Fait la moyenne des indices de structuration des groupes de l'etape precedente
  vMean <- numeric()
  for(k in levels(FSplit)){
    v <- VecIndStruc[which(FSplit[]==k)]
    vMean[k] <- mean(v)
  }

  ###Fait vecteur pour l'annotation soit les groupes soit les seuils
  if(GrouporSeuil == "seuil"){
    vGroup <- character(length = length(VecIndStruc))
    for(l in levels(FSplit)){
    if(vMean[l] >=Ssup){
      vGroup[which(FSplit[]==l)] <- "sup"
    } else if(vMean[l] <=Sinf){
      vGroup[which(FSplit[]==l)] <- "inf"
    } else vGroup[which(FSplit[]==l)] <- NA
   }
  } else {
    vGroup <- numeric(length = length(VecIndStruc))
    indice = 1;
    for(l in levels(FSplit)){
      if(vMean[l]<Ssup && vMean[l]>Sinf) {
        vGroup[which(FSplit[]==l)] <- NA
      }
      else{vGroup[which(FSplit[]==l)]<- indice
      indice = indice + 1}
    }
  }

  RList <- list("FSplit" =FSplit, "vMean"=vMean, "vGroup"=vGroup, "Ssup"=Ssup, "Sinf"=Sinf)
  RList
}
