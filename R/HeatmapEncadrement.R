#' makeHeatmapEncadre
#'
#' @param matrice une matrice
#' @param VecIndStruc un vecteur issu d'une methode d'indice de structuration
#' @param LisTMS la liste resultant de la fonction TransMeanSeuil
#' @param supcol couleur si moyenne superieur au seuil
#' @param infcol couleur si moyenne inferieur au seuil
#' @param backgroundcol couleur de fond "black" ou "white"
#' @param Legendname nom de la legende
#' @param color palette de couleur
#' @param width largeur de la heatmap
#' @param column_names_max_height  voir Heatmap
#' @param column_names_gp voir Heatmap
#' @param row_names_gp voir Heatmap
#' @param show_row_names voir Heatmap
#' @param colorBorder couleur des barrieres sur la heatmap
#' @param row_title voir Heatmap
#' @param row_gap voir Heatmap
#' @param column_title voir Heatmap
#' @param column_gap voir Heatmap
#' @param legend_param voir Heatmap
#' @param AnnoClustCol une palette de couleur
#' @param AnnoLineCol une couleur pour faire les lignes des seuils
#' @param colPsup couleur si point superieur au seuil
#' @param colPinf couleur si point inferieur au seuil
#'
#' @export
#' @import viridis
#' @import ComplexHeatmap
#' @import grid
#' @importFrom circlize colorRamp2
#' @importFrom grDevices rainbow
makeHeatmapEncadre <- function(matrice, VecIndStruc,  #matrice ordonnee et vecteur d'analyse de la matrice
                               LisTMS, supcol = "green", infcol = "red",#definition des couleurs des groupes
                               #Voir les parametres suivants dans la fonction Heatmap de complexheatmap
                               backgroundcol = "black",#definition de la couleur de fond de la figure
                               Legendname = "Legend",
                               color = viridis::magma(256), width = NULL,
                               column_names_max_height = max_text_width(colnames(matrice)),
                               column_names_gp = gpar(fontsize = 0.2 + 1/log10(ncol(matrice)),
                                               col=colaff),
                               row_names_gp = gpar(fontsize = 0.2 + 1/log10(nrow(matrice)),
                                            col=colaff),
                               show_row_names = FALSE,
                               colorBorder = "white",
                               row_title = NULL, row_gap = unit(0,"mm"),
                               column_title = NULL, column_gap = unit(0,"mm"),
                               legend_param = list(title_gp = gpar(col=colaff), labels_gp = gpar(col=colaff)),
                               AnnoClustCol = rainbow, AnnoLineCol = "yellow", colPsup = "chocolate1", colPinf = "turquoise1"
                              ){
  #definition de parametre pour un bon affichage
  if(backgroundcol == "black"){
    colaff = "white"
  }
  else{colaff="black"}

    vGroup <- LisTMS$vGroup
    if(is.numeric(vGroup)){
      #Permet d'inverser des indices dans le vecteur vGroup et de faire une palette de couleur
      #(Avoir des couleurs differentes pour des clusters a cote dans l'annotation)
      VmA <- c(1:length(vGroup))
      mrep <- c(length(vGroup),0)
      vrep <- rep(mrep,length(vGroup))
      for(j in VmA){vGroup[which(vGroup[]==j)]<-j+vrep[j]}
      pal = circlize::colorRamp2(as.integer(levels(as.factor(vGroup))),AnnoClustCol(n = nlevels(as.factor(vGroup))))
    } else{
      pal = c("sup"= supcol, "inf" = infcol)
    }



    if(nrow(matrice) == length(VecIndStruc) && nrow(matrice) == length(vGroup)){
      #multiplication du vecteur d'analyse de la matrice avec la matrice permettant la coloration
      matrice <- VecIndStruc * matrice

      #definition de l'annotation des clusters des lignes et permet d'afficher la valeur de l'analyse de la ligne
      RowAN <- rowAnnotation(Cluster = anno_simple(vGroup, na_col=backgroundcol, col = pal),
                           foo = anno_points(VecIndStruc, gp = gpar(col = ifelse(VecIndStruc >=LisTMS$Ssup,colPsup,
                                                              ifelse(VecIndStruc<=LisTMS$Sinf,colPinf,colaff))),
                                          size=unit(1,"mm"),axis_param = list(gp = gpar(col=colaff)),),
                                          annotation_name_gp = gpar(col =colaff))
      #creation de la Heatmap
      HM <- Heatmap(matrice, name = Legendname,
                  cluster_rows = FALSE, cluster_columns = FALSE,
                  col = color,
                  column_names_max_height = column_names_max_height,
                  show_row_names = show_row_names,
                  row_names_gp = row_names_gp,
                  column_names_gp = column_names_gp,
                  width = width,
                  #pour split
                  border = colorBorder,
                  heatmap_legend_param = legend_param,
                  row_split = LisTMS$FSplit, row_title = row_title, row_gap = row_gap,
                  left_annotation = RowAN,
      )
      #dessine la Heatmap avec en fond la couleur choisie
      draw(HM, background = backgroundcol)
      #Permet de faire les cadres des groupes >Ssup ou <Sinf
      for(s in levels(LisTMS$FSplit)){
        decorate_annotation("foo",{grid.lines(unit(c(LisTMS$Ssup,LisTMS$Ssup),"native"),c(0,1), gp = gpar(col=AnnoLineCol))
          grid.lines(unit(c(LisTMS$Sinf,LisTMS$Sinf),"native"),c(0,1), gp = gpar(col=AnnoLineCol))
        }, slice = s)
      }
    } else if(ncol(matrice) == length(VecIndStruc) && ncol(matrice) == length(vGroup)) {
      #transpose de la multiplication du vecteur d'analyse de la matrice avec la transpose de la matrice
      #permettant la coloration
      matrice <- t(VecIndStruc* t(matrice))

      #definition de l'annotation des clusters des colonnes et permet d'afficher la valeur de l'analyse de la colonne
      ColumnAN <- HeatmapAnnotation(Cluster = anno_simple(vGroup, na_col=backgroundcol, col = pal),
                                    foo = anno_points(VecIndStruc,
                                                      gp = gpar(col = ifelse(VecIndStruc >LisTMS$Ssup,colPsup,ifelse(VecIndStruc<LisTMS$Sinf,colPinf,colaff))),
                                                      size=unit(1,"mm"),axis_param = list(gp = gpar(col=colaff)),),
                                    annotation_name_gp = gpar(col =colaff))
      #creation de la Heatmap
      HM <- Heatmap(matrice, name = Legendname,
                    cluster_rows = FALSE, cluster_columns = FALSE,
                    col = color,
                    column_names_max_height = column_names_max_height,
                    show_row_names = show_row_names,
                    row_names_gp = row_names_gp,
                    column_names_gp = column_names_gp,
                    width = width,
                    top_annotation = ColumnAN,
                    #pour split
                    border = colorBorder,
                    heatmap_legend_param = legend_param,
                    column_split = LisTMS$FSplit, column_title = column_title, column_gap = column_gap,
      )
      #dessine la Heatmap avec en fond la couleur choisie
      draw(HM, background = backgroundcol)
      #Permet de faire les cadres des groupes >Ssup ou <Sinf
      for(s in levels(LisTMS$FSplit)){
        decorate_annotation("foo",{grid.lines(c(0,1), unit(c(LisTMS$Ssup,LisTMS$Ssup),"native"), gp = gpar(col=AnnoLineCol))
          grid.lines(c(0,1),unit(c(LisTMS$Sinf,LisTMS$Sinf),"native"), gp = gpar(col=AnnoLineCol))
        }, slice = s)
      }
    } else { cat("Erreur : les donnees entrees ne correspondent pas a la matrice")}
}
