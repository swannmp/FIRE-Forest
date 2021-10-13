#############################################################  ACP   ########################################################


library(FactoMineR)
library(factoextra)
library(ggplot2)



##### Import des donnees
library(readr)
MBA <- read_csv("MBA.csv", col_types = cols(Old = col_skip(), 
                                            Tag = col_skip(), Fam = col_factor(levels = c()), 
                                            Gen = col_factor(levels = c()), Spe = col_factor(levels = c()), 
                                            WSG = col_skip(), WSG.code = col_skip(), 
                                            Height = col_skip(), HeightC = col_skip(), 
                                            HeightT = col_skip()))
View(MBA)

### variable quali en quanti
library(dplyr, quietly = TRUE)

library(questionr)
d$fs.fac <- factor(MBA$freres.soeurs)
levels(d$fs.fac)


##### PCA

#pca
res.pca<-PCA(MBA, scale.unit = TRUE, ncp = 5, graph = TRUE)
res.pca


#screeplot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


#cercles des variables
par(mfrow=c(1,2))
x11()
fviz_pca_var(res.pca, axes= c(1,2), col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), labelsize=0.35)


#contribution des variables aux 2 dimensions
fviz_contrib(res.pca, choice = "var", axes = c(1,2), top = 10)


#graphique ACP
fviz_pca_ind (res.pca, col.ind = "cos2", axes = c(1,2),
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # ?vite le chevauchement de texte 
)

#ACP ellipses
pcael.pca <- PCA(T3ACPEL[, - 1], graph = FALSE)

x11()

fviz_pca_ind(pcael.pca, axes = c(1,3), geom.ind = "text", # Montre  le "text" seulement
             col.ind = T3ACPEL$ID, # colorer by groups
             palette = rainbow(15),
             addEllipses = TRUE, # Ellipses de concentration
             ellipse.type = "confidence",
             legend.title = "Groups"
)





















