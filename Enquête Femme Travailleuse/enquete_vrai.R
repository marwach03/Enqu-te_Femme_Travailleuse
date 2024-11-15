rm(list=ls())
setwd("C:/Users/Marwa CHAOUI/Desktop/enquete vrai")
library("FactoMineR")
library("factoextra")
FT<-read.delim("FT2.txt",sep = "\t", dec = ";",row.names = 1)
head(FT)
View(FT)
chisq<-chisq.test(FT)
print(chisq)


res.AFC<-CA(FT, graph = FALSE)
summary(res.AFC)


#etape 2
#valeurs propres /Variances
eig.val <-get_eigenvalue(res.AFC)
eig.val
res.AFC$eig
fviz_eig(res.AFC)

#etape 3 : interpretation des axes contributions et qualités
#repel = TRUE pour eviter le chevauchement de texte
res.AFC<-CA(FT, graph = TRUE, axes = c(1,2), ncp = 2)
fviz_ca_biplot(res.AFC, repel = TRUE)
ligne <- get_ca_row(res.AFC)
colo=get_ca_col(res.AFC)

#utilisation rbind
re1=rbind(colo$contrib[,1],colo$cos2[,1],colo$coord[,1])
re1
re2=rbind(colo$contrib[,2]+colo$cos2[,2]+colo$coord[,2])
re2



#contributions des colonnes à la dimension 1
fviz_contrib(res.AFC, choice="col", axes = 1, top = 17)
sort(colo$contrib[,1], decreasing = TRUE)
colo$cos2[,1]
colo$coord[,1]

#contributions des lignes à la dimension 1
fviz_contrib(res.AFC, choice="row", axes = 1, top = 9)
sort(ligne$contrib[,1], decreasing = TRUE)
ligne$cos2[,1]
ligne$coord[,1]

#contributions des colonnes à la dimension 2
fviz_contrib(res.AFC, choice="col", axes = 2, top = 17)
sort(colo$contrib[,2], decreasing = TRUE)
colo$cos2[,2]
colo$coord[,2]

#contributions des lignes à la dimension 2
fviz_contrib(res.AFC, choice="row", axes = 2, top = 9)
sort(ligne$contrib[,2], decreasing = TRUE)
ligne$cos2[,2]
ligne$coord[,2]



fviz_cos2(res.AFC, choice="col", axes = 1:2, top = 17)
fviz_cos2(res.AFC, choice="row", axes = 1:2, top = 9)

#etape 4
sort(ligne$cos2[,1]+ligne$cos2[,2], decreasing = TRUE)
sort(colo$cos2[,1]+colo$cos2[,2], decreasing = TRUE)
fviz_ca_biplot(res.AFC, select.row=list(cos2=0.84), select.col=list(cos2=0.74),axes = 1:2)
