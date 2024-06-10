# ACP exemple 1 : 
library(FactoMineR)
library(factoextra)
X1  = c(3,5,6)
X1
X2 = c(7,5,4)
X2
X3 = c(9,5,2)
X3
prix = data.frame(X1,X2,X3)
prix
rownames(prix) = c("mag1","mag2","mag3")
prix 

X<-data.matrix(prix)
X
summary(X)
#Affichage de le matrice du correlation 
cor(X)

# Affichage de la dispersion des variables 
boxplot(X)

summary(t(X)) # Il donne le nuage des individus

#ACP Centree les donnees :

Xbar <- scale(X,center=TRUE ,  scale=FALSE ) # Il donne la  matrice centree et aussi la moyenne de chaque variable 
Xbar

Sigma<- t(Xbar)%*%Xbar/nrow(Xbar)  # calcule la matrice du correlation  associe a X
# R= Xtranspose*P*X
ACP <- eigen(Sigma)
ACP
ACP$values # donne les valeurs propres du ACP 

cumsum(ACP$values)/sum(ACP$values) # participation des axes 

plot(ACP$values, type = "b")  # nous retiendrions 2 axes associe a les deuux premieres valeurs propres  pour l'ACP 


U<-eigen(Sigma)$vectors
C <- Xbar%*%U
C

coord = C[,1:3]
coord # extraire les deux premiers composante 
coord2 = coord^2
coord2 # le caree de chaque composantes  pour tirer la qualite du representation 

matrix(eigen(Sigma)$values,3,3,byrow=TRUE)

contrib = 1/3*coord2/matrix(eigen(Sigma)$values,3,3,byrow=TRUE)
contrib


coord3 = C[,1:3]
coord3
coord4 = coord3^2
coord4 
cos2 = coord2/rowSums(coord4)
cos2 

plot(C[,1:2],pch=2,cex=cos2)
plot(C[,1],C[,2],pch=2 , xlab="PC1" ,ylab="PC2")
text(C[,1:2],labels = rownames(c),pos=3)

# Matrice contenant les corrélations entre les variables initiales et les variables principales
Corvarcomp <- diag(1/sqrt(diag(Sigma))) %*% ACP$vectors %*% diag(sqrt(ACP$values)) 
rownames(Corvarcomp) <- colnames(X) 
Corvarcomp


# ACP sur Corvarcomp
res.pca <- PCA(Corvarcomp, graph = FALSE)
fviz_pca_var(res.pca, col.var = "black", repel = TRUE, axes = c(1, 2))
title("Cercle des corrélations")
