library(FactoMineR)
library(factoextra)
library(ade4)
AFC = read.table("C:/Users/DELL 5570/Desktop/aff.txt")
print(AFC)

#Convertion du tablau en matrice 
afc <- as.matrix(AFC)
summary(afc)
# calul  du nombre total d'observation 
n = sum(afc)
print(n)

#Calcul  du tableau des fréquences relatives (fij).
F <- afc/n
print(F)


# les marges de fréquences relatives
Tmarges <- as.data.frame(addmargins(F))
print(Tmarges)

# Calcul des profils lignes  Methode1 
D1 <- diag(margin.table(F,1))
print(D1)
L <- solve(D1) %*% F
print(L)
L_moyen <- t(colSums(F))
print(L_moyen)
C_moyen <- t(rowSums(F))
print(C_moyen)

#Distance euclidienne 
D2 <- diag(margin.table(F,2))
x12 <- as.matrix(L[1,] - L[2,])
d_euclidienne_ligne_12 <- t(x12) %*% x12

#Distance khi2 deux
d_chi2_ligne_12 <- t(x12) %% as.matrix(solve(D2)) %% (x12)


print(d_euclidienne_ligne_12)
print(d_chi2_ligne_12)

x14 <- as.matrix(L[1,] - L[4,])
d_euclidienne_ligne_14 <- t(x14) %*% x14

#Distance khi2 deux
d_chi2_ligne_14 <- t(x14) %% as.matrix(solve(D2)) %% (x14)
print(d_euclidienne_ligne_14)
print( d_chi2_ligne_14)

#fréquences attendues à l'indépendance
F_ind <- t(C_moyen)%*%L_moyen
F_ind

# Calcul de la statistique Khi-deux du test
X2 = sum((F-F_ind)^2/F_ind)%*%n
X2

# Calcul des degrees de liberte 
ddl <- (nrow(F) - 1) * (ncol(F) - 1)
print(ddl)

#Calcul du la p-valeur
pchisq(X2,ddl, lower.tail = FALSE)

# Matrice dont on cherche les vecteurs propres dans l'analyse 
C <- solve(D2)%*% F
print(C)

#Valeurs et vecteurs propres de ML
ML <-t(C)%*%L
print(ML)
eigen(ML)

# Facteurs
FacL2 <- solve(D2) %*% eigen(ML)$vectors[,2]
print(FacL2)
FacL3 <- solve(D2) %*% eigen(ML)$vectors[,3]
print(FacL3)

#Les projections des profils des lignes
coord_ligne2 <- solve(D1) %*% F %*% FacL2
coord_ligne3 <- solve(D2) %*% F %*% FacL3

dat1 <- data.frame(dim1 = coord_ligne2, dim2 = coord_ligne3)
ggplot()+ geom_label(aes(dim1,dim2, label = c('Marrons','Noisettes','Verts','Bleus')), dat1)
chisq.test(F)
res_ca <- CA(F)
res_ca$eigen$values
summary(res_ca)






