
****************************#   Realiser Par   #************************************  

             //////////////                    ///////////////
                             YOUSSEF LAAMARI

             //////////////                    ///////////////
********************************#   Partie 1   #***************************** 
# Chargement des bibliothèques nécessaires
library(FactoMineR)
library(factoextra)
library(ade4)

# Lecture du fichier de données "examen.txt"
Data <- read.table("C:/Users/DELL 5570/Downloads/examen.txt")

# Affichage des données
Data

# Renommage des colonnes pour faciliter la manipulation
colnames(Data) <- c("Value", "Price", "Economy", "Service", "Design", "Sport", "Safety", "Easy")

# Définition des noms de lignes
rownames(Data) <- c("Audi", "BMW", "Citroen", "Ferrari", "Fiat", "Ford", "Hyundai", "Jaguar", "Chevrolet", "Dacia", "Mercedes", "Misubishi", "Nissan", "Opel", "Peugoet", "Renault", "Toyota", "Volvo", "VW")

# Affichage des données après le renommage
Data

# Conversion des données en une matrice
X <- data.matrix(Data)

# Affichage de la matrice X
X

# Résumé des caractéristiques statistiques
summary(Data)

# Calcul de la matrice de corrélation
Correlation_matrix <- cor(Data)
Correlation_matrix

# Analyse en composantes principales (ACP)
pca <- prcomp(X, scale=TRUE)
summary(pca)

# ACP avec visualisation
pca <- PCA(X, scale.unit = TRUE, graph = FALSE)

# Affichage des valeurs propres et des vecteurs propres
eigenvalues <- get_eigenvalue(pca)
eigenvalues

# Affichage des contributions des variables
contributions <- get_pca_var(pca)$contrib
contributions

# Affichage des qualités de représentation des variables
quality_representations <- get_pca_var(pca)$cos2
quality_representations

# Représentation graphique des individus avec les noms de lignes
individus_plot <- fviz_pca_ind(pca, geom = "point", palette = "jco")

# Affichage du graphique des individus
print(individus_plot)

# Représentation graphique des variables
variables_plot <- fviz_pca_var(pca, col.var = "contrib")
variables_plot


**********************************#  Partie 2  #******************************

# Division des données en deux groupes
X_group <- c("Value", "Price")
Y_group <- c("Economy", "Service", "Design", "Sport", "Safety", "Easy") 

# Sélection des variables pour les groupes X et Y
X_Data <- X[, X_group]
Y_Data <- X[, Y_group]

# Centrage des données
X_data_centered <- scale(X_Data, center = TRUE, scale = FALSE)
Y_data_centered <- scale(Y_Data, center = TRUE, scale = FALSE)

# Affichage des données centrées
print(X_Data)
print(Y_Data)
print(X_data_centered)
print(Y_data_centered)

# Calcul des corrélations complètes
cor_complete <- cor(X)
cor_complete

# Calcul des corrélations intra-groupes
cor_intra_X <- cor(X_Data)
cor_intra_Y <- cor(Y_Data)
cor_intra_X
cor_intra_Y

# Calcul des corrélations inter-groupes
cor_inter <- cor(X_Data, Y_Data)
cor_inter

# Analyse canonique des corrélations (CCA)
cca_results <- cancor(X_data_centered, Y_data_centered)
cca_results

# Affichage des corrélations canoniques
print(cca_results$cor)

# Extraction des coefficients canoniques pour le groupe de variables X
coefficients_X <- cca$xcoef
coefficients_X

# Extraction des coefficients canoniques pour le groupe de variables Y
coefficients_Y <- cca$ycoef
coefficients_Y

# Récupération des valeurs propres associées
eigenvalues <- cca$cor^2
eigenvalues

# Calcul des coefficients canoniques normalisés pour le groupe X
norm_coefs_X <- coefficients_X / sqrt(eigenvalues)
norm_coefs_X

# Calcul des coefficients canoniques normalisés pour le groupe Y
norm_coefs_Y <- coefficients_Y / sqrt(eigenvalues)
norm_coefs_Y

# Transposition de la matrice du groupe X
t_groupe_X <- t(X_group)
t_groupe_X

# Calcul des corrélations entre les variables originales et les axes canoniques pour le groupe X

# Graphique des corrélations canoniques
barplot(cca$cor, names.arg = c("Canonical 1", "Canonical 2"),  main = "Corrélations Canoniques", xlab = "Axes Canoniques", ylab = "Corrélation")

***********************************#  Partie 3  #****************************

# Lecture du fichier de données "examen.txt" dans un nouveau tableau nommé "Marques"
Marques <- read.table("C:/Users/DELL 5570/Downloads/examen.txt")

# Affichage des données dans le tableau "Marques"
Marques
# Résumé des statistiques des données
summary(Marques)

# Vérification que toutes les colonnes sont de type numérique
str(Marques)

# Conversion des données en une matrice
Marques <- data.matrix(Marques)

# Création de la matrice de croisement avec la fonction pairs()
pairs(Marques, mar=c(2, 2, 2, 2))

# Calcul de la matrice centrée réduite
Marques.cr <- scale(Marques)
print(Marques.cr)
summary(Marques.cr)

# Affichage des premières lignes du tableau de données centrées réduites
head(Marques.cr)

# Calcul de la matrice de distances
d.Marques <- dist(Marques)
print(d.Marques)

# Partitionnement de la page en quatre
par(mfrow=c(2,2))

# Classification ascendante hiérarchique avec la méthode de liaison moyenne
cah.ave <- hclust(d.Marques, method="ave")

# Tracé du dendrogramme avec la méthode de liaison moyenne
cah.ave <- hclust(d.Marques, method="ave")
dendro.ave <- as.dendrogram(cah.ave)
plot(dendro.ave, main="Dendrogramme (Méthode de liaison moyenne)")

# Découpage en trois sous-groupes (classes)
rect.hclust(cah.ave, k=3)

# Découpage numérique
dcah.ave <- cutree(cah.ave, k=3)

# Affichage des résultats
print(dcah.ave)

# Méthode de liaison complète
cah.complete <- hclust(d.Marques, method="complete", members=NULL)

# Dendrogramme avec la méthode de liaison complète)
plot(cah.complete, main="Dendrogramme (Méthode de la liaison complète)

# Dendrogramme de la méthode de la liaison simple
plot(cah.single, main="Dendrogramme (Méthode de la liaison simple)")

# Découper en trois sous-groupes (classes)
rect.hclust(cah.single, k=3)

# Découpage numérique
dcah.single <- cutree(cah.single, k=3)

# Afficher les résultats
print(dcah.single)


































