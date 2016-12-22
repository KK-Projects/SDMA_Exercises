# SDMA TP4
# Ahmed-Karim BERRADA
# Karl BOU ABBOUD
# Marouane YALA

# D?composition en valeurs singuli?res & Analyse en composantes principales


# EXERCICE 1 : DECOMPOSITION EN VALEURS SINGULIERES

n = 50
p = 5

X = matrix(rnorm(p*n), nrow=n, ncol=p)
S = cov(X)

# Les coefficients diagonaux de S sont plus significatif que les coefficients non-diagonaux
# ? cause de l'ind?pendance des variables Xj

# La d?composition en valeures singulieres consiste ? decomposer une matrice en un produit
# U . Sigma . tV avec U et V sont orthogonales, respectivement matrice de passage et de sortie et
  # Sigma est une matrice diagonale contenant les valeures singuli?res de S (racine des valeures propres de tS.S)

svd_ <- svd(S)
U <- svd_$u
V <- svd_$v
Sigma <- diag(svd_$d)

# S est symétrique, on a U = V et les valeures propres de tS.S sont les valeures propres de S
# Sigma est donc une décomposition en valeures propres.
# On compare U et V:

norm(U - V, type="F") # 1e-15
norm(U - V, type="2")  # 1e-15

sum(Sigma) - sum(diag(S)) # ~ 8e-16 les traces sont ?gales 

S2 <- U %*% Sigma %*% t(V)
norm(S2 - S, type="F") # ~3e-15
# Les deux matrices sont ?gales


# Analyse en composantes principales

# Le fichier contient les caractéristiques de plusieurs marques de voitures
# les caractéristiques sont: cylindre; puissance; vitess; poids; longeur; largeur
# les données sont d'ordre de grandeur assez différents

X <- read.table("cardata.txt", header=T, row.names=1, sep=";")
ncol(X) # 6 colonnes
nrow(X) # 24 lignes
plot(X) 
# Ce graphique représente les graphes des variables de nos données prise deux à deux.
# si le graphe ne semble pas dispersé les variables sont sûrement corrélés

library("corrplot")

cor(X)
corrplot(cor(X))
# Il y a de fortes correlation, la cylindre avec la puissance ou la longeur et le poids par exemple


# ACP

res <- prcomp(X, scale.=TRUE)
attributes(res)
# les attributs sont:
# sdev: la variance des composantes principales ordonnées de manières décroissante
# cela correspond aussi à la racine carrées des valeures propres de la matrice de covariance
# rotation: la matrice des vecteurs propres
# x: coordonnées des vecteurs de l'ancienne base dans la nouvelle base
# center, scale: moyennes et variance utilisées pour normaliser les variables

valpr <- res$sdev**2
plot(valpr, main="Valeures propres par ordre décroissant")
cumsum(valpr) / length(valpr)
# pour expliquer 95% de la covariance les trois premières composantes suffisent
# les quatres premières pour 98%

res$rotation
# pour le premier axe toutes les coordonnées sont prise en compte positivement avec un facteur 0.4
# pour le second axe cylind, puiss, vit sont pris en compte positivement, les autres négativement,
# la cylindrée à une importance plus faible que les autres variables
# pour le troisieme axe, la cylindres, la pusisance et le poids ont des contributions négatives
# la contribution de la puissance et de longeur sont très faibles
# pour le quatrième axe la vitesse et la longeur ont une contribution négative
# la contribution de la cylindrée est faible et celle de la longeur est importante

res2 <- dudi.pca(X, scannf=F, nf=6)

biplot(res,choices=1:2)
s.corcircle(res2$co[,1:2])

biplot(res,choices=2:3)
s.corcircle(res2$co[,2:3])

# Dans le premier graphe le premier axe indique les voitures qui ont des caractéristiques importante dans tous les domaines
# ce sont les grosses voitures, ces voitures sont représentés à droite sur le graphe.
# le deuxième axe met en opposition la cylindre, la puissance et la vitesse aux autres coordonnées
# il s'agit de différencier le moteur de la taille de la voiture
# Pour le deuxième graphe on compare cette fois-ci le poids ainsi que la puissance du moteur aux autres caractéristiques

## Classification non supervisée

res1 <- kmeans(X, 1)
# Coordonnées des centres
res1$centers 
# Classe de chaque point
res1$cluster 
# vector contenant la somme des distances au carré de chaque center aux points de son cluster
res1$withinss 
# somme des distances au carré de chaque voitures qui ne font pas partie du même cluster
res1$betweenss 
# la classification pour k=1 à peu de sens 

# On a ici deux clusters pour les voitures puissantes et imposantes et les voitures plus lègere 
res2 <- kmeans(X,2)
res2$centers
res2$cluster
res2$withinss
res2$betweenss

# Pour les deux clustering ci-dessous,
res3 <- kmeans(X,3)
res3$centers
res3$cluster
res3$withinss
res3$betweenss


res4 <- kmeans(X,4)
res4$centers
res4$cluster
res4$withinss
res4$betweenss
# Une classe intermédiaire en plus

## Caractères manuscrits

mImage = function(x) {
  img <- matrix(x,16,16,byrow = T)
  image(img, axes=FALSE, col=gray(0:255/255))
}

indices <- sample(nrow(d3), 1000, replace=FALSE)
d3train <- d3[indices,]
d3test <- d3[-indices,]
d8train <- d8[indices,]
d8test <- d8[-indices,]

# 3 et 8 moyen
mean3 <- colMeans(d3train)
mean8 <- colMeans(d8train)

mImage(mean3)
mImage(mean8)

d3train_s <- scale(d3train, center=T, scale=T)
var3 <- attr(d3train,"scaled:center")

d8train_s <- scale(d8train, center=T, scale=T)
var8 <- attr(d8train,"scaled:center")

eigen3 <- eigen(cov(d3train_s))
eigenval_3 <- eigen3$values
eigenvec_3 <- eigen3$vectors

eigen8 <- eigen(cov(d8train_s))
eigenval_8 <- eigen8$values
eigenvec_8 <- eigen8$vectors

# les valeures singulieres sont égales aux racines des valeures propres de la matrice de covariance

mod31 <- eigenvec_3[,1] %*% t(eigenvec_3[,1])
mod32 <- eigenvec_3[,2] %*% t(eigenvec_3[,2])
mod33 <- eigenvec_3[,3] %*% t(eigenvec_3[,3])

mod81 <- eigenvec_8[,1] %*% t(eigenvec_8[,1])
mod82 <- eigenvec_8[,2] %*% t(eigenvec_8[,2])
mod83 <- eigenvec_8[,3] %*% t(eigenvec_8[,3])

mImage(mod31)
mImage(mod32)
mImage(mod33)
mImage(mod81)
mImage(mod82)
mImage(mod83)

# Il est possible de reconnaitre des 3 et 8 dans les figures

P3 <- eigenvec_3[,1:5]
p3 <- P3 %*% t(P3)
norm(p3 %*% p3 - p3) # C'est bien une matrice de projection

P8 <- eigenvec_8[,1:5]
p8 <- P8 %*% t(P8)
norm(p8 %*% p8 - p8)


# Reconstruction
test_scale <- scale(d3test)

testImg = test_scale[1,]
mImage(testImg + mean3)
projection = t(P3) %*% testImg;
mImage(P3 %*% projection + mean3)

# Ce qui permet de reconstruire les données
# Il suffit donc de garder en mémoire cinq éléments ce qui réduit le stockage