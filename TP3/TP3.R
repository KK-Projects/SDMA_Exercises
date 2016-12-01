
# Application I : regression Ridge et Lasso
### Regression Ridge
data <- read.table("usa_indicators.txt", sep=";", header=T)
dim(data)

# Il y a 14 observations pour 110 indicateurs ici p > n 
# Le problème de regression n'est pas simple, la matrice de condionnement
# est non inversible et l'estimateur des moindres carrées instable

# EN.ATM.CO2E.KT représente l'évolution du CO2 au cours du temps
plot(data$Year, tab$EN.ATM.CO2E.KT)

# La regression minimise l'erreur des moindres carrées, si les données ne sont
# pas normées les variables dont les valeures peuvent devenir assez importante
# prendront plus d'importance dans le model
scaled_data <- data.frame(scale(data, center=FALSE))

# La regression ridge est une méthode de régularisation de la méthode des moindres
# carrées. C'est une pénalisation en norme L2 de la MCO qui impose au vecteur solution du problème
# MCO une penalisation des valeurs ce qui diminuera la valeur des beta s'ils explosent.

library(MASS)
# On effectue la Regression Ridge en retirant "Year" des variables
ridge <- lm.ridge('EN.ATM.CO2E.KT~.', data=scaled_data[-1], lambda=c(0,100)) 
coef(ridge) 
coeffs <- data.frame(coef(ridge))
sort(abs(coeffs[1,]), decreasing=T)[1:6]
# 5 coefficients les plus influents: AG.LND.TOTL.K2, EG.USE.COMM.FO.ZS
# AG.LND.AGRI.K2, SP.RUR.TOTL, AG.SRF.TOTL.K2
# On va jusque 6 pour ne pas tenir du compte de l'intercept

sort(abs(coeffs[2,]), decreasing=T)[1:6]
# 5 coefficients les plus influents: AG.LND.TOTL.K2, SP.RUR.TOTL
# EG.USE.COMM.FO.ZS, AG.SRF.TOTL.K2, SP.POP.65UP.TO.ZS

# Les indicateurs paraissent vraisemblables ex: Taille de la terre, 
# consommation en énergies fossiles etc...

coef(ridge) # Recupere les coefficients de la regression multiple
ridge$coef 
# Avec ridge$coef les coefficient sont quasiment tous nuls
# ce qui n'est pas le cas avec coef(ridge)
# D'après la documentation:
# coef:matrix of coefficients, one row for each value of lambda.
# Note that these are not on the original scale and are for use by the coef method.
# Donc coef est la fonction qui prend en compte les donnée scale

resridge <- lm.ridge('EN.ATM.CO2E.KT~.', data=scaled_data[-1],lambda=seq(0,100,0.01))
plot(resridge$GCV)
plot(resridge)
# La courbe est croissante, son minimum est au niveau de l'origine, c'est donc le modèle à conserver
which.min(resridge$GCV) # lambda = 0.01 (deuxième colonne)
coefridge <- coef(resridge)[which.min(resridge$GCV),]

# Calcul de l'erreur quadratique moyenne
Yridge <- as.matrix(scaled_data[-1])%*%as.vector(coefridge)
mean((Yridge - scaled_data$EN.ATM.CO2E.KT)**2) # 0.092 (avec les données normalisées)

## Regression lasso

# La regression LASSO suit le même principe que la regression RIDGE, sauf
# que la regularisation est de type L1
library(lars)
data_clean <- scaled_data[-22] # Supprime la variable a predire de nos donnees (CO2E)
data_clean <- data_clean[-1] # Supprime la variable "Year"
X = as.matrix(data_clean) 
Y = as.vector(scaled_data$EN.ATM.CO2E.KT)

reslasso <- lars(X,Y,type="lasso") # Fit la regression Lasso
plot(reslasso)
plot(reslasso$lambda)
# Le premier graphe représente les chemins de regularisation
# L'axe des x represente le ratio de la some des valeurs absole des betas sous lasso par rapport
# a la somme des coefficients obtenu part estimation ordinaire. La barre verticale indique quand
# un des coefficients est mis à 0. 
# Le deuxieme graphique represente le lambda qui annule les coefficients.

coef <- predict.lars(reslasso,X,type="coefficients",mode="lambda",s=0)
# Correspond au problème de regression classique, on remraque que presque tout les coefficients
# sont nuls

coef <- predict.lars(reslasso,X,type="coefficients",mode="lambda",s=c(0.02,0.04,0.06))
# Presque tout les coefficients sont nuls (influence de la norme l1)
# Les variables significatives sont: TX.VAL.FOOD.ZS.UN, SP.DYN.AMRT.MA, SE.PRM.ENRL.FE.ZS
# EG.USE.COMM.FO.ZS, EG.USE.COMM.KT.OE
# Celles-ci sont plus significative que lors de la regression ridge.
# Elles témoignent d'une forte activité industrielle, source d'émission de CO2

pY <- predict.lars(reslasso,X,type="fit",mode="lambda",s=0.06)
mean((Y-pY$fit)**2) 
# On obtient 0.008606711 inférieur à la regression ridge, ce qui montre que la regression Lasso
# a pu être un meilleur fit en donnant plus d'importance aux variables dont le coefficient est 
# non nul


# Application II : aggregation des modeles

library(rpart)
library(ipred)
library(randomForest)
library(kernlab)
library(ade4)


tab = read.table('spam.txt', header=T, sep=';');
dim(tab)
colnames(tab)
# Table has 4601 rows (observations) and 58 columns (variables)
# Le 48 premieres variables representent le pourcentagede mots egals
# a une certaine valeur dans le texte

# Les 6 suivantes font la même chose pour un caractère particulier
  # longeur moyenne sequence de lettres
  # nombre de lettres capitales
# target variable: spam ou non

# Splitting data in train and test
## 75% of the sample size
smp_size <- floor(0.75 * nrow(tab))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(tab)), size = smp_size)

tab_train <- tab[train_ind, ]
tab_test <- tab[-train_ind, ]
# Arbres de Classification

tree <- rpart('spam ~ .', data=tab_train)
par(mfrow = c(1,1), xpd = NA) 
text(tree, use.n = TRUE)
# La variable la plus influente est A.53, c'est à dire la présence du caractere "$"

p_train <- predict(tree, tab_train, type="class")
t_train <- table(tab_train$spam, p_train)
(t_train[2] + t_train[3]) / sum(t_train) 
# 8.7% erreurs sur la base d'apprentissage
t_train[2] / sum(t_train) 
# non detection 5.9%
t_train[3] / sum(t_train)
# taux de fausse alarme 2.8%

p_test <- predict(tree, tab_test, type="class")
t_test <- table(tab_test$spam, p_test)
(t_test[2] + t_test[3]) / sum(t_test) 
# 10.3% erreurs sur la base de test
t_test[2] / sum(t_test)
# non detection 7.6%
t_test[3] / sum(t_test)
# taux de fausse alarme 2.7%
# le taux de non détection est plus éleve  que celui de fausse alarme
# on préfère laisser passer de spam que de classer en spam de email valides

# Aggregation de modeles. Bagging

bag <- bagging(spam ~ ., data=tab_train)
# La fonction genere 25 arbres par default

p_train <- predict(bag, tab_train, type="class")
t_train <- table(tab_train$spam, p_train)
(t_train[2] + t_train[3]) / sum(t_train) 
# 0.11% erreurs sur la base d'apprentissage
t_train[2] / sum(t_train) 
# non detection 0.09%
t_train[3] / sum(t_train)
# taux de fausse alarme 0.057%

p_test <- predict(bag, tab_test, type="class")
t_test <- table(tab_test$spam, p_test)
(t_test[2] + t_test[3]) / sum(t_test)
# 6.42% erreurs sur la base de test
t_test[2] / sum(t_test) 
# non detection 4.43%
t_test[3] / sum(t_test) 
# taux de fausse alarme 1.9%

# On obtient ici de meilleurs résultats vue que le bagging fait appel à plusieurs
# classifieurs, et utilise le système de vote pour trouver la bonne estimation


# Random Forest
tab = read.table('spam.txt', header=T, sep=';');
indtrain = read.table('indtrain.txt', header=T, sep=';');
train <- train <- tab[indtrain$indtrain,]
forest <- randomForest(spam ~ ., train)

p_train <- predict(forest, train, type="class")
t_train <- table(train$spam, p_train)

(t_train[2] + t_train[3]) / sum(t_train) 
# 0.34% erreurs sur la base d'apprentissage
t_train[2] / sum(t_train) 
# non detection 0.31%
t_train[3] / sum(t_train) 
# taux de fausse alarme 0.029%

p_test <- predict(forest, test, type="class")
t_test <- table(test$spam, p_test)

(t_test[2] + t_test[3]) / sum(t_test) 
# 1.7% erreurs sur la base de test
t_test[2] / sum(t_test) 
# taux de non detection: 1.4%
t_test[3] / sum(t_test) 
# taux de fausse alarme 0.26%

# Les performances sont encore meilleurs  par rapport au bagging
# un randomforest permet de créer des arbres plus indépendants en les échantillonant 
# à chaque noeud, ce qui permet une meilleur precision d'apprentissage.

varImpPlot(forest)
# Les variables importantes son celles correspondant à la fréquence des caractères "$" et "!"
# Elles maximisent le coefficient de Gini qui mesure la dispersion de nos données


# SCORING

# Classification avec un svm

Ytrain <- train[,ncol(train)]
train_bis <- train[,-ncol(train)]

svm <- ksvm(x=as.matrix(train_bis), y=as.factor(Ytrain), kernel="rbfdot", type='C-svc')

p_train <- predict(svm, train_bis, type="response")
t_train <- table(train$spam, p_train)
(t_train[2] + t_train[3]) / sum(t_train)
# 4.6% erreurs sur la base d'apprentissage
t_train[2] / sum(t_train) 
# taux de non detection: 3%
t_train[3] / sum(t_train) 
# taux de fausse alarme: 1.53%

p_test <- predict(svm, test[,-ncol(test)], type="response")
t_test <- table(test$spam, p_test)
(t_test[2] + t_test[3]) / sum(t_test) 
# 5.21% erreurs sur la base de test
t_test[2] / sum(t_test)
# taux de non detection: 3.6%
t_test[3] / sum(t_test) 
# taux de fausse alarme: 1.65%

# le svm semble être moins precis que les algorithmes utilisés précedemment
# La methode de Random Forest est plus adaptés pour traiter ce problème


# Comparaison des modèles de classification

n = 50
last_train <- matrix(0, n, 4)
last_test <- matrix(0, n, 4)
colnames(last_train) = c("arbre", "bagging", "randomForest", "svm")
colnames(last_test) = c("arbre", "bagging", "randomForest", "svm")

for (k in (1:n)) {
  samp <- sample(nrow(tab), floor(nrow(tab)*0.75), replace=FALSE)
  train <- tab[-samp,]
  test <- tab[samp,]
  
  # Creation des Classifications: 
  x_svm = as.matrix(train[,-ncol(train)])
  y_svm = as.factor(train[,ncol(train)])
  svm <- ksvm(x=x_svm, y=y_svm, kernel="rbfdot", type="C-svc")
  tree <- rpart('spam ~ .', data=train)
  bag <- bagging(spam ~ ., data=train)
  forest <- randomForest(spam ~ ., train)
  
  # Prediction des données Train
  p_svm <- predict(svm, train[,-ncol(train)], type="response")
  p_tree <- predict(tree, train, type="class")
  p_bag <- predict(bag, train, type="class")
  p_forest <- predict(forest, train, type="class")
  
  # Store les erreurs
  last_train[k,1] <- (table(train$spam, p_svm)[1,2] + table(train$spam, p_svm)[2,1]) / nrow(train)
  last_train[k,2] <- (table(train$spam, p_tree)[1,2] + table(train$spam, p_tree)[2,1]) / nrow(train)
  last_train[k,3] <- (table(train$spam, p_bag)[1,2] + table(train$spam, p_bag)[2,1]) / nrow(train)
  last_train[k,4] <- (table(train$spam, p_forest)[1,2] + table(train$spam, p_forest)[2,1]) / nrow(train)
  
  # Prediction des données Test
  p_svm <- predict(svm, test[,-ncol(test)], type="response")
  p_tree <- predict(tree, test, type="class")
  p_bag <- predict(bag, test, type="class")
  p_forest <- predict(forest, test, type="class")
  
  # Store les erreurs
  last_test[k,1] <- (table(test$spam, p_svm)[1,2] + table(test$spam, p_svm)[2,1]) / nrow(test)
  last_test[k,2] <- (table(test$spam, p_tree)[1,2] + table(test$spam, p_tree)[2,1]) / nrow(test)
  last_test[k,3] <- (table(test$spam, p_bag)[1,2] + table(test$spam, p_bag)[2,1]) / nrow(test)
  last_test[k,4] <- (table(test$spam, p_forest)[1,2] + table(test$spam, p_forest)[2,1]) / nrow(test)
}
# Calcul des moyennes des erreurs pour chaque algorithme:
colMeans(last_train)
colMeans(last_test)

# PLot du Graphique
boxplot(last_train, main="Spam training (size iteration n=50) ")
boxplot(last_test, main="Spam testing (size iteration n=50) ")

# Conclusion:
  # On obtient les meilleurs resultats sur le test set avec l'algorithme SVM  suivi du RandomForest
  # L'arbre et le bagging montrent quand a eux les resultats les moins bons




