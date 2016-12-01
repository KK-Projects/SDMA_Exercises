
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


# Application II : agr ́egation de mod`eles



