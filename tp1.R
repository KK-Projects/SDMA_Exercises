setwd('C:/Users/Karl/Desktop/Karl/Centrale3A/SDMA/TP1/')

library(ggplot2)

#APPLICATION 1

tab=read.table('UsCrime.txt',header=TRUE)

nrow(tab)
#nbr observations = 47

pairs(tab)
cor(tab)
#on remarque que Ex0 et Ex1 sont tres corrélés (facteur de corrélation de 0.99358648)

#QUESTION 1

#On fit le modèle
res=lm('R~.',data=tab)
print(res)
summary(res)

# t-value est le test de Student: il indique pour chaque variable si le test est significatif ou pas
# Pr(>|t|) est la p-value: c'est la probabilité d'obtenir une t-value supérieure à celle obtenue, par chance, si l'hypothèse nulle est correcte.
attributes(res)

# Modele Explicite : R = -6.918e+02 + 1.040e+00 * Age + -8.308e+00 * S + ......

#QUESTION 2 

#R^2 = Adjusted R-squared = 0.6783 = il indique que 67% de la variation de la variable qu'on étudie (R)
#peut être expliqué par les variables explicatives séléctionnées.

# Test Fisher : La F-statistic teste si la combinaison des variables explicatives choisies expliquent la variation de la variable cible
# d'une manière qu'il serait peut probable d'obtenir par chance si l'hypothèse nulle est vraie.
#la p-value de ce test étant égale à 3.686e-07 << 0.001, le modèle est donc significatif.

#QUESTION 3

summary(res)
anova(res)
plot(res)

# D'après le test de student appliqué à chacune des variables, seules 5 sont significatives
# En effet la p-value pour les variables Age, Ed, U2, X et la constante sont assez faible
# On remarque cela grace aux étoiles, en effet *** signnifie que c'est tres proches de 0
# * signifie que l'on a un p-value proche de 0.01

confint(res, level = 0.95)
confint(res, level = 0.99)

# QUESTION 4

print(fitted(res))
# On plot les residuals pour chaque estimation
plot(fitted(res),residuals(res))
# on remarque que les residuals sont centrés sur 0 avec quelques valeurs extrêmes

# QUESTION 5

#l'erreur quadratique des résidus est de 15878.7
erreur_quadratique = sum(residuals(res)^2)
print(erreur_quadratique)
estim_var = erreur_quadratique / length(residuals(res))
print(estim_var)
plot(x=tab$R, y=res$residuals)
#on remarque que les residuals sont plutot négatifs pour R<100 et plutot positifs pour R>100


qqnorm(res$residuals)
qqline(res$residuals)
# le modele est approximativement conforme car le QQ plot montre que l'hypothese de la loi normale est une bonne approximation

shapiro.test(res$residuals)
# le shapiro test nous donne une p-value de 82%.
#on ne peut donc pas rejeter l'hypothèse que l'échantillon suive une loi normale.

