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
estim_var = sqrt( erreur_quadratique / length(residuals(res)) )
print(estim_var)
plot(x=tab$R, y=res$residuals)
#on remarque que les residuals sont plutot négatifs pour R<100 et plutot positifs pour R>100


qqnorm(res$residuals)
qqline(res$residuals)
# le modele est approximativement conforme car le QQ plot montre que l'hypothese de la loi normale est une bonne approximation

shapiro.test(res$residuals)
# le shapiro test nous donne une p-value de 82%.
#on ne peut donc pas rejeter l'hypothèse que l'échantillon suive une loi normale.

# QUESTION 6

indTest = seq(1,nrow(tab),by=3)
print(indTest)

# on crée la data de test
tabTest = tab[indTest,]
print(head(tabTest))

# on crée la data d'entrainement
tabTrain = tab[(1:nrow(tab))[! 1:nrow(tab) %in% indTest],]
print(head(tabTrain))

# on construit un modèle sur la data d'entrainement
model=lm('R~.',data=tabTrain)
print(model)
summary(model)

# on prédit des estimations sur la data de test
tabTestClean = subset(tabTest, select=-c(R))
prediction=predict(model,tabTestClean)
print(prediction)

# on calcule l'erreur quadratique moyenne
realite = tabTest$R
erreur = prediction - realite
err_quad_moy = sum(erreur^2) / length(realite)
print(err_quad_moy)
ecart_type = sqrt(err_quad_moy)
print(ecart_type)
# on remarque que l'écart type est deux fois plus élevé que dans les questions précédentes. 
# le modèle prédit donc moins bien de nouvelles données.

# QUESTION 7

x11()
par(mfrow=c(2,2))
plot(res)

#residuals vs fitted : on remarque que les erreur sont en moyenne autour de zero
#normal QQ : on remarque que les points sont alignés avec la première bisectrice, le modèle est donc assez proche de la réalité
#scale location: les erreurs sont plus grandes près de la valeur moyenne de la variable à prédire.
#residuals vs leverage: tous les points sont à l'intérieur de la distance de cook

#on remarque dans ces quatre figures que l'observation 11 pose problème, il serait judicieux de refaire l'analyse en excluant cette observation.

