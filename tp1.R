# setwd('C:/Users/Karl/Desktop/Karl/Centrale3A/SDMA/TP1/')

#APPLICATION 1

tab=read.table('UsCrime.txt',header=TRUE)

nrow(tab)
#nbr observations = 47

pairs(tab)
cor(tab)
#on remarque que Ex0 et Ex1 sont tres correles (facteur de correlation de 0.99358648)

#QUESTION 1

#On fit le modele
res=lm('R~.',data=tab)
print(res)
summary(res)

# t-value est le test de Student: il indique pour chaque variable si le test est significatif ou pas
# Pr(>|t|) est la p-value: c'est la probabilite d'obtenir une t-value superieure a celle obtenue, par chance, si l'hypothese nulle est correcte.
attributes(res)

# Modele Explicite : R = -6.918e+02 + 1.040e+00 * Age + -8.308e+00 * S + ......

#QUESTION 2 
attributes(res)

#R^2 = Adjusted R-squared = 0.6783 = il indique que 67% de la variation de la variable qu'on ?tudie (R)
#peut etre explique par les variables explicatives selectionnees.

# Test Fisher : La F-statistic teste si la combinaison des variables explicatives choisies expliquent la variation de la variable cible
# d'une maniere qu'il serait peut probable d'obtenir par chance si l'hypothese nulle est vraie.
#la p-value de ce test etant egale a 3.686e-07 << 0.001, le modele est donc significatif.

#QUESTION 3

summary(res)
anova(res)
plot(res)

# D'apres le test de student applique a chacune des variables, seules 5 sont significatives
# En effet la p-value pour les variables Age, Ed, U2, X et la constante sont assez faible
# On remarque cela grace aux ?toiles, en effet *** signnifie que c'est tres proches de 0
# * signifie que l'on a un p-value proche de 0.01

confint(res, level = 0.95)
confint(res, level = 0.99)

# QUESTION 4

print(fitted(res))
# On plot les residuals pour chaque estimation
plot(fitted(res),residuals(res))
# on remarque que les residuals sont centres sur 0 avec quelques valeurs extremes

# QUESTION 5

#l'erreur quadratique des residus est de 15878.7
erreur_quadratique = sum(residuals(res)^2)
print(erreur_quadratique)
estim_var = sqrt( erreur_quadratique / length(residuals(res)) )
print(estim_var)
plot(x=tab$R, y=res$residuals)
#on remarque que les residuals sont plutot negatifs pour R<100 et plutot positifs pour R>100


qqnorm(res$residuals)
qqline(res$residuals)
# le modele est approximativement conforme car le QQ plot montre que l'hypothese de la loi normale est une bonne approximation

shapiro.test(res$residuals)
# le shapiro test nous donne une p-value de 82%.
<<<<<<< HEAD
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

=======
#on ne peut donc pas rejeter l'hypothese que l'echantillon suive une loi normale.

## APPLICATION 2:

# QUESTION 1
irm = as.matrix(read.table("irm_thorax.txt", header=F, sep=';'))

# QUESTION 2
image(irm)
hist(irm)

# QUESTION 3
gmdensity <- function(x, p, m, sigma) {
  n <- length(p)
  dens <- 0
  p <- p / sum(p)
  for (i in 1:n) {
    dens <- dens + p[i]*dnorm(x, m[i], sigma[i])
  }
  return(dens)
}

em <- function(x, N) {
  n <- length(x)
  p <- rep(1/N, N)
  sigma <- sqrt(rep(var(x), N));
  mu <- sample(x, N)
  
  for (j in 1:50) {
    # E step
    res <- matrix(0, N, n) 
    for (k in 1:N) {
      res[k,] <- p[k] * dnorm(x, mu[k], sigma[k]) / gmdensity(x, p, mu, sigma)
    }
    
    # M step
    for (k in (1:N)) {
      mu[k] <- sum(x * res[k,]) / sum(res[k,])
      p[k] <- sum(res[k,]) / sum(res)
      sigma[k] <- sqrt(sum(((x - mu[k])**2)*res[k,])/ sum(res[k,]))
    }
  }
  return(list(p=p, mu=mu, sigma=sigma))
}

# Ici on realise un nombre fini d'itÃ©rations avant d'arreter l'algorithme
# Il faudrait nÃ©anmoins pour gÃ©nÃ©raliser utiliser un autre critÃ¨re comme attendre que la norme entre deux itÃ©rations des paramÃ¨tre
# Ã  effectuer soit sous un certain seuil
# Le problÃ¨me Ã  rÃ©soudre est non-convexe et on peut se retrouver pris un minimum local, il faut gÃ©nÃ©ralement appliquer l'algorithme
# plusieurs fois avec des conditions alÃ©atoires et garder le rÃ©sultat avec la vraisemblance la plus Ã©levÃ©e
# Parfois un k-means est appliquÃ© pour dÃ©terminer les conditions intiales.

for (i in c(2, 3, 5)) {
  em_result <- em(as.vector(irm), i)
  gm <- gmdensity(seq(1:255), em_result$p, em_result$mu, em_result$sigma)
  hist(irm, freq=F, main=paste("EM with", i, "classes"), ylim = c(0,max(gm)))
  lines(gm)
}
# la segmentation semble pertinente en particulier pour deux classes comme c'Ã©tait prÃ©vu

## MÃ©lange de gaussiennes

# 2. Mixture de regression par l'algorithm EM

data = read.table("regression_double.txt", header=F, sep=';')
# Les donnÃ©es ont l'air de suivre deux droites diffÃ©rentes
# peut arriver si les donnÃ©es appartiennes Ã  deux catÃ©gories diffÃ©rentes
# On ne peut pas utiliser une regression classique car celle ci minimize l'erreur
# des moindres carrÃ©es, la droite que l'on obtiendrait serait entre les deux droites

reg <- regmixEM(data$V2, data$V1)
plot(reg, whichplots=2)
plot(reg, whichplots=1)

# We get the class of each component and calculate the residue
klass = apply(reg$posterior, FUN=which.max, MARGIN=1)
class1 = reg$beta[1,1] + reg$beta[2,1]*data$V1
class2 = reg$beta[1,2] + reg$beta[2,2]*data$V1
residue = ((class1-data$V2)**2) * (klass==1) + ((class2-data$V2)**2) * (klass==2)
mse = sqrt(sum(residue)) # 275.06

par(mfrow=c(1,1))
for (k in c(1,3,5)) {
  reg <- regmixEM(data$V2, data$V1, maxit=k)
  
  class = apply(reg$posterior, FUN=which.max, MARGIN=1)
  c1 = reg$beta[1,1] + reg$beta[2,1]*data$V1
  c2 = reg$beta[1,2] + reg$beta[2,2]*data$V1
  residue = ((c1-data$V2)**2) * (class==1) + ((c2-data$V2)**2) * (class==2)
  
  plot(data$V1[class==1], data$V2[class==1], col=2, xlab="y", ylab="x",main=paste("EM regression", k," iterations"), ylim=c(-5,100))
  lines(data$V1[class==2], y=data$V2[class==2], col=3,type="p")
  lines(data$V1, c1, col=2)
  lines(data$V1, c2, col=3)
  
  table(klass, class)
  #erreur de prÃ©diction
  print(paste("Erreur de prÃ©diction de classes pour", k, "itÃ©rations", mean(klass!=class)))
}

# L'erreur de classification n'est pas une fonction strictement dÃ©croissante
# C'est du au fait qu'on essaie d'optimiser un problÃ¨me non-convexe et qu'il arrive
# qu'on se retrouve coince dans un minimum local
>>>>>>> master
