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
estim_var = erreur_quadratique / length(residuals(res))
print(estim_var)
plot(x=tab$R, y=res$residuals)
#on remarque que les residuals sont plutot negatifs pour R<100 et plutot positifs pour R>100


qqnorm(res$residuals)
qqline(res$residuals)
# le modele est approximativement conforme car le QQ plot montre que l'hypothese de la loi normale est une bonne approximation

shapiro.test(res$residuals)
# le shapiro test nous donne une p-value de 82%.
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

# Ici on realise un nombre fini d'itérations avant d'arreter l'algorithme
# Il faudrait néanmoins pour généraliser utiliser un autre critère comme attendre que la norme entre deux itérations des paramètre
# à effectuer soit sous un certain seuil
# Le problème à résoudre est non-convexe et on peut se retrouver pris un minimum local, il faut généralement appliquer l'algorithme
# plusieurs fois avec des conditions aléatoires et garder le résultat avec la vraisemblance la plus élevée
# Parfois un k-means est appliqué pour déterminer les conditions intiales.

for (i in c(2, 3, 5)) {
  em_result <- em(as.vector(irm), i)
  gm <- gmdensity(seq(1:255), em_result$p, em_result$mu, em_result$sigma)
  hist(irm, freq=F, main=paste("EM with", i, "classes"), ylim = c(0,max(gm)))
  lines(gm)
}
# la segmentation semble pertinente en particulier pour deux classes comme c'était prévu

## Mélange de gaussiennes

# 2. Mixture de regression par l'algorithm EM

data = read.table("regression_double.txt", header=F, sep=';')
# Les données ont l'air de suivre deux droites différentes
# peut arriver si les données appartiennes à deux catégories différentes
# On ne peut pas utiliser une regression classique car celle ci minimize l'erreur
# des moindres carrées, la droite que l'on obtiendrait serait entre les deux droites

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
  #erreur de prédiction
  print(paste("Erreur de prédiction de classes pour", k, "itérations", mean(klass!=class)))
}

# L'erreur de classification n'est pas une fonction strictement décroissante
# C'est du au fait qu'on essaie d'optimiser un problème non-convexe et qu'il arrive
# qu'on se retrouve coince dans un minimum local
