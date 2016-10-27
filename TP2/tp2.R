##TP2 SDMA
# BERRADA Karim
# YALA Marouane
# BOU ABBOUD Karl

##setwd(dir = "C:/Users/Karl/Desktop/Karl/Centrale3A/SDMA/TP/TP2")


## Application 1 : Regression logistique

#1.
tab = read.table("SAHeart.txt",header=T,sep=",")

# D'apres le fichier info:
# sbp  	systolic blood pressure
# tobacco		cumulative tobacco (kg)
# ldl		low densiity lipoprotein cholesterol
# famhist		family history of heart disease (Present, Absent)
# obesity
# alcohol		current alcohol consumption
# age		age at onset
# chd		response, coronary heart disease

tab = tab[,-1]

#2.
pairs(tab,pch=22,bg=c("red","blue")[unclass(factor(tab[,"chd"]))])

# 3.
glm_fit <- glm(chd~.,family="binomial",data=tab)
summary(glm_fit)

# 4.
prediction <- predict(glm_fit, tab, type="response")
glm_pred <- rep(0,length(prediction))
glm_pred[prediction >.5] <- 1

conf_matrix <- table(true=tab$chd,pred=glm_pred)
print(conf_matrix)

# faux positifs
conf_matrix[1,2] / sum(conf_matrix[1,]) # P(pred=1 | true=0) = 15%

# faux negatifs
conf_matrix[2,1] / sum(conf_matrix[2,]) # P(pred=0 | true=1) = 48%

# Le taux de faux negatif est tres important (48%), ce qui est mauvais pour un modele
# cense detecter les cas de risques eleves de developpement de la maladie.

# 5.
train_err <- rep(0,100)
test_err <- rep(0,100)

for (i in 1:100) {
  samp <- sample(nrow(tab), nrow(tab)*0.75)
  train <- tab[samp,]
  test <- tab[-samp,]
  
  glm.fit <- glm(chd~.,family="binomial",data=train)
  
  probs_train <- predict(glm.fit, train, type="response")
  pred_train <- rep(0,length(train))
  pred_train[probs_train >.5] <- 1
  table_train <- table(train$chd, pred_train)
  
  probs_test <- predict(glm.fit, test, type="response")
  pred_test <- rep(0,length(test))
  pred_test[probs_test >.5] <- 1
  table_test <- table(test$chd, pred_test)
  
  train_err[i] <- (table_train[1,2]+table_train[2,1]) / nrow(train)
  test_err[i] <- (table_test[1,2]+table_test[2,1]) / nrow(test)
}

train_err
test_err

mean(train_err) # 10%
mean(test_err) # 12.1%

max(train_err) # 12.4%
max(test_err) # 18.9%

min(train_err) # 7.5%
min(test_err) # 5.1%

par(mfrow=c(1,2))
boxplot(train_err)
boxplot(test_err)
par(mfrow=c(1,1))

# Cette etude nous permet de determiner la sensibilite de notre modele aux donnees d'apprentissage

# 6.
step_ <- step(glm.fit, direction="both")
summary(step_)

# Les variables significatives choisies par le modele sont tobacco, ldl, famhist, typea, obesity, age
# Les coefficients les plus signicatifs sont l'age et ldl
# la moins significative est obesity
# L'alcool n'est donc pas percu comme un facteur significatif de notre modele, ce qui peut paraitre etonnant

# 7.
library(ROCR)

samp <- sample(nrow(tab), nrow(tab)*0.75)
train <- tab[samp,]
test <- tab[-samp,]

# Modele complet
glm.fit <- glm(chd~.,family="binomial",data=train)
glm.pred <- predict(glm.fit, test)
pred_test <- prediction(glm.pred, test$chd)
full <- performance(pred_test, measure="tpr", x.measure="fpr")

# Modele step
step.fit <- step(glm.fit, direction="both")
step.pred <- predict(step.fit, test)
pred_test <- prediction(step.pred, test$chd)
step_ <- performance(pred_test, measure="tpr", x.measure="fpr")

# Modele une variable explicative
glm.fit <- glm(chd~age,family = "binomial",data=train)
glm.pred <- predict(glm.fit, test)
pred_test <- prediction(glm.pred, test$chd)
var1 <- performance(pred_test, measure="tpr", x.measure="fpr")

plot(full,col="red")
par(new=TRUE)
plot(step_,col="green")
par(new=TRUE)
plot(var1,col="blue")

# Meme chose pour les donnees d'apprentissage

# Modele complet
glm.fit <- glm(chd~.,family="binomial",data=train)
glm.pred <- predict(glm.fit, train)
pred_test <- prediction(glm.pred, train$chd)
full <- performance(pred_test, measure="tpr", x.measure="fpr")

# Modele step
step.fit <- step(glm.fit, direction="both")
step.pred <- predict(step.fit, train)
pred_test <- prediction(step.pred, train$chd)
step_ <- performance(pred_test, measure="tpr", x.measure="fpr")

# Modele une variable explicative
glm.fit <- glm(chd~age,family = "binomial",data=train)
glm.pred <- predict(glm.fit, train)
pred_test <- prediction(glm.pred, train$chd)
var1 <- performance(pred_test, measure="tpr", x.measure="fpr")

plot(full,col="red")
par(new=TRUE)
plot(step_,col="green")
par(new=TRUE)
plot(var1,col="blue")





## Application 2 : classification par SVM

#1. etude du fichier info

#2.
tab = read.table('spam.txt', header=T, sep=';')

#3.
nrow(tab)
ncol(tab)
names(tab)
# 4601 observations de 58 variables
# 57 variables numeriques et le label spam/non-spam

#4.
head(tab)
# Head(tab) retourne les cinq premieres lignes, la variable cible est la derniere

#5.
Y <- tab[,ncol(tab)]
levels(Y) # [1] "email" "spam"
nlevels(Y) # 2
Y_tab <- table(Y) 
# email   spam
#  2788   1813

# email percentage
Y_tab[["email"]] / sum(Y_tab) # 0.60

# spam percentage
Y_tab[["spam"]] / sum(Y_tab) # 0.60

#6.
train_ind = sample(1:nrow(tab),0.75*nrow(tab))

Xtrain <- tab[train_ind, -ncol(tab)]
Ytrain <- tab[train_ind, ncol(tab)]
Xtest <- tab[-train_ind, -ncol(tab)]
Ytest <- tab[-train_ind, ncol(tab)]

#7.
library(kernlab)
classif <- ksvm(x=as.matrix(Xtrain), y=as.factor(Ytrain), kernel="rbfdot", type='C-svc')

#8. Effectuer la prediction sur le train data set
Ypred_train <- predict(classif, Xtrain)
confusion <- table(Ytrain, Ypred_train)
error_train <- (confusion[1,2] + confusion[2,1]) / sum(confusion) # 4.50% d'erreurs
# Le pourcentage de bonnes detections est 100 - 4.50 = 95.50%
error_email <- confusion[3] / (confusion[1] + confusion[3]) # 2.70%
error_spam <- confusion[2] / (confusion[2] + confusion[4]) # 7.20%

#9. Effectuer la prediction sur le test data set
Ypred_test <- predict(classif, Xtest)
confusion <- table(Ytest, Ypred_test)
error_test <- (confusion[2] + confusion[3]) / length(Ytest) # 6.5% d'erreurs
# Le pourcentage de bonnes detections est 100 - 6.5 = 93.5%
error_email <- confusion[3] / (confusion[1] + confusion[3]) # 3.4%
error_spam <- confusion[2] / (confusion[2] + confusion[4]) # 11%

#10. Effectuer la meme chose 20 fois

error_vector <- numeric(length=20)

for (i in 1:20) {
  train_ind <- sample(1:nrow(tab),0.75*nrow(tab))
  
  Xtrain <- tab[train_ind, -ncol(tab)]
  Ytrain <- tab[train_ind, ncol(tab)]
  Xtest <- tab[-train_ind, -ncol(tab)]
  Ytest <- tab[-train_ind, ncol(tab)]
  
  classif <- ksvm(x=as.matrix(Xtrain), y=as.factor(Ytrain), kernel="rbfdot", type='C-svc')
  Ypred_test <- predict(classif, Xtest)
  confusion <- table(Ytest, Ypred_test)
  error_vector[i] <- (confusion[2] + confusion[3]) / length(Ytest)
}

# Mean 6.8%
mean(error_vector)
# Max 8.5%
max(error_vector)
# Min 5.9%
min(error_vector)

hist(error_vector, nclass=7, main="Erreur de classification pour noyau Gaussien")
boxplot(error_vector)
# La distribution semble suivre une distribution gaussienne

# Test des autres noyaux
for (kern in c('vanilladot', 'polydot')) {
  error_vector <- numeric(length=20)
  for (i in 1:20) {
    train_ind <- sample(seq_len(nrow(tab)), size=smp_size)
    
    Xtrain <- tab[train_ind, -ncol(tab)]
    Ytrain <- tab[train_ind, ncol(tab)]
    Xtest <- tab[-train_ind, -ncol(tab)]
    Ytest <- tab[-train_ind, ncol(tab)]
    
    classif <- ksvm(x=as.matrix(Xtrain), y=as.factor(Ytrain), kernel=kern, type='C-svc')
    Ypred_test <- predict(classif, Xtest)
    confusion <- table(Ytest, Ypred_test)
    error_vector[i] <- (confusion[2] + confusion[3]) / length(Ytest)
  }
  print(paste("Erreur pour noyau ", kern, " : ", mean(error_vector)))
  print(paste("Max Erreur pour noyau ", kern, " : ", max(error_vector)))
  print(paste("Min Erreur pour noyau ", kern, " : ", min(error_vector)))
  hist(error_vector, nclass=7, main=paste("Erreur de classification pour noyau ", kern))
}

# Le noyau lineaire semble apporter de meilleurs resultats (moyennes equivalentes mais variance plus faible)
# C'est probablement du au fait que la frontiere de separation entre les deux classes peut etre assez bien 
# approximee par un noyau lineaire et le noyau polynomial en ajoutant des parametres fait du surapprentissage






# Application 3 : Modele de regression par SVM


crime_data = read.table("UsCrime.txt",header=T)

for (kern in c('rbfdot','vanilladot', 'polydot')) {
  
  error_vector = rep(0,20)
  for (i in 1:20) {
    train_index = sample(1:nrow(crime_data), 0.75*nrow(crime_data))
    
    X_train = crime_data[train_index,-1]
    Y_train = crime_data[train_index,1]
    X_test = crime_data[-train_index,-1]
    Y_test = crime_data[-train_index,1]
    
    model = ksvm(x=as.matrix(X_train), y=as.vector(Y_train), type="eps-svr", kernel=kern)
    pred = predict(model,X_test)
    
    error_vector[i] = sqrt( sum( (as.vector(pred) - Y_test)^2 ) )
  }
  print(paste("Erreur pour noyau ", kern, " : ", mean(error_vector)))
  print(paste("Max Erreur pour noyau ", kern, " : ", max(error_vector)))
  print(paste("Min Erreur pour noyau ", kern, " : ", min(error_vector)))
  hist(error_vector, nclass=7, main=paste("Erreur de classification pour noyau ", kern))
}

#le modele le plus performant semble etre le modele polynomial car la moyenne de son erreur quadratique 
# est la plus basse, et la variance des erreurs est egalement plus resseree.



