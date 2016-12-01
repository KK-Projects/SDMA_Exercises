
tab <- read.table("usa_indicators.txt", sep=";", header=T)
dim(tab)

# Il y a 14 observations pour 110 indicateurs ici p > n 
# Le problème de regression n'est pas simple, la matrice de condionnement
# est non inversible et l'estimateur des moindres carrées instable

