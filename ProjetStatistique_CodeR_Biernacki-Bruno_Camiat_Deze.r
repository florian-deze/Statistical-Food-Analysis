# PROJET STAT

###########################
# Importation des donnees #
###########################

setwd("C:/Users/User/Documents/Polytech/GIS Semestre 8/Projet Statistique")
food = read.csv("food.csv", header=TRUE, dec=".", sep=";", row.names=1)
# On verifie que l'importation s'est bien deroulee
head(food)
str(food)
attach(food)

foodQ = food
str(foodQ)

##########################################
# Statistiques univariees (descriptives) #
##########################################

summary(food)

# Ecarts types
apply(food, 2, sd)

# variable Energy
par(mfrow=c(1,2))
boxPlotEnergy = boxplot(Energy, main="Variable Energy"); boxPlotEnergy
#text(x=rep(1:5,each=5)+0.3,y=as.vector(boxPlotEnergy$stats),labels=as.vector(boxPlotEnergy$stats),col=c("green","blue","red","blue","green",cex=0.7))
plot(Energy,main="Variable Energy")
table(Energy)
par(mfrow=c(1,1))

# variable Protein
par(mfrow=c(1,2))
boxPlotProtein = boxplot(Protein, main="Variable Protein"); boxPlotProtein 
table(Protein)
plot(Protein, main="Variable Protein")
# valeur "350" : surement une valeur aberrante 
which(food$Protein == 350)
food$Protein[which(food$Protein == 350)] = NA
foodQ$Protein[which(foodQ$Protein == 350)] = NA
par(mfrow=c(1,1))

# variable Fat
par(mfrow=c(1,2))
boxPlotFat = boxplot(Fat, main="Variable Fat"); boxPlotFat
table(Fat)
plot(Fat, main="Variable Fat") # beaucoup de valeurs vers le 0
par(mfrow=c(1,1))

# variable Water
par(mfrow=c(1,2))
boxPlotWater = boxplot(Water, main="Variable Water"); boxPlotWater
#text(x=rep(1:5,each=5)+0.3,y=as.vector(boxPlotWater$stats),labels=as.vector(boxPlotWater$stats),col=c("green","blue","red","blue","green",cex=0.7))
table(Water)
plot(Water, main="Variable Water") # tres regulier
par(mfrow=c(1,1))

# variable VitaminA
par(mfrow=c(1,2))
boxPlotVitA = boxplot(VitaminA, main="Variable Vitamin A"); boxPlotVitA #on ne voit pas grand chose
plot(VitaminA, main="Variable Vitamin A") # valeurs concentrees vers 0, 2 valeurs extremes : a verifier si aberrantes ?
table(VitaminA) # on a 319 "0" : 68%
par(mfrow=c(1,1))

# variable VitaminB1
par(mfrow=c(1,2))
boxPlotVitB1 = boxplot(VitaminB1, main="Variable Vitamin B1"); boxPlotVitB1 
plot(VitaminB1, main="Variable Vitamin B1")
table(VitaminB1)
quantile(food$VitaminB1)
par(mfrow=c(1,1))

# variable VitaminB2
par(mfrow=c(1,2))
boxPlotVitB2 = boxplot(VitaminB2, main="Variable Vitamin B2"); boxPlotVitB2
plot(VitaminB2, main="Variable Vitamin B2")
table(VitaminB2)
which(food$VitaminB2 == 20) #valeur aberrante
food$VitaminB2[which(food$VitaminB2 == 20)] = NA
foodQ$VitaminB2[which(foodQ$VitaminB2 == 20)] = NA
par(mfrow=c(1,1))

# variable VitaminC
par(mfrow=c(1,2))
boxPlotVitC = boxplot(VitaminC, main="Variable Vitamin C"); boxPlotVitC
plot(VitaminC, main="Variable Vitamin C")
table(VitaminC)
par(mfrow=c(1,1))


###############################################
# Tranformation des variables en qualitatives #
###############################################

# Il faut au moins 24 indivudus par classes (5% de 469)

## ---- Variable Energy ----

outE = food$Energy
table(outE)
quantile(outE)

foodQ$Energy = cut(foodQ$Energy, breaks = c(0,65, 200, 320, 900))
levels(foodQ$Energy) = c("pasEnergique", "pauveEnEnergie", "energique", "richeEnEnergie")
table(foodQ$Energy)
foodQ$Energy

## ---- Variable Protein ----

outP = food$Protein[-which(food$Protein == 0)]
table(na.omit(outP))
quantile(na.omit(outP))

foodQ$Protein = cut(foodQ$Protein, breaks = c(-1, 0, 2, 8, 19, 45))
levels(foodQ$Protein) = c("pasProteine", "peuProteine", "proteine", "bienProteine", "tresProteine")
table(foodQ$Protein)
foodQ$Protein

## ---- Variable Fat ----

outF = food$Fat[-which(food$Fat == 0)]
table(outF)
quantile(outF)

foodQ$Fat = cut(foodQ$Fat, breaks = c(-1, 0,2.5, 10, 22, 100))
levels(foodQ$Fat) = c("pasGras", "peuGras", "gras", "bienGras", "tresGras")
table(foodQ$Fat)
foodQ$Fat

## ---- Variable Water ----

outW = food$Water
table(outW)
quantile(outW)

foodQ$Water = cut(foodQ$Water, breaks = c(-1, 29.75, 63, 80, 100))
levels(foodQ$Water) = c("pasDEau", "pauveEnEau", "eau", "RicheEnEeau")
table(foodQ$Water)
foodQ$Water

## ---- Variable VitaminA ----

outA = food$VitaminA[-which(food$VitaminA == 0)]
table(outA)
quantile(outA)

foodQ$VitaminA = cut(foodQ$VitaminA, breaks = c(-1,0,40,80,300,12000))
levels(foodQ$VitaminA) = c("pasVitamine", "peuVitamine", "vitamine", "bienVitamine","tresVitamine")
table(foodQ$VitaminA)
foodQ$VitaminA


## ---- Variable VitaminB1 ----

outB1 = food$VitaminB1[-which(food$VitaminB1 == 0)]
table(outB1)
quantile(outB1)

foodQ$VitaminB1 = cut(foodQ$VitaminB1, breaks = c(-1, 0, 0.05, 0.10, 0.20, 1.50))
levels(foodQ$VitaminB1) = c("pasVitamine", "peuVitamine", "vitamine", "bienVitamine","tresVitamine")
table(foodQ$VitaminB1)
foodQ$VitaminB1

## ---- Variable VitaminB2 ----

outB2 = food$VitaminB2[-which(food$VitaminB2 == 0)]
table(na.omit(outB2))
quantile(na.omit(outB2))

foodQ$VitaminB2 = cut(foodQ$VitaminB2, breaks = c(-1, 0,0.06, 0.1, 0.20, 4.20))
levels(foodQ$VitaminB2) = c("pasVitamine", "peuVitamine", "vitamine", "bienVitamine","tresVitamine")
table(foodQ$VitaminB2)
foodQ$VitaminB2

## ---- Variable VitaminC ----

outC = food$VitaminC[-which(food$VitaminC == 0)]
table(outC)
quantile(outC)

foodQ$VitaminC = cut(foodQ$VitaminC, breaks = c(-1, 0, 15, 25, 200))
levels(foodQ$VitaminC) = c("pasVitamine", "peuVitamine", "vitamine", "tresVitamine")
table(foodQ$VitaminC)
foodQ$VitaminC

## Modification des noms des colonnes
names(foodQ) = c("EnergyQ", "ProteinQ", "FatQ", "WaterQ", "VitaminAQ", "VitaminB1Q", "VitaminB2Q", "VitaminCQ")


##########################
# Statistiques bivariees #
##########################

# Matrice de correlations
corfood = cor(food, use="pairwise.complete.obs")
corfood

# Boxplots Energy, Protein, Water et Fat en fonction des vitamines

par(mfrow=c(2,2))
boxplot(food$Energy~foodQ$VitaminA,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Energy en fonction de la VitaminA")
boxplot(food$Energy~foodQ$VitaminB1,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Energy en fonction de la VitaminB1")
boxplot(food$Energy~foodQ$VitaminB2,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Energy en fonction de la VitaminB2")
boxplot(food$Energy~foodQ$VitaminC,col=c("red", "orange" , "yellow", "green"), main="Energy en fonction de la VitaminC")

par(mfrow=c(2,2))
boxplot(food$Protein~foodQ$VitaminA,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Protein en fonction de la VitaminA")
boxplot(food$Protein~foodQ$VitaminB1,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Protein en fonction de la VitaminB1")
boxplot(food$Protein~foodQ$VitaminB2,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Protein en fonction de la VitaminB2")
boxplot(food$Protein~foodQ$VitaminC,col=c("red", "orange" , "yellow", "green"), main="Protein en fonction de la VitaminC")

par(mfrow=c(2,2))
boxplot(food$Water~foodQ$VitaminA,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Water en fonction de la VitaminA")
boxplot(food$Water~foodQ$VitaminB1,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Water en fonction de la VitaminB1")
boxplot(food$Water~foodQ$VitaminB2,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Water en fonction de la VitaminB2")
boxplot(food$Water~foodQ$VitaminC,col=c("red", "orange" , "yellow", "green"), main="Water en fonction de la VitaminC")

par(mfrow=c(2,2))
boxplot(food$Fat~foodQ$VitaminA,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Fat en fonction de la VitaminA")
boxplot(food$Fat~foodQ$VitaminB1,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Fat en fonction de la VitaminB1")
boxplot(food$Fat~foodQ$VitaminB2,col=c("red", "orange" , "yellow", "green", "darkgreen"), main="Fat en fonction de la VitaminB2")
boxplot(food$Fat~foodQ$VitaminC,col=c("red", "orange" , "yellow", "green"), main="Fat en fonction de la VitaminC")

par(mfrow=c(1,1))


##############
# ACP et CAH #
##############

library(FactoMineR)

# On fait l'ACP en remplacant les 3 valeurs tres hautes de vitaminA(12000,12000,8000) par 1500 pour eviter de fausser le classement
food.corrige = food
food.corrige$VitaminA[food.corrige$VitaminA==12000]=1500
food.corrige$VitaminA[food.corrige$VitaminA==8000]=1500

res.pca = PCA(na.omit(food.corrige), ncp=3, scale.unit=TRUE, graph=TRUE)
res.pca$eig # 3 composantes principales (inertie moy > 1)

res.pca.hcpc = HCPC(res.pca,nb.clust=3) #on veut 3 classes
res.pca.hcpc$desc.var #description par variables
res.pca.hcpc$desc.axes #description par les axes
res.pca.hcpc$desc.ind #description par les individus

# Effectif des individus classe
table(res.pca.hcpc$data.clust$clust) 
# Liste des individus dans chaque classe
row.names(res.pca.hcpc$data.clust[which(res.pca.hcpc$data.clust$clust==2),])
row.names(res.pca.hcpc$data.clust[which(res.pca.hcpc$data.clust$clust==3),])
row.names(res.pca.hcpc$data.clust[which(res.pca.hcpc$data.clust$clust==1),])


##############
# ACM et CAH #
##############

# ACM
res.mca = MCA(na.omit(foodQ), ncp=16, graph=FALSE)
res.mca$eig

# CAH
res.mca.hcpc = HCPC(res.mca,nb.clust=3) #on veut 3 classes
res.mca.hcpc$desc.var #description par les variables
res.mca.hcpc$desc.axes #description par les axes
res.mca.hcpc$desc.ind #description par les individus

# Effectif des individus par classe
table(res.mca.hcpc$data.clust$clust)
# Liste des individus dans chaque classe
row.names(res.mca.hcpc$data.clust[which(res.mca.hcpc$data.clust$clust==1),])
row.names(res.mca.hcpc$data.clust[which(res.mca.hcpc$data.clust$clust==2),])
row.names(res.mca.hcpc$data.clust[which(res.mca.hcpc$data.clust$clust==3),])


##################
# Indice de Rand #
##################

# Renommer les classes pour que la classe i faite avec ACP corresponde a la classe i faite avec ACM 
# On utilise les numeros de classe 4, 5 et 6 pour eviter les confusions en faisant le lien entre les classes
classes.acp = res.pca.hcpc$data.clust$clust
classes.acm = res.mca.hcpc$data.clust$clust

classes.acp.renomme = as.vector(classes.acp)
classes.acp.renomme[classes.acp==1]=6
classes.acp.renomme[classes.acp==2]=4
classes.acp.renomme[classes.acp==3]=5
classes.acp.renomme

classes.acm.renomme = as.vector(classes.acm)
classes.acm.renomme[classes.acm==1]=4
classes.acm.renomme[classes.acm==2]=5
classes.acm.renomme[classes.acm==3]=6
classes.acm.renomme

# Calcul de l'indice de rand = mesure de classement identique entre les deux classifications
library(flexclust)
indice.rand = randIndex(table(classes.acp.renomme,classes.acm.renomme), correct=FALSE)
indice.rand #l'indice est eleve, donc les 2 classifications donnent des resultats similaires


##########################
# Analyse discriminante  #
##########################

library(MASS)
res.lda = lda(na.omit(res.pca.hcpc$data.clust[,-9]),grouping = res.pca.hcpc$data.clust$clust)
# Exemple d'utilisation pour predire la classe
predict(res.lda, newdata=c(350,5,15,30,10,0.1,0.1,5))
