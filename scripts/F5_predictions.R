#Regression Lineaire
################################################################"
irve <- read.csv("../../IRVE.csv",header = TRUE)
library(rgl)
modele1<-library(rgl)
modele1<-lm(puissance_nominale ~ toupper(gratuit),toupper(paiement_acte),toupper(reservation), data=irve)
summary(modele1)
plot(modele1)
###################################################################
#Regression logistique
predict.glm(model,data.frame(X1=1.25),type="response")
