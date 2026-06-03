#Regression Lineaire
################################################################"
CPS1985 <- read.csv("C:/Users/manon/OneDrive/Documents/CoursIsen/A3/S6/projet_bigdata/IRVE.csv",header = TRUE)
library(rgl)
modele1<-library(rgl)
modele1<-lm(puissance_nominale ~ nbre_pdc+paiement_cb, data=CPS1985)
summary(modele1)
plot(modele1)
###################################################################
#Regression logistique
predict.glm(model,data.frame(X1=1.25),type="response")
