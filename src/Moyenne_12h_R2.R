# Working data  
###
library(tidyverse)
library(xts)
library(lubridate)
library(ggplot2)
library(cowplot)

# load data : 
source("./src/Fusion_replicat.R")
# => listes ne contenant les données que pour les jours choisis. 


#5%
# 2. Delete days with a sum activity=0
sumR2daily <- list()
for (i in 1:length(CinqR2))
{
  sumR2daily[[i]] <- apply.daily(CinqR2[[i]], sum)
} 

## creation nouvelle liste avec selection des jours somme activité>0 
CinqR2bis <- list()
for (i in 1:30){ 
  for (j in 1:4){
    if ((isTRUE(sumR2daily[[i]][j]==0)==TRUE)){
      sel0 <- which(sumR2daily[[i]]==0)
      zero <- sel0[1]
      d <- date(time(sumR2daily[[i]][zero]))
      CinqR2bis[[i]] <- CinqR2[[i]][paste0("/",d)]
    }else {
      CinqR2bis[[i]] <- CinqR2[[i]]
    }
  }
}

## calcul moyenne par 12 heures 
activitymeanR2 <- list()
for (i in 1:length(CinqR2bis)) {
  activitymeanR2[[i]] <- apply.daily(CinqR2bis[[i]], FUN=mean)
}

# creation du DF pour calcul des moyennes (methode1)
meanR2DF <- data.frame()
for (i in 1:length(activitymeanR2))
{
  meanR2DF <- cbind(meanR2DF, activitymeanR2[[i]][,1])
}

## moyenne par ligne 
R5meanR2 <- as.xts(apply(meanR2DF, 1, mean, na.rm=TRUE))
plot(R5meanR2)


#0.5%
# 2. Delete days with a sum activity=0
sumR2daily05 <- list()
for (i in 1:length(ZeroCinqR2))
{
  sumR2daily05[[i]] <- apply.daily(ZeroCinqR2[[i]], sum)
} 

## creation nouvelle liste avec selection des jours somme activité>0 
ZeroCinqR2bis <- list()
for (i in 1:30){ 
  for (j in 1:4){
    if ((isTRUE(sumR2daily05[[i]][j]==0)==TRUE)){
      sel0 <- which(sumR2daily05[[i]]==0)
      zero <- sel0[1]
      d <- date(time(sumR2daily05[[i]][zero]))
      ZeroCinqR2bis[[i]] <- ZeroCinqR2[[i]][paste0("/",d)]
    }else {
      ZeroCinqR2bis[[i]] <- ZeroCinqR2[[i]]
    }
  }
}

##Calcul moyenne par 12 heures
activity0.5meanR2 <- list()
for (i in 1:length(ZeroCinqR2bis)) {
  activity0.5meanR2[[i]] <- apply.daily(ZeroCinqR2bis[[i]], FUN=mean)
}

# creation du DF pour calcul des moyennes (methode1)
meanR2DF0.5 <- data.frame()
for (i in 1:length(activity0.5meanR2))
{
  meanR2DF0.5 <- cbind(meanR2DF0.5, activity0.5meanR2[[i]][,1])
}

## moyenne par ligne 
R0.5meanR2 <- as.xts(apply(meanR2DF0.5, 1, mean, na.rm=TRUE))
plot(R0.5meanR2)