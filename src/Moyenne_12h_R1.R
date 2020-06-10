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
sumR1daily <- list()
for (i in 1:length(CinqR1))
{
  sumR1daily[[i]] <- apply.daily(CinqR1[[i]], sum)
} 

## creation nouvelle liste avec selection des jours somme activité>0 
CinqR1bis <- list()
for (i in 1:30){ 
  for (j in 1:4){
    if ((isTRUE(sumR1daily[[i]][j]==0)==TRUE)){
      sel0 <- which(sumR1daily[[i]]==0)
      zero <- sel0[1]
      d <- date(time(sumR1daily[[i]][zero]))
      CinqR1bis[[i]] <- CinqR1[[i]][paste0("/",d)]
    }else {
      CinqR1bis[[i]] <- CinqR1[[i]]
    }
  }
}
## ces deux boucles n'était pas a toucher car elles permettent juste de selectionner les jours où les ID sont vivants. 

## calcul moyenne par 12 heures 
#activitymeanR1 <- list()
#for (i in seq_along(CinqR1bis)) {
 # activitymeanR1[[i]] <- ts_ma(CinqR1bis[[i]], n = 720, plot = FALSE)[[1]]
#}

# ne fonctionne pas car les ID n'ont pas des series de même longeur comme on a selectionné que les jours où les moustiques sont vivant. 

# ici il vaut mieux utiliser la fonction aggregate ou apply.daily directement 
activitymeanR1 <- list()
for (i in 1:length(CinqR1bis)) {
  activitymeanR1[[i]] <- apply.daily(CinqR1bis[[i]], FUN=mean)
}

# creation du DF pour calcul des moyennes (methode1)
meanR1DF <- data.frame()
for (i in 1:length(activitymeanR1))
{
  meanR1DF <- cbind(meanR1DF, activitymeanR1[[i]][,1])
}

## moyenne par ligne 
R5mean <- as.xts(apply(meanR1DF, 1, mean, na.rm=TRUE))
plot(R5mean)

################
## faire de même pour R2 et si tu as bien compris pour 0.5% R1 et R2. 
################


#0.5%
# 2. Delete days with a sum activity=0
sumR1daily05 <- list()
for (i in 1:length(ZeroCinqR1))
{
  sumR1daily05[[i]] <- apply.daily(ZeroCinqR1[[i]], sum)
} 

## creation nouvelle liste avec selection des jours somme activité>0 
ZeroCinqR1bis <- list()
for (i in 1:30){ 
  for (j in 1:4){
    if ((isTRUE(sumR1daily05[[i]][j]==0)==TRUE)){
      sel0 <- which(sumR1daily05[[i]]==0)
      zero <- sel0[1]
      d <- date(time(sumR1daily05[[i]][zero]))
      ZeroCinqR1bis[[i]] <- ZeroCinqR1[[i]][paste0("/",d)]
    }else {
      ZeroCinqR1bis[[i]] <- ZeroCinqR1[[i]]
    }
  }
}

##Calcul moyenne par 12 heures
activity0.5meanR1 <- list()
for (i in 1:length(ZeroCinqR1bis)) {
  activity0.5meanR1[[i]] <- apply.daily(ZeroCinqR1bis[[i]], FUN=mean)
}

# creation du DF pour calcul des moyennes (methode1)
meanR1DF0.5 <- data.frame()
for (i in 1:length(activity0.5meanR1))
{
  meanR1DF0.5 <- cbind(meanR1DF0.5, activity0.5meanR1[[i]][,1])
}

## moyenne par ligne 
R0.5mean <- as.xts(apply(meanR1DF0.5, 1, mean, na.rm=TRUE))
plot(R0.5mean)

