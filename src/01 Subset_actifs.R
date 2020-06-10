## Selection des database avec seulement les jours des vivants.
# sortie : DB pour R1, R2 5 et 05 et total

# load packages
library(tidyverse)
library(xts)
library(lubridate)
library(ggplot2)
library(cowplot)

# load data : 

source("./src/00 Subset_creation.R")

# R1 5%
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

# R2 5%

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

##### 0.5% #####
# R1 0.5 

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

## R2 0.5%

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


### TOTAL ####
#R1

sumdaily <- list()
for (i in 1:length(Cinqtot))
{
  sumdaily[[i]] <- apply.daily(Cinqtot[[i]], sum)
} 

Cinqtotbis <- list()
for (i in 1:length(sumdaily)){ 
  for (j in 1:4){
    if ((isTRUE(sumdaily[[i]][j]==0)==TRUE)){
      sel0 <- which(sumdaily[[i]]==0)
      zero <- sel0[1]
      d <- date(time(sumdaily[[i]][zero]))
      Cinqtotbis[[i]] <- Cinqtot[[i]][paste0("/",d)]
    }else {
      Cinqtotbis[[i]] <- Cinqtot[[i]]
    }
  }
}


# R2 

sumdaily05 <- list()
for (i in 1:length(ZeroCinqtot))
{
  sumdaily05[[i]] <- apply.daily(ZeroCinqtot[[i]], sum)
} 

## creation nouvelle liste avec selection des jours somme activité>0 

ZeroCinqtotbis <- list()
for (i in 1:length(ZeroCinqtot)){ 
  for (j in 1:4){
    if ((isTRUE(sumdaily05[[i]][j]==0)==TRUE)){
      sel0 <- which(sumdaily05[[i]]==0)
      zero <- sel0[1]
      d <- date(time(sumdaily05[[i]][zero]))
      ZeroCinqtotbis[[i]] <- ZeroCinqtot[[i]][paste0("/",d)]
    }else {
      ZeroCinqtotbis[[i]] <- ZeroCinqtot[[i]]
    }
  }
}

rm(sumdaily)
rm(sumdaily05)
rm(sumR1daily)
rm(sumR1daily05)
rm(sumR2daily)
rm(sumR2daily05)




