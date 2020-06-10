## Methode du papier de Upshur. 

# 1: somme activité sur 10mins pour chaque ID
# 2: moyenne d'activité pour tous les ID d'un même groupe => graphiques 
# 3: comparaison de moyenne des groupes toutes les 10 min. (moyenne géométrique)
# NB: je ne vois pas comment ils ont appliqué le pairwise avec une moyenne de williams. 
# Avantage de cette méthode : moins de points =moins de 0 et meilleure gestion de la variance à certains point. 
# => detection de différence possible pour certains points. 
# je validerai cette méthode. 

# load packages
library(tidyverse)
library(xts)
library(lubridate)
library(ggplot2)
library(cowplot)

# load data : 

source("./src/01 Subset_actifs.R")

## Preparation des données. 

# 1. sum sur 10 min 

### 5% 

cinqsum <- list()
for (i in 1:length(Cinqtotbis))
{
  Cinqtotbis[[i]]$freq <- rep(1:433, each=10, length.out=length(Cinqtotbis[[i]][,1]))
  cinqsum[[i]] <- fortify.zoo(aggregate(Cinqtotbis[[i]][,1], by=Cinqtotbis[[i]]$freq, FUN = sum))
  colnames(cinqsum[[i]]) = c("Index", "Value")
  cinqsum[[i]] <- as.xts(cinqsum[[i]])
  
}

### 0.5%

zerocinqsum <- list()
for (i in 1:length(ZeroCinqtotbis))
{
  ZeroCinqtotbis[[i]]$freq <- rep(1:433, each=10, length.out=length(ZeroCinqtotbis[[i]][,1]))
  zerocinqsum[[i]] <- aggregate(ZeroCinqtotbis[[i]][,1], by=ZeroCinqtotbis[[i]]$freq, FUN = sum)
  zerocinqsum[[i]] <- fortify.zoo(zerocinqsum[[i]])
  colnames(zerocinqsum[[i]]) = c("Index","Value")
  zerocinqsum[[i]] <- as.xts(zerocinqsum[[i]])
}

# Creation objet sur lequel on vas pouvoir calculer les moyennes pour tous les ID du regime 5% 
R5sum <- data.frame()
for (i in 1:length(cinqsum))
{
  R5sum<- cbind(R5sum, cinqsum[[i]][,2])
}

R05sum <- data.frame()
for (i in 1:length(zerocinqsum))
{
  R05sum<- cbind(R05sum, zerocinqsum[[i]][,2])
}

# DF en ligne 

R5sum2 <- data.frame()
for (i in 1:length(cinqsum))
{
 cinqsumloc <-fortify.zoo(cinqsum[[i]][,2])
  R5sum2<- rbind(R5sum2, cinqsumloc)
}

R5sum2$Food <- rep("5%")


R05sum2 <- data.frame()
for (i in 1:length(zerocinqsum))
{
  zerocinqsumloc <-fortify.zoo(zerocinqsum[[i]][,2])
  R05sum2<- rbind(R05sum2, zerocinqsumloc)
}

R05sum2$Food <- rep("0.5%")

#DF tot 
Rsumtot <- rbind(R05sum2, R5sum2)# plutôt pour tab GLM 

## 2. moyenne sur les sommes d'activité pour tous les ID 
### 5% 
R5meansum <- as.xts(apply(R5sum, 1, mean, na.rm=TRUE))

### 0.5%
R05meansum <- as.xts(apply(R05sum, 1, mean, na.rm=TRUE))

## pour faire les graphs  il nous faut un DF regroupant les moyenne des 2 régimes 

R5meansum2 <- fortify.zoo(R5meansum)
head(R5meansum2)
R5meansum2$Food <- rep("5%")
colnames(R5meansum2) <- c("Time", "Mean", "Food")

R05meansum2 <- fortify.zoo(R05meansum)
head(R05meansum2)
R05meansum2$Food <- rep("0.5%")
colnames(R05meansum2) <- c("Time", "Mean", "Food")

# DF complet
meansumtot <- rbind(R5meansum2, R05meansum2)
head(meansumtot)

## graph couplé
library(ggplot2)
ggplot(meansumtot, aes(x = Time, y = Mean)) + 
  geom_line(aes(color = Food), size = 0.5) +
  theme_minimal()

# t.test global
t.test(Rsumtot$Value~Rsumtot$Food, na.rm=TRUE)
pairwise.t.test(Rsumtot$Value,Rsumtot$Food, na.rm=TRUE)# même reponse : pas signif en fonction du régime global. 
# pval=0.21

## t.test par minute 
### faut un dataframe avec chq colone = 10min et en ligne les ID 

R05sumst <- as.data.frame(t(R05sum))
R5sumst <- as.data.frame(t(R5sum))

R05sumst$Food <- rep("0.5%")
R5sumst$Food <- rep("5%")

meancomp <- rbind(R5sumst, R05sumst)
meancomp$Food <- as.factor(meancomp$Food)
head(meancomp)

## pairwise.t.test par colonne

respwt <- vector()
for (i in 1:ncol(meancomp))
{
  respwt[i] <-  pairwise.t.test(meancomp[,i], meancomp$Food, p.adj = "bonf")$p.value
}
listmin <- which(respwt<=0.05)
pvallistmin <- respwt[respwt<=0.05]# faudrai enlever les NA pour que ça soit plus propre
timdiff <- colnames(meancomp[,listmin])# temps où ya une différence entre les deux régimes
library(lubridate)
timdiff <- ymd_hms(timdiff)
## graph avec des droites au points différents 
class(Time)

p1 <- ggplot(meansumtot, aes(x = as.Date(Time, y = Mean)) + 
  geom_line(aes(color = Food), size = 0.5) +
  theme_minimal())
  
p2 <- ggplot(meansumtot, aes(x = Time, y = Mean)) + 
  geom_line(aes(color = Food), size = 0.5) +
  theme_minimal()+
  geom_vline(xintercept = timdiff, size=0.3)

plot_grid(p1, p2, nrow=2)


##### Réplicat 1 #####

### 5% 

cinqsumR1 <- list()
for (i in 1:length(CinqR1bis))
{
  CinqR1bis[[i]]$freq <- rep(1:433, each=10, length.out=length(CinqR1bis[[i]][,1]))
  cinqsumR1[[i]] <- fortify.zoo(aggregate(CinqR1bis[[i]][,1], by=CinqR1bis[[i]]$freq, FUN = sum))
  colnames(cinqsumR1[[i]]) = c("Index", "Value")
  cinqsumR1[[i]] <- as.xts(cinqsumR1[[i]])
  
}

### 0.5%

zerocinqsumR1 <- list()
for (i in 1:length(ZeroCinqR1bis))
{
  ZeroCinqR1bis[[i]]$freq <- rep(1:433, each=10, length.out=length(ZeroCinqR1bis[[i]][,1]))
  zerocinqsumR1[[i]] <- aggregate(ZeroCinqR1bis[[i]][,1], by=ZeroCinqR1bis[[i]]$freq, FUN = sum)
  zerocinqsumR1[[i]] <- fortify.zoo(zerocinqsumR1[[i]])
  colnames(zerocinqsumR1[[i]]) = c("Index","Value")
  zerocinqsumR1[[i]] <- as.xts(zerocinqsumR1[[i]])
}

# Creation objet sur lequel on vas pouvoir calculer les moyennes pour tous les ID du regime 5% 
R15sum <- data.frame()
for (i in 1:length(cinqsumR1))
{
  R15sum<- cbind(R15sum, cinqsumR1[[i]][,2])
}

R105sum <- data.frame()
for (i in 1:length(zerocinqsumR1))
{
  R105sum<- cbind(R105sum, zerocinqsumR1[[i]][,2])
}

## 2. moyenne sur les sommes d'activité pour tous les ID 
### 5% 
R15meansum <- as.xts(apply(R15sum, 1, mean, na.rm=TRUE))

### 0.5%
R105meansum <- as.xts(apply(R105sum, 1, mean, na.rm=TRUE))

## 3. t.test par minute 
### faut un dataframe avec chq colone = 10min et en ligne les ID 

R105sumst <- as.data.frame(t(R105sum))
R15sumst <- as.data.frame(t(R15sum))

R105sumst$Food <- rep("0.5%")
R15sumst$Food <- rep("5%")

meancompR1 <- rbind(R15sumst, R105sumst)
meancompR1$Food <- as.factor(meancompR1$Food)
head(meancompR1)

## pairwise.t.test par colonne

respwtR1 <- vector()
for (i in 1:ncol(meancompR1))
{
  respwtR1[i] <-  pairwise.t.test(meancompR1[,i], meancompR1$Food, p.adj = "bonf")$p.value
}
listminR1 <- which(respwtR1<=0.05)
pvallistminR1 <- respwtR1[respwtR1<=0.05]# faudrai enlever les NA pour que ça soit plus propre
timdiffR1 <- colnames(meancompR1[,listminR1])# temps où ya une différence entre les deux régimes

library(lubridate)
timdiffR1 <- ymd_hms(timdiffR1)

##Test global
# Il faut un tableau avec 3 colonnes et les 2 conditions 
# DF en ligne 

R15sum2 <- data.frame()
for (i in 1:length(cinqsumR1))
{
  cinqsumloc <-fortify.zoo(cinqsumR1[[i]][,2])
  R15sum2<- rbind(R15sum2, cinqsumloc)
}

R15sum2$Food <- rep("5%")


R105sum2 <- data.frame()
for (i in 1:length(zerocinqsumR1))
{
  zerocinqsumloc <-fortify.zoo(zerocinqsumR1[[i]][,2])
  R105sum2<- rbind(R105sum2, zerocinqsumloc)
}

R105sum2$Food <- rep("0.5%")

#DF tot 
RsumtotR1 <- rbind(R105sum2, R15sum2)# plutôt pour tab GLM 

# t.test global
t.test(RsumtotR1$Value~RsumtotR1$Food, na.rm=TRUE)
pairwise.t.test(RsumtotR1$Value,RsumtotR1$Food, na.rm=TRUE)# même reponse : pas signif en fonction du régime global. 
#Diff signif ici pval=0.0048

## Graphique R1

R15meansum2 <- fortify.zoo(R15meansum)
head(R15meansum2)
R15meansum2$Food <- rep("5%")
colnames(R15meansum2) <- c("Time", "Mean", "Food")

R105meansum2 <- fortify.zoo(R105meansum)
head(R105meansum2)
R105meansum2$Food <- rep("0.5%")
colnames(R105meansum2) <- c("Time", "Mean", "Food")

# DF complet
meansumtotR1 <- rbind(R15meansum2, R105meansum2)
head(meansumtotR1)

## graph couplé (a améliorer)
library(ggplot2)
breakT <- meansumtotR1$Time["18:00:00"]
ggplot(meansumtotR1, aes(x = Time, y = Mean)) + 
  geom_line(aes(color = Food), size = 0.5) +
  theme_minimal()+
  geom_vline(xintercept = timdiffR1, size=0.3)+
  ggtitle("Mean activity for R1")+
  scale_x_datetime(date_labels = "%d-%H")


###############################################
############### R2 ############################


### 5% 

cinqsumR2 <- list()
for (i in 1:length(CinqR2bis))
{
  CinqR2bis[[i]]$freq <- rep(1:433, each=10, length.out=length(CinqR2bis[[i]][,1]))
  cinqsumR2[[i]] <- fortify.zoo(aggregate(CinqR2bis[[i]][,1], by=CinqR2bis[[i]]$freq, FUN = sum))
  colnames(cinqsumR2[[i]]) = c("Index", "Value")
  cinqsumR2[[i]] <- as.xts(cinqsumR2[[i]])
  
}

### 0.5%

zerocinqsumR2 <- list()
for (i in 1:length(ZeroCinqR2bis))
{
  ZeroCinqR2bis[[i]]$freq <- rep(1:433, each=10, length.out=length(ZeroCinqR2bis[[i]][,1]))
  zerocinqsumR2[[i]] <- aggregate(ZeroCinqR2bis[[i]][,1], by=ZeroCinqR2bis[[i]]$freq, FUN = sum)
  zerocinqsumR2[[i]] <- fortify.zoo(zerocinqsumR2[[i]])
  colnames(zerocinqsumR2[[i]]) = c("Index","Value")
  zerocinqsumR2[[i]] <- as.xts(zerocinqsumR2[[i]])
}

# Creation objet sur lequel on vas pouvoir calculer les moyennes pour tous les ID du regime 5% 
R25sum <- data.frame()
for (i in 1:length(cinqsumR2))
{
  R25sum<- cbind(R25sum, cinqsumR2[[i]][,2])
}

R205sum <- data.frame()
for (i in 1:length(zerocinqsumR2))
{
  R205sum<- cbind(R205sum, zerocinqsumR2[[i]][,2])
}

## 2. moyenne sur les sommes d'activité pour tous les ID 
### 5% 
R25meansum <- as.xts(apply(R25sum, 1, mean, na.rm=TRUE))

### 0.5%
R205meansum <- as.xts(apply(R205sum, 1, mean, na.rm=TRUE))

## pour graphiques avec SE 

tab25 <- data_frame()
for (i in 1:nrow(R25sum))
{
  print(i)
  xloc <- as.numeric(R25sum[i,])
  tab25 <- rbind(tab25,mean_se(xloc, mult=1))
}
tab25$Time <- index(R25sum)
tab25$Food <- "5%"

pR25 <- ggplot(data=tab, aes(x=Time, y=y))+
geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey70") +
  geom_line(aes(y = y))+ 
  ylab("Mean of activity") +
  ggtitle("Mean of activity for 5% (R2)")

## 3. t.test par minute 
### faut un dataframe avec chq colone = 10min et en ligne les ID 

R205sumst <- as.data.frame(t(R205sum))
R25sumst <- as.data.frame(t(R25sum))

R205sumst$Food <- rep("0.5%")
R25sumst$Food <- rep("5%")

meancompR2 <- rbind(R25sumst, R205sumst)
meancompR2$Food <- as.factor(meancompR2$Food)
head(meancompR2)

## pairwise.t.test par colonne

respwtR2 <- vector()
for (i in 1:ncol(meancompR2))
{
  respwtR2[i] <-  pairwise.t.test(meancompR2[,i], meancompR2$Food, p.adj = "bonf")$p.value
}
listminR2 <- which(respwtR2<=0.05)
pvallistminR2 <- respwtR2[respwtR2<=0.05]# faudrai enlever les NA pour que ça soit plus propre
timdiffR2 <- colnames(meancompR2[,listminR2])# temps où ya une différence entre les deux régimes

library(lubridate)
timdiffR2 <- ymd_hms(timdiffR2)

##Test global
# Il faut un tableau avec 3 colonnes et les 2 conditions 
# DF en ligne 

R25sum2 <- data.frame()
for (i in 1:length(cinqsumR2))
{
  cinqsumloc <-fortify.zoo(cinqsumR2[[i]][,2])
  R25sum2<- rbind(R25sum2, cinqsumloc)
}

R25sum2$Food <- rep("5%")


R205sum2 <- data.frame()
for (i in 1:length(zerocinqsumR2))
{
  zerocinqsumloc <-fortify.zoo(zerocinqsumR2[[i]][,2])
  R205sum2<- rbind(R205sum2, zerocinqsumloc)
}

R205sum2$Food <- rep("0.5%")

#DF tot 
RsumtotR2 <- rbind(R205sum2, R25sum2)# plutôt pour tab GLM 

# t.test global
t.test(RsumtotR2$Value~RsumtotR2$Food, na.rm=TRUE)
pairwise.t.test(RsumtotR2$Value,RsumtotR2$Food, na.rm=TRUE)# même reponse : pas signif en fonction du régime global. 
#Pas diff : pval=0.1 

## Graphique R2

R25meansum2 <- fortify.zoo(R25meansum)
head(R25meansum2)
R25meansum2$Food <- rep("5%")
colnames(R25meansum2) <- c("Time", "Mean", "Food")

R205meansum2 <- fortify.zoo(R205meansum)
head(R205meansum2)
R205meansum2$Food <- rep("0.5%")
colnames(R205meansum2) <- c("Time", "Mean", "Food")

# DF complet
meansumtotR2 <- rbind(R25meansum2, R205meansum2)
head(meansumtotR2)

## graph couplé (a améliorer)
library(ggplot2)
breakT <- meansumtotR2$Time["18:00:00"]
hours <- c("18:00:00", "23:59:00")
Tim <- (meansumtotR2$Time[c(1,37)])

ggplot(meansumtotR2, aes(x = Time, y = Mean)) + 
  geom_line(aes(color = Food), size = 0.5) +
  theme_minimal()+
  geom_vline(xintercept = timdiffR2, size=0.3)+
  ggtitle("Mean activity for R2")+
  scale_x_datetime(date_labels = "%D-%H")




p1 <- pairwise.t.test(meancompR2[,35], meancompR2$Food, p.adj = "bonf")
p1$data.name
ess <- apply(R25sum, 1, mean, na.rm=TRUE)
plot(ess, type="l")
ess[15,]
R25meansum[15,]
mean_se(x, mult=1)
class(R25sum[,])
x <- as.numeric(R25sum[15,])
