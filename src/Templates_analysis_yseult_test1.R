## Script demarrage 

# 1. load packages and functions 

source("./src/fun/select.elements.R")
source("./src/fun/meanXind.R")
source("./src/fun/AvgActByHour.R")
source("./src/fun/plot.AvgActByHour.R")
source("./src/fun/consolidate.activity.R")
source("./src/fun/extract.timepoints.R")
source("./src/fun/plot.raw.activity.R")

library(xts)
library(tidyverse) 
library(lubridate)
library(TSstudio) 


# 2. Load data 

activity1 <- readRDS("./data/Activity/R01/Activity.rds")
activity2 <-  readRDS("./data/Activity/R02/Activity.rds")
activtot <- c(activity1, activity2)



# 3. Creation of subset 
# We need subset for control, 5% R1 et R2, 0.5% R1 et R2 et 5% tot et 0.5% tot. 

Control <- select.elements(activtot, "Strain", "Control")# creation sous base Control

# Checking the Control with graphics.

Control_summed_activity <- consolidate.activity(Control, FUN = sum) / length(Control)# creation moyenne de l'activité
plot(Control_summed_activity)# données très proches de 0 on est ok. 

## delete control from database for analysis
actif1 <- Filter(function(x) attr(x, which = "Strain", exact = TRUE) !="Control", activity1)
actif2 <- Filter(function(x) attr(x, which = "Strain", exact = TRUE) !="Control", activity2)
actiftot <- Filter(function(x) attr(x, which = "Strain", exact = TRUE) !="Control", activtot)

CinqP <- select.elements(actiftot, "Food(%)", 5)#creation sous base avec que les individus nourris à 5% de sucre
CinqP1 <- select.elements(actif1, "Food(%)", 5)# sous base 5% sucre pour le réplicat 1
CinqP2 <- select.elements(actif2, "Food(%)", 5)# sous base 5% sucre pour le réplicat 2

ZeroCinqP <-select.elements(actiftot, "Food(%)", 0.5)#creation sous base avec que les individus nourris à 0.5% de sucre
ZeroCinqP1 <-select.elements(actif1, "Food(%)", 0.5)# sous base 0.5% sucre pour le réplicat 1
ZeroCinqP2 <-select.elements(actif2, "Food(%)", 0.5)# sous base 0.5% sucre pour le réplicat 2


#4 . select the days of analysis
# les jours d'analyse sont différents entre le replicat 1 et 2, donc pour le moment on travaille séparement, mais faudra les lier à un moment.

#R1
# creation de liste avec juste les jours qui nous interesse (à faire pour  tous les groupes le replicat 1 et 2)
# pour le replicat 2 faudra changer les dates mais les heures devrait rester les mêmes si tu as fait comme pour R1.

# Création d'une nouvelle liste ne contenant les données que pour les jours choisis. 
# a faire pour chaque subset crée dans la partie 3. 

#R1
CinqR1 <- list()
for (i in 1:length(CinqP1))
{
  CinqR1[[i]] <- CinqP1[[i]]["2020-02-20 18:00:00/2020-02-23 18:00:00"]
  
}

#R2 
CinqR2 <- list()
for (i in 1:length(CinqP2))
{
  CinqR2[[i]] <- CinqP2[[i]]["2020-03-05 18:00:00/2020-03-08 18:00:00"]
  
}

#R1 0.5
ZeroCinqR1 <- list()
for (i in 1:length(ZeroCinqP1))
{
  ZeroCinqR1[[i]] <- ZeroCinqP1[[i]]["2020-02-20 18:00:00/2020-02-23 18:00:00"]
  
}

#R2 0.5
ZeroCinqR2 <- list()
for (i in 1:length(ZeroCinqP2))
{
  ZeroCinqR2[[i]] <- ZeroCinqP2[[i]]["2020-03-05 18:00:00/2020-03-08 18:00:00"]
  
}


## Graphiques 
startR1 <- ymd_hms("2020-02-20 18:00:00")# jour début
endR1 <- ymd_hms("2020-02-23 18:00:00")# jour fin

startR2 <- ymd_hms("2020-03-05 18:00:00")# jour début
endR2 <- ymd_hms("2020-03-08 18:00:00")# jour fin

# Moyenne par heure et pour chq individu
# R1 5%
CinqR1mean <- AvgActByHour(CinqR1, startR1, endR1)# calcule la moyenne par heure pour chq ind
plot.AvgActByHour(CinqR1mean, main = "5% Sugar")# plot activité moyenne par heure pour chaq individu.

#R2 5%
CinqR2mean <- AvgActByHour(CinqR2, startR2, endR2)# calcule la moeynne par heure pour chq ind
plot.AvgActByHour(CinqR2mean, main = "5% Sugar")# plot activité moyenne par heure pour chaq individu. 

#R1 0.5%
ZeroCinqR1mean <- AvgActByHour(ZeroCinqR1, startR1, endR1)# calcule la moyenne par heure pour chq ind
plot.AvgActByHour(ZeroCinqR1mean, main = "0.5% Sugar")# plot activité moyenne par heure pour chaq individu.

#R2 0.5%
ZeroCinqR2mean <- AvgActByHour(ZeroCinqR2, startR2, endR2)# calcule la moeynne par heure pour chq ind
plot.AvgActByHour(ZeroCinqR2mean, main = "0.5% Sugar")# plot activité moyenne par heure pour chaq individu. 

# Avec CinqPmean on peut trouver les morts (ceux qui ont une activité moyenne de 0 pour toutes les heures). 

# Medianne sur 10 min 
#R1 5%
Cinq_median_activity_R1 <- consolidate.activity(CinqR1, FUN = median, smooth.by = 10)
plot(Cinq_median_activity_R1)

#R2 5%
Cinq_median_activity_R2 <- consolidate.activity(CinqR2, FUN = median, smooth.by = 10)
plot(Cinq_median_activity_R2)

#R1 0.5%
ZeroCinq_median_activity_R1 <- consolidate.activity(ZeroCinqR1, FUN = median, smooth.by = 10)
plot(ZeroCinq_median_activity_R1)

#R2 0.5%
ZeroCinq_median_activity_R2 <- consolidate.activity(ZeroCinqR2, FUN = median, smooth.by = 10)
plot(ZeroCinq_median_activity_R2)

## avec les median on a des graphiques pas top (ca vient du fait des morts encore présent je pense)
## je propose les mêmes graphes mais avec la somme (pour limiter l'impact des 0 qui sont trèès nombreux ici).

#R1 5%
Cinq_sum_activity_R1 <- consolidate.activity(CinqR1, FUN = sum, smooth.by = 10)
plot(Cinq_sum_activity_R1)

sumR1ess <- consolidate.activity(CinqR1[[3]], FUN = sum, smooth.by = 60)
plot(sumR1ess)
head(sum)

#R2 5%
Cinq_sum_activity_R2 <- consolidate.activity(CinqR2, FUN = sum, smooth.by = 10)
plot(Cinq_sum_activity_R2)

par(mfrow=c(2,1))
plot(Cinq_sum_activity_R1)
plot(Cinq_sum_activity_R2)
par(mfrow=c(1,1))

#R1 0.5%
ZeroCinq_sum_activity_R1 <- consolidate.activity(ZeroCinqR1, FUN = sum, smooth.by = 10)
plot(ZeroCinq_sum_activity_R1)

#R2 0.5%
ZeroCinq_sum_activity_R2 <- consolidate.activity(ZeroCinqR2, FUN = sum, smooth.by = 10)
plot(ZeroCinq_sum_activity_R2)

par(mfrow=c(2,1))
plot(ZeroCinq_sum_activity_R1)
plot(ZeroCinq_sum_activity_R2)
par(mfrow=c(1,1))

## moyenne
#R1 5%
Cinq_mean_activity_R1 <- consolidate.activity(CinqR1, FUN = mean, smooth.by = 10)
plot(Cinq_mean_activity_R1)

#R2 5%
Cinq_mean_activity_R2 <- consolidate.activity(CinqR2, FUN = mean, smooth.by = 10)
plot(Cinq_mean_activity_R2)

par(mfrow=c(2,1))
plot(Cinq_mean_activity_R1)
plot(Cinq_mean_activity_R2)
par(mfrow=c(1,1))

#R1 0.5%
ZeroCinq_mean_activity_R1 <- consolidate.activity(ZeroCinqR1, FUN = mean, smooth.by = 10)
plot(ZeroCinq_mean_activity_R1)

#R2 0.5%
ZeroCinq_mean_activity_R2 <- consolidate.activity(ZeroCinqR2, FUN = mean, smooth.by = 10)
plot(ZeroCinq_mean_activity_R2)

par(mfrow=c(2,1))
plot(ZeroCinq_mean_activity_R1)
plot(ZeroCinq_mean_activity_R2)
par(mfrow=c(1,1))


####### PLOT ########
R1 <- fortify.zoo(Cinq_mean_activity_R1)# transforme en DF (permettra de faire avec ggplot plus tard)
R105df <- fortify.zoo(ZeroCinq_mean_activity_R1)

plot(R1, type="l", ylim=c(0,2.5), xlab="", ylab="")
par(new=TRUE)
plot(R105df, type="l", col="red", ylim=c(0,2.5), 
     main="Moyenne des passages sur 10min (réplicat 1)",
     xlab="Time", ylab="nb moyen de passage")
legend("topright" ,legend=c("5%", "0.5%"),
       col=c("black", "red"), lty=1:2, cex=0.8)

#############
R2 <- fortify.zoo(Cinq_mean_activity_R2)# transforme en DF (permettra de faire avec ggplot plus tard)
R205df <- fortify.zoo(ZeroCinq_mean_activity_R2)

plot(R2, type="l", ylim=c(0,1.5), xlab="", ylab="")
par(new=TRUE)
plot(R205df, type="l", col="red", ylim=c(0,1.5), 
     main="Moyenne des passages sur 10min (réplicat 2)",
     xlab="Time", ylab="nb moyen de passage")
legend("topright" ,legend=c("5%", "0.5%"),
       col=c("black", "red"), lty=1:2, cex=0.8)


