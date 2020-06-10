## Script demarrage 

# 1. Load data 

activity1 <- readRDS("./data/Activity/R01/Activity.rds")
activity2 <-  readRDS("./data/Activity/R02/Activity.rds")
activtot <- c(activity1, activity2)

# 2. load packages and functions 

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


# 3. Creation of subset 
# We need subset for control, 5% R1 et R2, 0.5% R1 et R2 et 5% tot et 0.5% tot. 

Control <- select.elements(activtot, "Strain", "Control")# creation sous base Control

CinqP <- select.elements(activtot, "Food(%)", 5)#creation sous base avec que les individus nourris à 5% de sucre
CinqP1 <- select.elements(activity1, "Food(%)", 5)# sous base 5% sucre pour le réplicat 1
CinqP2 <- select.elements(activity2, "Food(%)", 5)# sous base 5% sucre pour le réplicat 2


#05P <- #je te laisse completer pour 0.5% ;) 

# 4. Checking the Control with graphics.

Control_summed_activity <- consolidate.activity(Control, FUN = sum) / length(Control)# creation moyenne de l'activité de tous les controles

plot(Control_summed_activity)# données très proches de 0 on est ok. 

# 5. select the days of analysis
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
  CinqR1[[i]] <- CinqP1[[i]]["2020-02-20 23:59:00/2020-02-23 18:00:00"]
  
}

#R2 
CinqR2 <- list()
for (i in 1:length(CinqP2))
{
  CinqR2[[i]] <- CinqP2[[i]]["2020-03-05 23:59:00/2020-03-08 18:00:00"]
  
}

#R1 0.5

#R2 0.5 


## Graphiques 

startR1 <- ymd_hms("2020-02-20 23:59:00")# jour début
endR1 <- ymd_hms("2020-02-23 18:00:00")# jour fin

startR2 <- ymd_hms("2020-03-05 23:59:00")# jour début
endR2 <- ymd_hms("2020-03-08 18:00:00")# jour fin

# Moyenne par heure et pour chq individu
# R1 5%
CinqR1mean <- AvgActByHour(CinqR1, startR1, endR1)# calcule la moyenne par heure pour chq ind
plot.AvgActByHour(CinqR1mean, main = "5% Sugar")# plot activité moyenne par heure pour chaq individu. 

#R2 5%

CinqR2mean <- AvgActByHour(CinqR2, startR2, endR2)# calcule la moeynne par heure pour chq ind
plot.AvgActByHour(CinqR2mean, main = "5% Sugar")# plot activité moyenne par heure pour chaq individu. 

# Avec CinqPmean on peut trouver les morts (ceux qui ont une activité moyenne de 0 pour toute les heures). 

# Medianne sur 10 min 
#R1
Cinq_median_activity_R1 <- consolidate.activity(CinqR1, FUN = median, smooth.by = 10)
plot(Cinq_median_activity_R1)

Cinq_mean_activity_R1 <- consolidate.activity(CinqR1, FUN = mean, smooth.by = 10)
plot(Cinq_mean_activity_R1)

#R2
Cinq_median_activity_R2 <- consolidate.activity(CinqR2, FUN = median, smooth.by = 10)
plot(Cinq_median_activity_R2)

Cinq_mean_activity_R2 <- consolidate.activity(CinqR2, FUN = mean, smooth.by = 10)
plot(Cinq_mean_activity_R2)

## avec les median on a des graphiques pas top (ca vient du fait des morts encore présent je pense)
## je propose les mêmes graphes mais avec la somme (pour limiter l'impact des 0 qui sont trèès nombreux ici). 
## ou avec la moyenne, ca passe mieux aussi qu'avec la mediane. 


#R1
Cinq_sum_activity_R1 <- consolidate.activity(CinqR1, FUN = sum, smooth.by = 10)
plot(Cinq_sum_activity_R1)

#R2
Cinq_sum_activity_R2 <- consolidate.activity(CinqR2, FUN = sum, smooth.by = 10)
plot(Cinq_sum_activity_R2)

par(mfrow=c(2,1))
plot(Cinq_sum_activity_R1)
plot(Cinq_sum_activity_R2)
par(mfrow=c(1,1))

## A faire pour 0.5 



##### Selection mort (en cours)
## selection des morts 
RM2 <- list()
for (i in 1:length(CinqR2mean)){
  if (sum(CinqR2mean[[i]])>0){
    RM2[[i]] <- CinqR2mean[[i]]
  } else
    RM2[[i]] <- "NULL"
}

dead <- which(RM2=="NULL")
RM2b <- RM2[-dead]
## a affiner car on peut considérer comme mort ceux qui sont inactif pendant plus de 



