## Creation Subset (pour eviter d'avoir à le faire à chaque fois en début de script). 

source("./src/fun/select.elements.R")
source("./src/fun/meanXind.R")
source("./src/fun/AvgActByHour.R")
source("./src/fun/plot.AvgActByHour.R")
source("./src/fun/consolidate.activity.R")
source("./src/fun/extract.timepoints.R")
source("./src/fun/plot.raw.activity.R")

library(xts)
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

Cinqtot <- c(CinqR1, CinqR2)
ZeroCinqtot <- c(ZeroCinqR1, ZeroCinqR2)

rm(Control)
rm(actif1)
rm(actif2)
rm(actiftot)
rm(Control_summed_activity)
