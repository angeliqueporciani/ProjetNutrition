## couplage des Réplicats par régime 

## base de donnée de départ : DF avec ID en colonne et index le temps (commum pour les 2)

# importation des listes de chaque réplicat pour régime food=5% 

source("./src/fun/select.elements.R")
source("./src/fun/meanXind.R")
source("./src/fun/AvgActByHour.R")
source("./src/fun/plot.AvgActByHour.R")
source("./src/fun/consolidate.activity.R")
source("./src/fun/extract.timepoints.R")
source("./src/fun/plot.raw.activity.R")
source("./src/00 Subset_creation.R")# permet de récuperer directement tous le subset que l'on veut. 

library(xts)
library(lubridate)
library(TSstudio) 


## faut appliquer le même index à R1 et R2 pour pouvoir bosser directement sur une liste contenant les 60 ID. 
# on fait cette fonction après selection des jours d'interet comme ça time series de la même longueur. 

Time <- index(CinqR1[[1]])
for( i in 1:length(CinqR2))
{
  index(CinqR2[[i]]) <- Time
}

Cinqtot <- c(CinqR1, CinqR2)


Time <- index(ZeroCinqR1[[1]])
for( i in 1:length(ZeroCinqR2))
{
  index(ZeroCinqR2[[i]]) <- Time
}

ZeroCinqtot <- c(ZeroCinqR1, ZeroCinqR2)

