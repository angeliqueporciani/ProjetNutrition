library(ggplot2)

## Methode utilisée ici : 
# 1 : moyenne d'activité sur 10 min 
# 2 : moyenne de ces activités pour tous les ID d'un même groupe. 
# 3 : comparaison min/min de chaque groupe avec t. test ou pairwise. 

## Préparation des données. 

### Selection des jours utilisé pour chaque ID 
sumdaily <- list()
for (i in 1:length(Cinqtot))
{
  sumdaily[[i]] <- apply.daily(Cinqtot[[i]], sum)
} 

## creation nouvelle liste avec selection des jours somme activité>0 

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


## calcul moyenne par 10min 
activity <- list()
for (i in seq_along(Cinqtotbis)) {
  activity[[i]] <- ts_ma(Cinqtotbis[[i]], n = 10, plot = FALSE)[[1]]
}


tsmaess <- ts_ma(Cinqtotbis[[1]], n = 720, plot = FALSE)
str(tsmaess)
tsmaess[[2]]
plot(tsmaess[[1]])
tsmaess[[3]]


## trasnformation en DF (DATE + 1ID=1colonne)
R5DF <- data.frame()
for (i in 1:length(activity))
{
  R5DF<- cbind(R5DF, activity[[i]][,1])
}

# moyenne par ligne 

R5mean <- as.xts(apply(R5DF, 1, mean, na.rm=TRUE))
R5sd <- as.xts(apply(R5DF, 1, sd, na.rm=TRUE))
R5sum <- as.xts(apply(R5DF, 1, sum, na.rm=TRUE))

plot(R5sum)

par(new=TRUE)
plot(R5sd)

R5mean2 <- fortify.zoo(R5mean)
head(R5mean2)
R5mean2$Food <- rep("5%")
colnames(R5mean2) <- c("Time", "Mean", "Food")

# prep pour t test

R5bis <- fortify.zoo(R5DF)
R5bis$Food <- rep("5%")
head(R5bis)

##################################################################
## Essai avec 0.5 R1 et test de student pour voir si ça fonctionne 
##################################################################

# 2. Delete days with a sum activity=0

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

## calcul moyenne par 10min 
activity05 <- list()
for (i in seq_along(ZeroCinqtotbis)) {
  activity05[[i]] <- ts_ma(ZeroCinqtotbis[[i]], n = 10, plot = FALSE)[[1]]
}


## trasnformation en DF (DATE + 1ID=1colonne)
R05DF <- data.frame()
for (i in 1:length(activity05))
{
  R05DF<- cbind(R05DF, activity05[[i]][,1])
}

# moyenne par ligne 

R05mean <- as.xts(apply(R05DF, 1, mean, na.rm=TRUE))
plot(R05mean)
R05sum <- as.xts(apply(R05DF, 1, sum, na.rm=TRUE))
plot(R05sum)

R05mean2 <- fortify.zoo(R05mean)
head(R05mean2)
R05mean2$Food <- rep("0.5%")
colnames(R05mean2) <- c("Time", "Mean", "Food")
## graphique superposé : assemblage R05mean2+R5mean2

meantot <- rbind(R5mean2, R05mean2)
ggplot(meantot, aes(x = Time, y = Mean)) + 
  geom_line(aes(color = Food), size = 0.5) +
  theme_minimal()

## peut être utile pour comp activité totale. 
class(R05bis)
R05bis <- fortify.zoo(R05DF)
R05bis$Food <- rep("0.5%")
head(R05bis)

## Test de student : faut transposer Index en colonne +ID+FOOD
# demarrage avec R105DF

R05ess <- as.data.frame(t(R05DF))
R5ess <- as.data.frame(t(R5DF))

R05ess$Food <- rep("0.5%")
R5ess$Food <- rep("5%")

meancomp <- rbind(R5ess, R05ess)
meancomp$Food <- as.factor(meancomp$Food)
str(meancomp$Food)

R05ess2 <- as.data.frame(t(R05mean))
R5ess2 <- as.data.frame(t(R5mean))

meancomp2 <- as.data.frame(rbind(R05ess2, R5ess2))

#t.test

###var.test = comparaison des variances pour verifier l'homosceda...

A1=apply(meancomp[,11:4311],2, function (vec,gploc)var.test(vec~gploc)$p.val, gploc=meancomp$Food)
resvarcor=p.adjust(A1, method="BH")##methode benjamin... pour ajuster les p-value car on a fait plusieur test 
sum(resvarcor<=0.05)##=1
plot(resvarcor, type="l")
listgenot=which(resvarcor<=0.05)##quelle minute ont des différence signif. 
listgenot

datatest <- meancomp[,-listgenot]
datatest[, 1360:1370]
##T.Test
A2=apply(datatest[,11:1360],2, function (vec,gploc) t.test(vec~gploc, var.equal=F, na.rm=TRUE)$p.val, gploc=meancomp$Food)
resvarcor2=p.adjust(A2, method="BH")##methode benjamini Hochberg
sum(resvarcor2<=0.05)## somme de toute les mins qui ont des différences entre les groupes 
listgenot=which(resvarcor2<=0.05)##quelle minute ont des différence signif. 
listgenot
## Aucune différence

plot(resvarcor2, type="l")

## essai selon methode comprise d'après le papier : comparaison de 2 vecteurs de moyenne de williams
## 1 essai t.test avec moyenne geom

t.test(R5mean2[,2], R05mean2[,2], paired = T, na.rm=TRUE)# moyenne d'activité totale différentes.

t.test2=apply(meancomp2[, 11:4300], 2, function (x, y) t.test(x, y, var.equal=F, paired=TRUE, na.rm=TRUE)$p.val, y=meancomp2[2, 11:4300])


## pairwise.t.test
respwt3 <- vector()
for (i in 1:ncol(meancomp))
{
  respwt3[i] <-  pairwise.t.test(meancomp[,i], meancomp$Food, p.adj = "bonf")$p.value
}

