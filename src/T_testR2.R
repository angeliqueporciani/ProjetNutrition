# Working data  

# => listes ne contenant les données que pour les jours choisis. 

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

## calcul moyenne par 10min 
activity <- list()
for (i in seq_along(CinqR2bis)) {
  activity[[i]] <- ts_ma(CinqR2bis[[i]], n = 10, plot = FALSE)[[1]]
}
length(activity[[4]])

## trasnformation en DF (DATE + 1ID=1colonne)
R25DF <- data.frame()
for (i in 1:length(activity))
{
  R25DF<- cbind(R25DF, activity[[i]][,1])
}

# moyenne par ligne 

R25mean <- as.xts(apply(R25DF, 1, mean, na.rm=TRUE))
plot(R25mean)

R25mean2 <- fortify.zoo(R25mean)
head(R25mean2)
R25mean2$Food <- rep("5%")

# prep pour t test

R25bis <- fortify.zoo(R25DF)
R25bis$Food <- rep("5%")
head(R25bis)

##################################################################
## Essai avec 0.5 R1 et test de student pour voir si ça fonctionne 
##################################################################

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

## calcul moyenne par 10min 
activity05 <- list()
for (i in seq_along(ZeroCinqR2bis)) {
  activity05[[i]] <- ts_ma(ZeroCinqR2bis[[i]], n = 10, plot = FALSE)[[1]]
}


## trasnformation en DF (DATE + 1ID=1colonne)
R205DF <- data.frame()
for (i in 1:length(activity05))
{
  R205DF<- cbind(R205DF, activity05[[i]][,1])
}

# moyenne par ligne 

R205mean <- as.xts(apply(R205DF, 1, mean, na.rm=TRUE))
plot(R205mean)

R205mean2 <- fortify.zoo(R205mean)
head(R205mean2)
R205mean2$Food <- rep("0.5%")

## peut être utile pour comp activité totale. 
class(R205bis)
R205bis <- fortify.zoo(R205DF)
R205bis$Food <- rep("0.5%")
head(R205bis)

## Test de student : faut transposer Index en colonne +ID+FOOD
# demarrage avec R205DF

R205ess <- as.data.frame(t(R205DF))
R25ess <- as.data.frame(t(R25DF))

R205ess$Food <- rep("0.5%")
R25ess$Food <- rep("5%")

meancomp <- rbind(R25ess, R205ess)
meancomp$Food <- as.factor(meancomp$Food)
str(meancomp$Food)

#t.test

###var.test = comparaison des variances pour verifier l'homosceda...

A1=apply(meancomp[,11:4311],2, function (vec,gploc)var.test(vec~gploc)$p.val, gploc=meancomp$Food)
resvarcor=p.adjust(A1, method="BH")##methode benjamin... pour ajuster les p-value car on a fait plusieur test 
sum(resvarcor<=0.05)##=1
plot(resvarcor, type="l")
listgenot=which(resvarcor<=0.05)##quelle minute ont des différence signif. 
listgenot

##T.Test
A2=apply(meancomp[,11:4311],2, function (vec,gploc) t.test(vec~gploc, var.equal=F, na.rm=TRUE)$p.val, gploc=meancomp$Food)
resvarcor2=p.adjust(A2, method="BH")##methode benjamin...
sum(resvarcor2<=0.05)## somme de toute les mins qui ont des différences entre les groupes 
listgenot=which(resvarcor2<=0.05)##quelle minute ont des différence signif. 
listgenot
## Aucune différence non plus ... 

plot(resvarcor2, type="l")


## Commentaires : beaucoup de 0, moyenne classique surement pas adaptée. Somme ou moyenne de williams? 

## Combiner les 2 réplicats ensemble = index unique 
## refaire le t test. 



