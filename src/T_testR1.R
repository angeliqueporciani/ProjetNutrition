# Working data  

# => listes ne contenant les données que pour les jours choisis. 

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

## calcul moyenne par 10min 
activity <- list()
for (i in seq_along(CinqR1bis)) {
  activity[[i]] <- ts_ma(CinqR1bis[[i]], n = 10, plot = FALSE)[[1]]
}
length(activity[[4]])

## trasnformation en DF (DATE + 1ID=1colonne)
R15DF <- data.frame()
for (i in 1:length(activity))
  {
  R15DF<- cbind(R15DF, activity[[i]][,1])
}

# moyenne par ligne 

R15mean <- as.xts(apply(R15DF, 1, mean, na.rm=TRUE))
plot(R15mean)

R15mean2 <- fortify.zoo(R15mean)
head(R15mean2)
R15mean2$Food <- rep("5%")

# prep pour t test

R15bis <- fortify.zoo(R15DF)
R15bis$Food <- rep("5%")
head(R15bis)

##################################################################
## Essai avec 0.5 R1 et test de student pour voir si ça fonctionne 
##################################################################

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

## calcul moyenne par 10min 
activity05 <- list()
for (i in seq_along(ZeroCinqR1bis)) {
  activity05[[i]] <- ts_ma(ZeroCinqR1bis[[i]], n = 10, plot = FALSE)[[1]]
}


## trasnformation en DF (DATE + 1ID=1colonne)
R105DF <- data.frame()
for (i in 1:length(activity05))
{
  R105DF<- cbind(R105DF, activity05[[i]][,1])
}

# moyenne par ligne 

R105mean <- as.xts(apply(R105DF, 1, mean, na.rm=TRUE))
plot(R105mean)

R105mean2 <- fortify.zoo(R105mean)
head(R105mean2)
R105mean2$Food <- rep("0.5%")

## peut être utile pour comp activité totale. 
R105bis <- fortify.zoo(R105DF)
R105bis$Food <- rep("0.5%")
head(R105bis)
class(R105bis)

## Test de student : faut transposer Index en colonne +ID+FOOD
# demarrage avec R105DF

R105ess <- as.data.frame(t(R105DF))
R15ess <- as.data.frame(t(R15DF))

R105ess$Food <- rep("0.5%")
R15ess$Food <- rep("5%")

meancomp <- rbind(R15ess, R105ess)
meancomp$Food <- as.factor(meancomp$Food)
str(meancomp$Food)

#t.test

###var.test = comparaison des variances pour verifier l'homosceda...

A1=apply(meancomp[,11:4311],2, function (vec,gploc)var.test(vec~gploc)$p.val, gploc=meancomp$Food)
resvarcor=p.adjust(A1, method="BH")##methode benjamin... pour ajuster les p-value car on a fait plusieurs test 
sum(resvarcor<=0.05)##=1
plot(resvarcor, type="l")
listgenot=which(resvarcor<=0.05)##quelle minute ont des différence signif. 
listgenot

##T.Test
A2=apply(meancomp[,11:4311],2, function (vec,gploc) t.test(vec~gploc, var.equal=F, na.rm=TRUE)$p.val, gploc=meancomp$Food)
resvarcor2=p.adjust(A2, method="BH")##methode benjamini Hochberg
sum(resvarcor2<=0.05)## somme de toute les mins qui ont des différences entre les groupes 
listgenot=which(resvarcor2<=0.05)##quelle minute ont des différence signif. 
listgenot
## Aucune différence

plot(resvarcor2, type="l")




