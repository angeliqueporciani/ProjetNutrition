## préparation tableau general et 10 min 
source("./src/01 Subset_actifs.R")


## 1. DF avec toute les minutes d'enregistrement 

###function
listofDF <- function(activitylist){
reslistDF <- list()
for (i in 1:length(activitylist))
  {
  reslistDF[[i]] <- data.frame(AC=as.integer(activitylist[[i]][,1]), Food=rep(xtsAttributes(activitylist[[i]])$Food), 
                               Monitor=rep(xtsAttributes(activitylist[[i]])$Monitor),
                               Time=index(activitylist[[i]]), 
                               Channel=rep(xtsAttributes(activitylist[[i]])$Channel),
                               ID= rep(i),
                               Replicat= rep(xtsAttributes(activitylist[[i]])$Replicat),
                               Hour=as.integer(strftime(index(activitylist[[i]]), format = "%H", tz="")),
                               Month=as.integer(strftime(index(activitylist[[i]]), format = "%m", tz="")),
                               Day=as.character(strftime(index(activitylist[[i]]), format = "%d", tz="")))
}
return(reslistDF)
}

## creation list de DF 
Cinqbis <- listofDF(CinqP)
Zerocinqbis <- listofDF(ZeroCinqP)

#creation of a dataframe
DFcinq <- data.frame()
for (i in 1:length(Cinqbis))
{
  DFcinq <- rbind(DFcinq, Cinqbis[[i]])
}

DF0cinq <- data.frame()
for (i in 1:length(Zerocinqbis))
{
  DF0cinq <- rbind(DF0cinq, Zerocinqbis[[i]])
}

## compil des 2 régimes dans un BIG DF (pas loin du 1.10^6 ligne ...)

DFactivity <- rbind(DFcinq, DF0cinq)
saveRDS(DFactivity, "./output/DFactivitytot.rds")

## 2. la même mais avec sum/10min (tableau plus petit)
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

## creation de list of df 

head(cinqsum[[1]])
head(Cinqbis[[1]])

cinqsumbis <- list()
for (i in 1:length(cinqsum)){
cinqsumbis[[i]] <- data.frame(AC=cinqsum[[i]]$Value, Time=index(cinqsum[[i]]),
                              Monitor=rep(Cinqbis[[i]]$Monitor[1]), 
                              Food=rep("5%"), 
                              Channel=rep(Cinqbis[[i]]$Channel[1]),
                              Replicat=rep(Cinqbis[[i]]$Replicat[1]),
                              ID=rep(i),
                              Hour=as.integer(strftime(index(cinqsum[[i]]), format = "%H", tz="")),
                              Month=as.integer(strftime(index(cinqsum[[i]]), format = "%m", tz="")),
                              Day=as.character(strftime(index(cinqsum[[i]]), format = "%d", tz=""))
                              )
}
head(cinqsumbis[[1]])

z <- c(61:120)
zerocinqsumbis <- list()
for (i in 1:length(zerocinqsum)){
  zerocinqsumbis[[i]] <- data.frame(AC=zerocinqsum[[i]]$Value, Time=index(zerocinqsum[[i]]),
                                Monitor=rep(Zerocinqbis[[i]]$Monitor[1]), 
                                Food=rep("0.5%"), 
                                Channel=rep(Zerocinqbis[[i]]$Channel[1]),
                                Replicat=rep(Zerocinqbis[[i]]$Replicat[1]),
                                ID=z[i],
                                Hour=as.integer(strftime(index(zerocinqsum[[i]]), format = "%H", tz="")),
                                Month=as.integer(strftime(index(zerocinqsum[[i]]), format = "%m", tz="")),
                                Day=as.character(strftime(index(zerocinqsum[[i]]), format = "%d", tz=""))
  )
}
head(zerocinqsumbis[[1]])

#creation of a dataframe
DFcinqsum <- data.frame()
for (i in 1:length(cinqsumbis))
{
  DFcinqsum <- rbind(DFcinqsum, cinqsumbis[[i]])
}

DF0cinqsum <- data.frame()
for (i in 1:length(zerocinqsumbis))
{
  DF0cinqsum <- rbind(DF0cinqsum, zerocinqsumbis[[i]])
}
head(DFcinqsum)

DFsumactivity <- rbind(DFcinqsum, DF0cinqsum)
saveRDS(DFsumactivity, "./output/DFSUMactivitytot.rds")
