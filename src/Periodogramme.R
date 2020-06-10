## selection des ID acycliques

## load package
library(xsp)
library(doRNG)
## load data 
source("./src/01 Subset_actifs.R")

# Periodogramme 

#5% 

chiSqPeriodograms <- foreach(i=1:length(Cinqtotbis), .packages = 'xsp') %dopar% {
  df <- data.frame(dateTime = seq_along(Cinqtotbis[[i]]), value = Cinqtotbis[[i]])
  names(df) <- c("dateTime", "value")
  chiSqPeriodogram(df, res = 1)
}

acyclic <- vector()
for (i in 1:length(chiSqPeriodograms)){
  diff <- chiSqPeriodograms[[i]]$Qp.act-chiSqPeriodograms[[i]]$Qp.sig
  a <- which.max(diff)
  b <- diff[a]
  if(isTRUE(b<0)){
    acyclic[i] <- i
  }
}

acyclic <- acyclic[!is.na(acyclic)]
length(acyclic)

## 60-46 =14 ... ca laisse que 14 ID chez les 5% 
#Verif graphique 
plot(Cinqtotbis[[59]], type="l")
chiSqPeriodogramPlot(chiSqPeriodograms[[59]])

# 0.5% 

chiSqPeriodograms05 <- foreach(i=1:length(ZeroCinqtotbis), .packages = 'xsp') %dopar% {
  df <- data.frame(dateTime = seq_along(ZeroCinqtotbis[[i]]), value = ZeroCinqtotbis[[i]])
  names(df) <- c("dateTime", "value")
  chiSqPeriodogram(df, res = 1)
}

acyclic05 <- vector()
for (i in 1:length(chiSqPeriodograms05)){
  diff <- chiSqPeriodograms05[[i]]$Qp.act-chiSqPeriodograms05[[i]]$Qp.sig
  a <- which.max(diff)
  b <- diff[a]
  if(isTRUE(b<0)){
    acyclic05[i] <- i
  }
}

acyclic05 <- acyclic05[!is.na(acyclic05)]
length(acyclic05)

plot(ZeroCinqtotbis[[3]], type="l")
chiSqPeriodogramPlot(chiSqPeriodograms05[[3]])

#############################

#46 chez les 5% 
## 5% R1= 22
##5% R2 = 24 

## 52 acyclic chez 0.5% 
## 0.5% R1 = 27 acyclic
## 05 R2 = 25 

# acyclicité est peut être un paramètre trop restrictif ici ou bien les périodogrammes ne sont pas l'outils adapté. 


