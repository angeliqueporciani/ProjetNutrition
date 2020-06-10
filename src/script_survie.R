
getwd()
mydata<-read.table("./data/Survival/Survie_Analyses_Tousreplicats.txt", header=T)
mydata<-read.table("Survie_Analyses_Tousreplicats.txt",header=T)
library(survival)
library(emmeans)

##Mette "Replicats" en facteur comme "Concentration"
mydata$Replicats<-as.factor(mydata$Replicats)
str(mydata)

model<-survfit(Surv(mydata$Date,mydata$Statut)~mydata$Concentration)
plot(model)  #proba desurvie      censur? ? droite
plot(model,main="Impact de différentes concentration de glucose sur la survie d'Anopheles gambiae",ylab="Mosquito survivorship",xlab="days",col=c("blue", "green"),lwd=1.5, las=1,cex.axis=0.8)
legend("bottomleft",c("0.5%", "5%"), title="Sugar concentration",col=c("blue", "green"),lty=c(1,1),bty="n",cex=0.8)
model 
####(ajouter l'intervalle de confiance?) 

boxplot(mydata$Date,mydata$Concentration,ylab="Long?vit? moyenne (jour)",xlab="Concentration de glucose")  #PAS UTILE

model1<-coxph(Surv(mydata$Date,mydata$Statut)~mydata$Concentration*mydata$Replicats)
summary(model1)
model1
anova(model1)

##utilise emmeans au lie de relevel, ça te donne les contrast en bas qui t'interesse. 
emmeans(model1, pairwise~Concentration|Replicats)

##meme chose que "model" avec l'effet replicats en plus
model2<-survfit(Surv(mydata$Date,mydata$Statut)~mydata$Concentration+mydata$Replicats)
plot(model2)
plot(model2,main="Impact de diff?rentes concentration de glucose sur la survie d'Anopheles gambiae",ylab="Survie des moustiques",xlab="jours",col=c("lightblue", "royalblue", "midnightblue","lightgreen", "forestgreen", "darkolivegreen"),lwd=1.5, las=1,cex.axis=0.8)
legend("bottomleft",c("0.5R1", "0.5R2", "0.5R3", "5R1", "5R2", "5R3"), title="Sugar concentration",col=c("lightblue", "royalblue", "midnightblue", "lightgreen", "forestgreen", "darkolivegreen"),lty=c(1,1),bty="n",cex=0.8)
model2


##Changer l'intercepte d'origine pour pouvoir comparer ? autre chose que le r?plicat 1


###Ajout Karine pour pouvoir obtenir les p-values de l'effet concentration, réplicat, et de leur interaction #####
attach(mydata)
modkar<-coxph(Surv(Date,Statut)~Concentration+mydata$Replicats+Concentration:mydata$Replicats)
anova(modkar)
glht_coxph <- glht(modkar, mcp(Concentration = "Tukey"))
modsubset<-coxph(Surv(Date,Statut)~Concentration*mydata$Replicats)

#####Replicat 1 retiré, pour voir ce que çà donne###
Rep23<-subset(mydata, SousGroupe=="Y")
attach(Rep23)
summary(Rep23)
Rep23$Replicats<-as.factor(Rep23$Replicats)
str(Rep23)
model<-survfit(Surv(Date,Statut)~Rep23$Concentration)
plot(model)  #proba desurvie      censur? ? droite
plot(model,main="Impact de différentes concentration de glucose sur la survie d'Anopheles gambiae",ylab="Mosquito survivorship",xlab="days",col=c("blue", "green"),lwd=1.5, las=1,cex.axis=0.8)
legend("bottomleft",c("0.5%", "5%"), title="Sugar concentration",col=c("blue", "green"),lty=c(1,1),bty="n",cex=0.8)
model 
modkarrep23<-coxph(Surv(Date,Statut)~Concentration*Rep23$Replicats)
anova(modkarrep23)

moy<-tapply(Date,list(Concentration,Replicats),mean,na.rm=T)
n<-tapply(Date,list(Concentration,Replicats),sd,na.rm=T)
se<-n/sqrt(table(list(Concentration,Replicats)))
moy
se


attach(mydata)                         

moyall<-tapply(Date,Concentration,mean,na.rm=T)
nall<-tapply(Date,Concentration,sd,na.rm=T)
seall<-nall/sqrt(table(Concentration))
moyall
seall            
             