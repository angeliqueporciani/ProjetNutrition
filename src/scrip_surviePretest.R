getwd()
mydata<-read.table("replicat1_analyses_survie_prétest.txt",header=T)
attach(mydata)
summary(mydata)
moy<-tapply(Date,Concentration,mean,na.rm=T)
n<-tapply(Date,Concentration,sd,na.rm=T)
se<-n/sqrt(table(Concentration))
moy
se
model<-survfit(Surv(Date,Statut)~Conc)
plot(model)  #proba desurvie      censur? ? droite
plot(model,main="Impact de différentes concentration de glucose sur la survie d'Anopheles gambiae",ylab="Mosquito survivorship",xlab="days",col=c("blue", "green"),lwd=1.5, las=1,cex.axis=0.8)
legend("bottomleft",c("0.5%", "5%"), title="Sugar concentration",col=c("blue", "green"),lty=c(1,1),bty="n",cex=0.8)
model 
Conc<-as.factor(mydata$Conc)
modcox<-coxph(Surv(Date,Statut)~Conc)
anova(modcox)
glht_coxph <- glht(modcox, mcp(Conc = "Tukey"))
summary(glht_coxph)
