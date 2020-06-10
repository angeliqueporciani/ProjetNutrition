###################Résultats de la microscopie et de la qPCR

################################"###################################"
#######################""""""""microscopie####################"
#################################################################"
#################"réalisation graphique des résultats de la microscopie de la manipe C########"
microscopy<-read.table("microscopy_infection_C.txt",header=T)
summary(microscopy)
library(ggplot2)
library(plyr)
library(Rmisc)

#Figure 1_a: Prévalence en oocyste en fonction du statut 
attach(microscopy)
t<-table(microscopy$infection,microscopy$status)
t
d<-data.frame(t)
d

library(reshape2)
d2=dcast(d, status~infection,value.var="Freq")
d2
names(d2)[2] <- "uninf"###renommer la troisième colonne en "uninf"
names(d2)[3] <- "inf" ###renommer la quatrième colone en "inf"
d2
detach(microscopy)
attach(d2)
total<-uninf+inf
d2$total<-total
propinf<-inf/total
d2$propinf<-propinf
CI<-1.96*sqrt(propinf*(1-propinf)/total)
d2$CI<-CI
d2$status <- factor(d2$status, levels = c("Control","Exposed_diluted","Exposed")) 

ggplot(d2, aes(x=status, y=propinf, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=propinf, ymax=propinf+CI),width=.2,  position=position_dodge(.9))+
  xlab("Statut infectieux")+
  ylab("Prévalence en oocyste")+
  coord_cartesian(ylim = c(0, 1.1))+
  scale_fill_manual(values=c("white","grey","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.3, 0.90))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


#Figure 1_b: Proportion de moustques infectés à l'issu de la qPCR
qPCR<-read.table("qPCR_infection_C.txt",header=T)
summary(qPCR)
attach(qPCR)
t<-table(infection,status,period)
t
d<-data.frame(t)
d

library(reshape2)
d2=dcast(d, status+period~infection,value.var="Freq")
d2
names(d2)[3] <- "uninf"###renommer la troisième colonne en "uninf"
names(d2)[4] <- "inf" ###renommer la quatrième colone en "inf"
d2
detach(qPCR)
attach(d2)
tot<-uninf+inf
d2$tot<-tot
propinf<-inf/tot
d2$propinf<-propinf
CI<-1.96*sqrt(propinf*(1-propinf)/tot)
d2$CI<-CI


d2$period <- factor(d2$period, levels = c("6_9dpi","13_16dpi"))
d2$status <- factor(d2$status, levels = c("Control","Exposed-diluted","Exposed"))

library(ggplot2)
ggplot(d2, aes(x=period, y=propinf, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=propinf, ymax=propinf+CI),width=.2,  position=position_dodge(.9))+
  xlab("Période de test")+
  ylab("Proportion de moustiques infectés")+
  coord_cartesian(ylim = c(0, 1.1))+
  scale_fill_manual(values=c("white","grey","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.7, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

###### Analyse #####

## 1/ différence entre prévalence des dilués et des exposés? ###

x<-matrix(c(23,8,28,2),2,2)
x
chisq.test(x)

## ## 12 diff?rence entre proportions infect?s chez les expos?s ? 6_9 dpi (qPCR) et ? J7 (microscopie)? ###

x<-matrix(c(28,2,21,9),2,2)
x
chisq.test(x)

## ## 3/ diff?rence entre proportions infect?s chez les dilu?s ? 6_9 dpi (qPCR) et ? J7 (microscopie)? ###

x<-matrix(c(23,8,18,12),2,2)
x
chisq.test(x)

## ## 4/ diff?rence entre proportions infect?s chez les expos?s ? 6_9 dpi (qPCR) et ? 13-16 dpi (qPCR)? ###

x<-matrix(c(21,9,5,15),2,2)
x
chisq.test(x)

## ## 5/ diff?rence entre proportions infect?s chez les dilu?s ? 6_9 dpi (qPCR) et ? 13-16 dpi (qPCR)? ###

x<-matrix(c(18,12,9,23),2,2)
x
chisq.test(x)


# Figure 2_a: Le nombre moyen en oocyste pour chaque statut infecté

microscopy_inf<-subset(microscopy,oocyst>0) ### ne selectioner que les femelles infect?es
table(microscopy_inf$status)
microscopy_inf_mean <- summarySE(microscopy_inf, measurevar="oocyst", groupvars=c("status"))
summary(microscopy_inf_mean)
microscopy_inf_mean$status <- factor(microscopy_inf_mean$status, levels = c("Control","Exposed_diluted","Exposed")) 

ggplot(microscopy_inf_mean, aes(x=status, y=oocyst, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=oocyst, ymax=oocyst+se),width=.2,  position=position_dodge(.9))+
  xlab("Statut infectieux")+
  ylab("Nombre moyen en oocyste")+
  coord_cartesian(ylim = c(0, 40))+
  scale_fill_manual(values=c("grey","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.3, 0.90))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


##Figure 2_b: Nombre moyen d'ADN parasitaire pour chaque période et chaque groupe.
qPCR<-read.table("qPCR_infection_C.txt",header=T)
summary(qPCR)
library(tidyr)
qPCR_inf<-subset(qPCR,infection=="1")
attach(qPCR_inf)
library(plyr)
library(Rmisc)
summary(qPCR_inf)

qPCR2_mean <- summarySE(qPCR_inf, measurevar="Ct", groupvars=c("status", "period"),na.rm=T)
summary(qPCR2_mean)

qPCR2_mean$period <- factor(qPCR2_mean$period, levels = c("6_9dpi","13_16dpi"))
qPCR2_mean$status <- factor(qPCR2_mean$status, levels = c("Control","Exposed-diluted","Exposed"))

library(ggplot2)
ggplot(qPCR2_mean, aes(x=period, y=Ct, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=Ct, ymax=Ct+se),width=.2,  position=position_dodge(.9))+
  xlab("Période de test")+
  ylab("Ct moyen")+
  coord_cartesian(ylim = c(0, 40))+
  scale_fill_manual(values=c("white","grey","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.35, 0.94))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

###############################################################"""
############################################################""
#### Analyse du nombre d'oocyste ####

## 1/ existe t-il une diff?rence du nombre d'oocyste entre dilu?s et infect?s ? 

attach(microscopy_inf)
hist(oocyst)
shapiro.test(oocyst)

### pas de normalit? donc on oublie le test param?trique de comparaison de moyenne (t-test) et on part sur wilcoxon

wilcox.test(oocyst~status)


## 2/ diff?rence de Ct entre expos?s et dilu?s ? 6_9 dpi 
summary(qPCR_inf)
qPCR_inf2<-subset(qPCR_inf, !status=="Control"&!period=="13_16dpi")
summary(qPCR_inf2)
attach(qPCR_inf2)
hist(Ct)
shapiro.test(Ct)
fligner.test(Ct~status)
t.test(Ct~status)

## 2/ différence de Ct entre exposés et dilués  ? 13_16 dpi
summary(qPCR_inf)
qPCR_inf3<-subset(qPCR_inf, !status=="Control"&!period=="6_9dpi")
summary(qPCR_inf3)
attach(qPCR_inf3)
hist(Ct)
shapiro.test(Ct)
fligner.test(Ct~status)
t.test(Ct~status)



#######################################################################################
####################################Mesure de l'activité###################################"
##############################################################################################

actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever les 6 moustiques controles infect?s ####

actinf2<-subset(actinf,!group=="CI"| is.na(group))

summary(actinf2)
#######Enlever les NA's des morts dans la colonne "activity"

library(tidyr)
actinf3<-actinf2 %>% drop_na(activity)
actinf3<-droplevels(actinf3)
summary(actinf3)

library(ggplot2)
library(plyr)
library(Rmisc)

######################Figure 3_a: Activité moyenne du moustique par heure en fonction du statut et de la période 
actinf3_mean <- summarySE(actinf3, measurevar="activity", groupvars=c("period","status", "hour"),na.rm=T)

actinf3_mean$period <- factor(actinf3_mean$period, levels = c("3_6dpi","6_9dpi","13_16dpi")) 
actinf3_mean$status <- factor(actinf3_mean$status, levels = c("Control","Exposed_diluted","Exposed")) 


ggplot(actinf3_mean, aes(x=hour, y=activity, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity-se, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Heure")+
  ylab("Activité moyenne par heure")+
  coord_cartesian(ylim = c(0, 750))+
  scale_fill_manual(values=c("white","grey","black"))+
  facet_grid(period~ status)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.75, 0.55))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


##### Figure 3_b: Activité moyenne du moustique par jour en fonction du statut et de la période
actinf3_mean1 <- summarySE(actinf3, measurevar="activity", groupvars=c("period","status", "hour_ND"),na.rm=T)
summary(actinf3_mean1)
actinf3_mean1$period <- factor(actinf3_mean1$period, levels = c("3_6dpi","6_9dpi","13_16dpi")) 
actinf3_mean1$status <- factor(actinf3_mean1$status, levels = c("Control","Exposed_diluted","Exposed")) 
ggplot(actinf3_mean1, aes(x=hour_ND, y=activity,fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity-se, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Temps (Day/Night)")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 300))+
  scale_fill_manual(values=c("white","grey","black"))+
  facet_wrap(~ period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.85, 0.85))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


####### ANALYSE STAT de l'activit? (fig 3a et b)
summary(actinf3)
attach(actinf3)

hist(activity)

library(lme4)


##### Evaluer l'effet principal de "period".

mod1<-glmer.nb(activity~period+(1|ID2))
summary(mod1)
mod2<-glmer.nb(activity~1+(1|ID2))
anova(mod1,mod2,test="Chi")

######################"""""exo
mod1<-glmer.nb(activity~period*status+(1|ID2))
mod2<-glmer.nb(activity~period+status+(1|ID2))
anova(mod1,mod2,test="Chi")
mod3<-glmer.nb(activity~period+(1|ID2))
anova(mod2,mod3,test="Chi")
mod4<-glmer.nb(activity~1+(1|ID2))
anova(mod3,mod4,test="Chi")
###############"

##### P?riode 3_6 dpi ####
library(lme4)
summary(actinf3)
p1<-subset(actinf3,period=="3_6dpi")
p1<-droplevels(p1)
attach(p1)
mod1<-glmer.nb(activity~status*hour_ND+(1|ID2))
mod1<-glmer.nb(activity~status+hour_ND+status:hour_ND+(1|ID2))
summary(mod1)
mod2<-glmer.nb(activity~status+hour_ND+(1|ID2))
anova(mod1,mod2,test="Chi")
summary(mod2)
mod3<-glmer.nb(activity~hour_ND+(1|ID2))
anova(mod2,mod3,test="Chi")


library(MASS)

mod1<-glm.nb(activity~status*hour_ND )

anova(mod1)

summary(mod1)
anova(mod1,test="Chi")
library(car)
Anova(mod1,type=3)




####Figure 4_a: Activité moyenne du moustique en fonction de l'heure, du group et de la période 

actinf3_mean <- summarySE(actinf3, measurevar="activity", groupvars=c("period","group", "hour"),na.rm=T)
summary(actinf3_mean)
actinf3_mean2<-subset(actinf3_mean,!period=="3_6dpi")
actinf3_mean3<-subset(actinf3_mean2,!group=="NA")
actinf3_mean3<-droplevels(actinf3_mean3)
actinf3_mean3$period <- factor(actinf3_mean3$period, levels = c("6_9dpi","13_16dpi")) 
ggplot(actinf3_mean3, aes(x=hour, y=activity, fill=group)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity-se, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Temps (heure)")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 1000))+
  scale_fill_manual(values=c("white","darkred","grey","red","black"))+
  facet_grid(group~period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = "none")+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


##### Figure 4_b: Activité moyenne du moustique en fonction de Jour/nuit, du groupe et de la période
actinf3_mean4 <- summarySE(actinf3, measurevar="activity", groupvars=c("period","group", "hour_ND"),na.rm=T)
actinf3_mean4<-subset(actinf3_mean4,!period=="3_6dpi")
actinf3_mean4<-subset(actinf3_mean4,!group=="NA")
actinf3_mean4<-droplevels(actinf3_mean4)
actinf3_mean4$period <- factor(actinf3_mean4$period, levels = c("6_9dpi","13_16dpi")) 

ggplot(actinf3_mean4, aes(x=hour_ND, y=activity,fill=group)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity-se, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Temps (Day/Night)")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 300))+
  scale_fill_manual(values=c("white","darkred","grey","red","black"))+
  facet_wrap(~ period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.60, 0.83))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

#########################################"""
#################Evaluer l'effet principal du groupe sur l'activité
mod3<-glmer.nb(activity~group*period+(1|ID2))
summary(mod3)
mod4<-glmer.nb(activity~group+period+(1|ID2))
anova(mod3,mod4,test="Chi")
mod5<-glmer.nb(activity~group+(1|ID2))
anova(mod4,mod5,test="Chi")
mod6<-glmer.nb(activity~1+(1|ID2))
anova(mod6,mod5,test="Chi")
###################################"
#################################

##### Figure 5: activity en fonction de Ct pour les deux groupes d'infect?s (EI et EDI) et pour les 2 p?riodes 6_9 dpi et 13-16 dpi ####

actinf4<-subset(actinf3,group== c("EDI","EI"))
summary(actinf4)
actinf4<-droplevels(actinf4)
actinf4$period <- factor(actinf4$period, levels = c("6_9dpi","13_16dpi")) 
ggplot(actinf4, aes(x=Ct, y=activity,color=group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  scale_colour_manual(values=c("darkred","red"))+
  coord_cartesian(ylim=c(0,200))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.55, 0.85))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Ct")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "group")




##### Figure 6: Activité moyenne du moustique en fonction de Jour/nuit, du groupe et de la période SUR FENETRE 22-4h
summary(actinf3)
actinf5<-subset(actinf3,!hour%in%c(5:21))
actinf5<-droplevels(actinf5)
summary(actinf5)
actinf5_mean <- summarySE(actinf5, measurevar="activity", groupvars=c("period","group"),na.rm=T)
summary(actinf5_mean)
actinf5_mean2<-subset(actinf5_mean,!period=="3_6dpi")
actinf5_mean3<-subset(actinf5_mean2,!group=="NA")
actinf5_mean3<-droplevels(actinf5_mean3)
actinf5_mean3$period <- factor(actinf5_mean3$period, levels = c("6_9dpi","13_16dpi")) 

ggplot(actinf5_mean3, aes(group, activity,fill=group)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity-se, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Groupe")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 200))+
  scale_fill_manual(values=c("white","darkred","grey","red","black"))+
  facet_wrap(~ period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.60, 0.80))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


##### Figure 7: activity en fonction de Ct pour les deux groupes d'infect?s (EI et EDI) et pour les 2 p?riodes 6_9 dpi et 13-16 dpi  SUR FENETRE 22-4h ####

actinf6<-subset(actinf5,group== c("EDI","EI"))
summary(actinf6)
actinf6<-droplevels(actinf6)
actinf6$period <- factor(actinf6$period, levels = c("6_9dpi","13_16dpi")) 
ggplot(actinf6, aes(x=Ct, y=activity,color=group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  coord_cartesian(ylim=c(0,200))+
  scale_colour_manual(values=c("darkred","red"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.85, 0.85))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Ct")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "group")


##############################iimportation des données
actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)
#### enlever les 6 moustiques controles infect?s ####
actinf2<-subset(actinf,!group=="CI"| is.na(group))
summary(actinf2)
##############""
actinf3<-subset(actinf2,!period=="3_6dpi")
summary(actinf3)
actinf4<-subset(actinf3,!winglength=="NA")
summary(actinf4)
library(tidyr)
actinf5<-actinf4 %>% drop_na(activity) #######Enlever les NA's des morts dans la colonne "activity"
actinf5<-droplevels(actinf5)
summary(actinf5)

##############"indépendamment de du groupe.....
##############Figure 8_a: Nombre moyen de passage par heure en fonction de la taille par groupe et par période
library(ggplot2)
actinf5$period <- factor(actinf5$period, levels = c("6_9dpi","13_16dpi"))

ggplot(actinf5, aes(x=winglength, y=activity)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  coord_cartesian(ylim=c(0,300))+
  scale_colour_manual(values=c("white","darkred","grey","red","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.55, 0.80))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Taille de l'aile (mm)")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "group")

###############""dépendant du groupe####
##############Figure 8_b: Nombre moyen de passage par heure en fonction de la taille par groupe et par période
library(ggplot2)
actinf5$period <- factor(actinf5$period, levels = c("6_9dpi","13_16dpi"))

ggplot(actinf5, aes(x=winglength, y=activity, color=group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  coord_cartesian(ylim=c(0,300))+
  scale_colour_manual(values=c("blue","darkred","grey","red","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.55, 0.80))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Taille de l'aile (mm)")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "group")

###########################################○""
################""le nombre moyen de passage par heure en fonction de la taille  et de la période
actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever les 6 moustiques controles infect?s ####
actinf2<-subset(actinf,period=="3_6dpi")
library(tidyr)
actinf3<-actinf2 %>% drop_na(activity)
actinf3<-droplevels(actinf3)
summary(actinf3)
#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)

##############Figure 9: Nombre moyen de passage par heure en fonction de la taille par statut et par période
actinf3$status <- factor(actinf3$status, levels = c("Control","Exposed_diluted","Exposed"))
ggplot(actinf3, aes(x=winglength, y=activity,color=status)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  coord_cartesian(ylim=c(0,350))+
  scale_colour_manual(values=c("blue","grey","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.66, 0.85))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Taille de l'aile (mm)")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "Status")



#####################################""""
###########################""activité en fonction du nombre d'oeuf

actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever les 6 moustiques controles infect?s ####

actinf2<-subset(actinf,!group=="CI"| is.na(group))

summary(actinf2)
#######Enlever les NA's des morts dans la colonne "activity"

library(tidyr)
actinf3<-actinf2 %>% drop_na(activity)#####enlever les Na's dans activity
actinf4<-actinf3 %>% drop_na(eggs)##enlever les NA's dans la colonne oeuf
actinf5<-subset(actinf4,!period=="3_6dpi"| is.na(period))###exclure la période 3-6dpi
actinf5<-droplevels(actinf5)
summary(actinf5)

#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)


##############Figure 10: Nombre moyen de passage par heure en fonction du nombre d'oeuf par groupe et par période
actinf5$period <- factor(actinf5$period, levels = c("6_9dpi","13_16dpi"))
actinf5$group <- factor(actinf5$group, levels = c("CU","EDI","EDU","EI",'EU'))
ggplot(actinf5, aes(x=eggs, y=activity,color=group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  coord_cartesian(ylim=c(0,300))+
  scale_colour_manual(values=c("blue","darkred","grey","red","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.90, 0.7))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Nombre d'oeuf")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "group")


##############################################################################################"
###########nombre d'oeuf en fonction du statut

actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever les périodes 6_9dpi et 13_16dpi
actinf2<-subset(actinf,period=="3_6dpi")
library(tidyr)
actinf3<-actinf2 %>% drop_na(activity)#####enlever les Na's dans activity
actinf4<-actinf3 %>% drop_na(eggs)##enlever les NA's dans la colonne oeuf
actinf4<-droplevels(actinf4)
summary(actinf4)

#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)


##############Figure 11: Nombre moyen de passage par heure en fonction du nombre d'oeuf par statut et par période
actinf4$status <- factor(actinf4$status, levels = c("Control","Exposed_diluted","Exposed"))
ggplot(actinf4, aes(x=eggs, y=activity,color=status)) +
  geom_point(shape=1) +
  geom_smooth(method=lm,se=FALSE)+
  facet_wrap(~period)+
  coord_cartesian(ylim=c(0,300))+
  scale_colour_manual(values=c("blue","grey","black"))+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(panel.background = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_line(colour = "black"))+
  theme(axis.text=element_text(size=12,colour="black"),axis.title=element_text(size=14))+
  xlab("Nombre d'oeuf")+
  ylab("Nombre moyen de passage/heure")+
  labs(color = "Status")

#####################################################""
######################Statut d'insémination###################
actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever la période 3_6dpi
actinf2<-subset(actinf,!period=="3_6dpi"| is.na(period))
actinf3<-subset(actinf2,!group=="CI"| is.na(group))
library(tidyr)
actinf4<-actinf3 %>% drop_na(activity)#####enlever les Na's dans activity
actinf5<-actinf4 %>% drop_na(insemination )##enlever les NA's dans la colonne insemination
actinf5<-droplevels(actinf5)
summary(actinf5)

actinf5$period <- factor(actinf5$period, levels = c("6_9dpi","13_16dpi"))
actinf5$group <- factor(actinf5$group, levels = c("CU","EDI","EDU","EI","EU"))

##############Figure 12_a: Nombre moyen de passage par heure en fonction du statut d'insémination par groupe et par période
#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)
actinf5_mean <- summarySE(actinf5, measurevar="activity", groupvars=c("period","group", "insemination"),na.rm=T)
summary(actinf5_mean)
ggplot(actinf5_mean, aes(x=insemination, y=activity, fill=group)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Statut d'insémination")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 250))+
  scale_fill_manual(values=c("white","darkred","grey","red","black"))+
  facet_wrap(~period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.7, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

#################################################en fontion du statut (période 3_6dpi)
actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever les période 6_9dpi et 13_16dpi
actinf2<-subset(actinf,!period=="6_9dpi"| is.na(period))
actinf3<-subset(actinf2,!period=="13_16dpi"| is.na(period))
library(tidyr)
actinf4<-actinf3 %>% drop_na(activity)#####enlever les Na's dans activity
actinf5<-actinf4 %>% drop_na(insemination )##enlever les NA's dans la colonne insemination
actinf5<-droplevels(actinf5)
summary(actinf5)

actinf5$status <- factor(actinf5$status, levels = c("Control","Exposed_diluted","Exposed"))

##############Figure 12_b: Nombre moyen de passage par heure en fonction du statut d'insémination par statut durant la période 3_6
#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)
actinf5_mean <- summarySE(actinf5, measurevar="activity", groupvars=c("period","status", "insemination"),na.rm=T)
summary(actinf5_mean)
ggplot(actinf5_mean, aes(x=insemination, y=activity, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Statut d'insémination")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 250))+
  scale_fill_manual(values=c("white","grey","black"))+
  facet_wrap(~period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.4, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))


####################################################################"""
##################################"Activité en fonction du statut gonotrophique#####"
#############################################"""

actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever la période 3_6dpi
actinf2<-subset(actinf,!period=="3_6dpi"| is.na(period))
actinf3<-subset(actinf2,!group=="CI"| is.na(group))
library(tidyr)
actinf4<-actinf3 %>% drop_na(activity)#####enlever les Na's dans activity
actinf5<-actinf4 %>% drop_na(gravidity )##enlever les NA's dans la colonne gravidity
actinf5<-droplevels(actinf5)
summary(actinf5)

actinf5$period <- factor(actinf5$period, levels = c("6_9dpi","13_16dpi"))
actinf5$group <- factor(actinf5$group, levels = c("CU","EDI","EDU","EI","EU"))

############"Figure 13_a: Activité moyenne en fonction du groupe et des périodes 6_9 et 13_16dpi
#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)
actinf5_mean <- summarySE(actinf5, measurevar="activity", groupvars=c("period","group", "gravidity"),na.rm=T)
summary(actinf5_mean)
ggplot(actinf5_mean, aes(x=gravidity, y=activity, fill=group)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Statut gonotrophique")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 270))+
  scale_fill_manual(values=c("white","darkred","grey","red","black"))+
  facet_wrap(~period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.7, 0.8))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

#################################################en fontion du statut (période 3_6dpi)
actinf<-read.table("activity_hour2_2.txt",header=T)
summary(actinf)

#### enlever les période 6_9dpi et 13_16dpi
actinf2<-subset(actinf,!period=="6_9dpi"| is.na(period))
actinf3<-subset(actinf2,!period=="13_16dpi"| is.na(period))
library(tidyr)
actinf4<-actinf3 %>% drop_na(activity)#####enlever les Na's dans activity
actinf5<-actinf4 %>% drop_na(gravidity)##enlever les NA's dans la colonne insemination
actinf5<-droplevels(actinf5)
summary(actinf5)

actinf5$status <- factor(actinf5$status, levels = c("Control","Exposed_diluted","Exposed"))


#########################figure 13_b: activité moyenne/heure en fonction du status et de la période 3_6dpi
#####package ggplot
library(ggplot2)
library(plyr)
library(Rmisc)
actinf5_mean <- summarySE(actinf5, measurevar="activity", groupvars=c("period","status", "gravidity"),na.rm=T)
summary(actinf5_mean)
ggplot(actinf5_mean, aes(x=gravidity , y=activity, fill=status)) + 
  geom_bar(stat = "identity", color="black", position=position_dodge())+
  geom_errorbar(aes(ymin=activity, ymax=activity+se),width=.2,  position=position_dodge(.9))+
  xlab("Statut gonotrophique")+
  ylab("Nombre moyen de passage/heure")+
  coord_cartesian(ylim = c(0, 270))+
  scale_fill_manual(values=c("white","grey","black"))+
  facet_wrap(~period)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ 
  theme(panel.background = element_blank()) + theme(panel.border = element_blank()) + 
  theme(axis.text=element_text(size=14,colour="black"),axis.title=element_text(size=14))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.position = c(0.6, 0.9))+
  theme(legend.title = element_blank())+
  theme(legend.background = element_rect(fill="white"),legend.key = element_rect(fill = "white", color = NA))

#######################################################################################################################"
####################################################################################################################"
#######################################################################################################################"
########################################################################################################################
######################################################################################################################

