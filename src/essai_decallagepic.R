## difference de début d'activité. 

library(ggplot2)
library(plyr)
library(Rmisc)
library(cowplot)
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(MASS)
library(car)
library(emmeans)
library(lme4)
library(xts)
library(lubridate)
library(hms)

data10 <- readRDS("./output/DFSUMactivitytot.rds")
x <- c(18:23)
x2 <- c(0: 6)
xt <- c(x,x2)
data10 <- mutate(data10, Jour_nuit = if_else(Hour%in%xt,
                                           "Nuit", "Jour"))%>%
  mutate(actif = if_else(Value==0,
                         "non", "oui"))
summary(data10)
data10$Replicat <- as.factor(data10$Replicat)
data10$actif <- as.factor(data10$actif)
data10$Time2 <-as.factor(data10$Time) 
data10$Time3 <- as.factor(as.character(as_hms(data10$Time)))

dataR1 <- subset(data10, Replicat=="1")
#dataR1 <- subset(dataR1, Day=="20")
dataR1 <- subset(dataR1, Hour==18 | Hour==19)
summary(dataR1)

ggplot(data=dataR1, aes(x=Time3, y=Value, colour=Food))+
  geom_point()

dataR2 <- subset(data10, Replicat=="2")
summary(dataR2)
#dataR2 <- subset(dataR2, Day=="06")
dataR2 <- subset(dataR2, Hour==18 | Hour==19)

ggplot(data=dataR2, aes(x=Time3, y=Value, colour=Food))+
  geom_point()

#mod neg binom = detecte pas de difference entre les 2 regimes pour chaque pas de temps
mod1 <- glmmTMB(Value~Time3 * Food +(1|ID), data=dataR1, family=nbinom1)
summary(mod1)
Anova(mod1)

emmeans(mod1, pairwise ~ Food | Time3)

mod2 <- glmmTMB(Value~Time3 * Food +(1|ID), data=dataR2, family=nbinom1)
summary(mod2)
Anova(mod2)
emmeans(mod2, pairwise ~ Food | Time3)

# essai en binom
## transformation en binom : si sum valu==0 alors activity==0 else activity==1 
#R1
dataR1 <- dataR1 %>% 
  mutate(actif = if_else(Value==0,
                         "non", "oui"))
dataR1$actif <- as.factor(dataR1$actif)

mod1B <- glmmTMB(actif~Time3*Food +(1|ID), data=dataR1, family=binomial(link="logit"))
summary(mod2)
Anova(mod2)

emmeans(mod1B, pairwise ~ Food | Time3)

#R2
dataR2 <- dataR2 %>% 
  mutate(actif = if_else(Value==0,
                         "non", "oui"))
dataR2$actif <- as.factor(dataR2$actif)

mod2B <- glmmTMB(actif~Time3*Food +(1|ID), data=dataR2, family=binomial(link="logit"))
summary(mod2)
Anova(mod2)

emmeans(mod2B, pairwise ~ Food | Time3)

## Ttest
rb <- dataR1 %>%
pivot_wider(id_cols=c(ID, Food), names_from = Time, values_from = Value)

A2=apply(rb[,-c(1:2)],2, function (vec,gploc) t.test(vec~gploc, var.equal=F, na.rm=TRUE)$p.val, gploc=rb$Food)
A2

rb2 <- dataR2 %>%
  pivot_wider(id_cols=c(ID, Food), names_from = Time, values_from = Value)

A3=apply(rb2[,-c(1:2)],2, function (vec,gploc) t.test(vec~gploc, var.equal=F, na.rm=TRUE)$p.val, gploc=rb2$Food)
A3

## pool des 2 réplicats (Attention au horaire d'allumage/extinction qui diffère entre les 2 réplicats) et de tous les jours
summary(dataR)
dataR <- subset(data10, Hour==18 | Hour==19)
dataR$Time2 <-as.factor(dataR$Time) 
dataR$Time3 <- as.factor(as.character(as_hms(dataR$Time)))

glob1 <- ggplot(data=dataR, aes(x=Time3, y=Value, colour=Food))+
  geom_point()+
  ylab("Activité moyenne par 10 minutes")+
  xlab("Heure")+
  theme(axis.text.x = element_text(angle=35))+
  ggtitle("Activité moyenne pour tous les jours et réplicats")
glob1
ggsave(plot=glob1, filename="./img/onsetactivitytot.pdf")

glob2 <- ggplot(data=dataR, aes(x=Time3, y=Value, colour=Food))+
  geom_point()+
  ylab("Activité moyenne par 10 minutes")+
  xlab("Heure")+
  theme(axis.text.x = element_text(angle=35))+
  ggtitle("Activité moyenne pour tous les jours et réplicats")+
  facet_grid(Replicat ~ .)
glob2
ggsave(plot=glob2, filename="./img/onsetactivity_Repl.pdf")


# modèle global 
# binomial 
modbglob <- glmmTMB(actif~Time3*Food +(1|ID), data=dataR, family=binomial(link="logit"))
summary(modbglob)
Anova(modbglob)

emmeans(modbglob, pairwise ~ Food | Time3)

modbglob2 <- glmmTMB(actif~Time3*Food+Day +(1|ID), data=dataR, family=binomial(link="logit"))
summary(modbglob2)
Anova(modbglob2)

emmeans(modbglob2, pairwise ~ Food | Time3+Day)

# negbinom 

modNBglob <- glmmTMB(Value~Time3 * Food +(1|ID), data=dataR, family=nbinom1)
summary(modNBglob)
Anova(modNBglob)
emmeans(modNBglob, pairwise ~ Food | Time3)

## on peut égalmenet regarder l'effet du jour ... 


