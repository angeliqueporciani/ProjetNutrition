library(ggplot2)
library(plyr)
library(Rmisc)
library(cowplot)
library(tidyverse)
library(dplyr)
library(glmmTMB)
library(car)
library(DHARMa)
library(bbmle)
library(sjPlot)
library(sjmisc)
library(emmeans)
detach(TSstudio)
library(MASS)

## creation des databases 
data10 <- readRDS("./output/DFSUMactivitytot.rds")
data10$Replicat <- as.factor(data10$Replicat)
data10$ID <- as.factor(data10$ID)
# creation variable "jour/nuit"
x <- c(18:23)
x2 <- c(0: 6)
xt <- c(x,x2)
data10 <- mutate(data10, Jour_nuit = if_else(Hour%in%xt,
                                             "Nuit", "Jour"))

# selection que de nuit 
data10bis <- subset(data10, Jour_nuit=="Nuit")
# creation hour en lag 
x1 <- c(6:12)
x2 <- c(0:5)
x <- as.factor(c(x1, x2))
data10bis$Hour2 <- as.factor(data10bis$Hour)
levels(data10bis$Hour2) <- x
data10bis$Hour3 <- as.integer(as.character(data10bis$Hour2))

# ajout d'un indice pour identifier la nuit d'enregistrement. 
data10bis <- mutate(data10bis, Nuit = case_when(
  data10bis['Time'] >= '2020-02-20 18:00:00' & data10bis['Time'] <= '2020-02-21 06:00:00' ~"N1",
  data10bis['Time'] >= '2020-02-21 18:00:00' & data10bis['Time'] <= '2020-02-22 06:00:00' ~"N2",
  data10bis['Time'] > '2020-02-22 18:00:00' & data10bis['Time'] <= '2020-02-23 06:00:00' ~ "N3"))

data10bis$Nuit <- as.factor(data10bis$Nuit)
          
N1 <- subset(data10bis, Nuit=="N1")
N1$ID <- droplevels(N1$ID)
N2 <- subset(data10bis, Nuit=="N2")
N2$ID <- droplevels(N2$ID)
N3 <- subset(data10bis, Nuit=="N3")
N3$ID <- droplevels(N3$ID)

#################
## MODELE #######

hist(data10bis$Value, breaks=100)## ya encore bcp de 0, on y echapera pas.

#standard zero-inflated negative binomial model, ici ZI depend de toute les variables
mod1 <- glmmTMB(Value~Food+Replicat+Nuit+(1|ID), zi=~1, data=data10bis, family = nbinom1)
mod2 <- glmmTMB(Value~Food*Replicat+Food*Nuit +(1|ID),zi=~1, data=data10bis, family = nbinom1)
mod3 <- glmmTMB(Value~Food*Replicat*Nuit+ (1|ID), zi=~1, data=data10bis, family = nbinom1)

# ZI en fonction de l'heure uniquement. Le pb ici c'est qu'on fait l'hypothèse d'une relation linéaire entre la 0 inflation et 
#l'heure ce qui est faux, ca serait plus une relation sinusoidale. 

mod4 <- glmmTMB(Value~Food*Replicat*Nuit+(1|ID), zi=~Hour3, data=data10bis, family = nbinom1)

# ajout de l'heure en lag de temps
mod5 <- glmmTMB(Value~Food*Replicat*Nuit*Hour3, zi=~1, data=data10bis, family = nbinom1)
mod6 <- glmmTMB(Value~Food*Replicat*Nuit*Hour3, zi=~Hour3, data=data10bis, family = nbinom1)

## comp modèle 
AICtab(mod1, mod2, mod3, mod4, mod5, mod6)#best=Mod4

# validation  
# anova pour connaitre l'effet des variables peut importe les modalités. 
Anova(mod4)
Anova(mod6)
summary(mod6)

res <- simulateResiduals(mod4)
plot(res)# ca vas

res <- simulateResiduals(mod6)
plot(res)# parfait, il s'ajuste bien! 

#plot des coeff estimé. 
plot_model(mod4, type="pred", terms = c("Replicat", "Food","Nuit"))
plot_model(mod6, type="pred", terms = c("Hour3","Food","Replicat","Nuit"))

# CCL : 
# le modèle global s'ajuste bien et nous donne les infos qui nous interesse. 
# le  meilleur modèle qui s'ajuste d'après l'AIC est un modèle qui tiens compte du régime+replicat+Nuit et de l'effet de l'heure sur la ZI. 
# Cependant, comme l'effet de l'heure, même s'il n'est pas majoritaire dans l'effet, nous interesse, c'est pourquoi j'ai quand même appliqué les étapes post-hoc sur ce modèle.

# L'heure est l'echelle maximale que l'on peut atteindre avec ces modèles, mes essais avec les obs par 10min donnent des modèles qui ne convergent pas. 
# cela vient probablement du fait que l'on suppose une relation lineaire entre le temps et la réponse, ce qui n'est pas le cas à l'echelle de 10min alors qu'on peut le supposer pour une echelle de 1h. 
# Si on veut modéliser ça, il faut partir sur des modèles plus complexes (hurdle comme Carlo ou HMM...). Cependant, ce n'est peut être pas l'objectif ici, nous allons donc rester sur l'heure. 

## recupération des coefficients
emmeans(mod4, pairwise ~ Food | Replicat+Nuit)
emmeans(mod6, pairwise ~ Food | Replicat+Nuit+Hour3)
rf <- ref_grid(mod6, at = list(Hour3=c(0,6,12)))
emmeans(rf, pairwise ~ Food | Replicat+Nuit+Hour3)

-----
############################################################
## travail par nuit qui n'apporte pas vraiment plus. 
############################################################
#N1

mod1_N1 <- glmmTMB(Value~Food+Replicat + (1|ID), zi=~1, data=N1, family = nbinom1)
mod2_N1 <- glmmTMB(Value~Food*Replicat +(1|ID),zi=~1, data=N1, family = nbinom1)
mod3_N1 <- glmmTMB(Value~Food*Replicat*Hour3 + (1|ID), zi=~1, data=N1, family = nbinom1)#pb convergence
mod4_N1 <- glmmTMB(Value~Food*Replicat+(1|ID), zi=~Hour3, data=N1, family = nbinom1)
mod5_N1 <- glmmTMB(Value~Food*Replicat*Hour3, zi=~Hour3, data=N1, family = nbinom1)

## comp modèle 
AICtab(mod1_N1, mod2_N1, mod4_N1, mod5_N1)#
anova(mod1_N1, mod4_N1) 
Anova(mod1_N1)

# verif de l'ajustement. 
res <- simulateResiduals(mod1_N1)
plot(res)

res <- simulateResiduals(mod5_N1)
plot(res)

#plot des coeff estimé. 
plot_model(mod1_N1, type="pred", terms = c("Food","Replicat"), title="Activity Day20")
plot_model(mod5_N1, type="pred", terms = c("Hour","Food","Replicat"), title="Activity Day20")

## N2

mod0_N2 <- glmmTMB(Value~Food+Replicat, zi=~1, data=N2, family = nbinom1)
mod1_N2 <- glmmTMB(Value~Food+Replicat + (1|ID), zi=~1, data=N2, family = nbinom1)# converge pas, je comprend pas pk 
mod2_N2 <- glmmTMB(Value~Food*Replicat + (1|ID),zi=~1, data=N2, family = nbinom1)#congverge pas non plus...
mod3_N2 <- glmmTMB(Value~Food*Replicat*Hour3 + (1|ID), zi=~1, data=N2, family = nbinom1)
mod4_N2 <- glmmTMB(Value~Food*Replicat+(1|ID), zi=~Hour3, data=N2, family = nbinom1)# converge pas 
mod5_N2 <- glmmTMB(Value~Food*Replicat*Hour3+(1|ID), zi=~Hour3, data=N2, family = nbinom1)
summary(mod2_22)
summary(mod1_22)
summary(mod3_22)

## comp modèle 
AICtab(mod0_N2, mod3_N2, mod5_N2)

anova(mod3_N2, mod5_N2)## diff signif on garde 5 
Anova(mod5_N2)

# verif de l'ajustement. 
res <- simulateResiduals(mod5_N2)
plot(res)# bien!

#plot des coeff estimé. 
#plot_model(mod3_22, type="pred", terms = c("Hour","Food","Replicat"))
plot_model(mod5_N2, type="pred", terms = c("Hour3","Food","Replicat"), title="Activité de le 2ème nuit")


## N3
mod1_N3 <- glmmTMB(Value~Food+Replicat + (1|ID), zi=~1, data=N3, family = nbinom1)
mod2_N3 <- glmmTMB(Value~Food*Replicat +(1|ID),zi=~1, data=N3, family = nbinom1)
mod3_N3 <- glmmTMB(Value~Food*Replicat*Hour3 + (1|ID), zi=~1, data=N3, family = nbinom1)# pour la comparaison avec Hour3
mod4_N3 <- glmmTMB(Value~Food*Replicat+(1|ID), zi=~Hour3, data=N3, family = nbinom1)
mod5_N3 <- glmmTMB(Value~Food*Replicat*Hour3+(1|ID), zi=~Hour3, data=N3, family = nbinom1)

## comp modèle 
AICtab(mod1_N3, mod2_N3, mod3_N3, mod4_N3, mod5_N3)

## mod 5 the best
anova(mod5_N3, mod3_N3)## diff signif on garde 5 
Anova(mod5_N3)
summary(mod5_N3)
# verif de l'ajustement. 
res <- simulateResiduals(mod5_N3)
plot(res)# pas idéal mais ça peut passer. 

#plot des coeff estimé. 
#plot_model(mod3_21, type="pred", terms = c("Hour","Food","Replicat"))
plot_model(mod5_N3, type="pred", terms = c("Hour3","Food","Replicat"), title="Activité de la 3ème nuit")

## on peut aussi s'amuser a regarder la différence en fonction de la période de la nuit(debut/fin) mais ça apporte pas plus d'info.
# je laisse le code d'ajout de la covar pour d'autre perspectives, mais j'ajoute pas les modèles. 

#ajout d'une covar "partie"
x2 <- c(0:5)
N1 <- mutate(N1, Partie = if_else(Hour3%in%x2,
                                             "1", "2"))
N1$Partie <- as.factor(N1$Partie)
N2 <- mutate(N2, Partie = if_else(Hour3%in%x2,
                                  "1", "2"))
N2$Partie <- as.factor(N2$Partie)
N3 <- mutate(N3, Partie = if_else(Hour3%in%x2,
                                  "1", "2"))
N3$Partie <- as.factor(N3$Partie)

# pour les modèles avec partie faudra pas laisser l'heure car il vas y avoir trop de covariance. 

### on peut tenter la normalisation 

n <- scale(N1$Value, center=TRUE, scale=TRUE)#Zscore=centré-reduire. 
plot(N1$Hour3,n, type="h")
plot(N1$Value, type="h")
# je comprend pas ce que ça apporte ici... 
# faire en plus de group_by ? 

ZS <- data10bis %>%
  group_by(Hour, Nuit) %>%
mutate(Zscore=scale(Value, center=TRUE, scale=TRUE)) %>%
  mutate(var=var(Value))

VT <- var.test(data10bis$Value, data10bis$Food)
VT <- var.test(ZS$Zscore, ZS$Food)
plot(ZS$Zscore, type="h")
str(data10bis)

leveneTest(Zscore~Nuit, data=ZS, center="mean")
leveneTest(Zscore~Nuit*Food*Replicat, data=ZS, center="mean")
leveneTest(Value~Nuit*Food*Replicat, data=ZS, center="mean")
## regarder du coté de mes cours de série temporelle pour la transfo plutôt ... 