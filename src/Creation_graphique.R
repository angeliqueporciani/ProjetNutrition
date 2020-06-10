## creation graphiques 

library(ggplot2)
library(plyr)
library(Rmisc)
library(cowplot)
library(tidyverse)
library(dplyr)

data10 <- readRDS("./output/DFSUMactivitytot.rds")
head(data10)
str(data10)

data10$Replicat <- as.factor(data10$Replicat)
data10$ID <- as.factor(data10$ID)

## Graphique : activité moyenne (10min) par min/replicat/food
R1 <- subset(data10, Replicat=="1")
R2 <- subset(data10, Replicat=="2")

act1 <- summarySE(R1, measurevar="Value", groupvars=c("Replicat","Food", "Time"),na.rm=T) #comptage moyenne ?cart type et
act2 <- summarySE(R2, measurevar="Value", groupvars=c("Replicat","Food", "Time"),na.rm=T) #comptage moyenne ?cart type et

head(act1)
names(act1)
str(act1)
act1$Hour <- hour(act1$Time)
act2$Hour <- hour(act2$Time)

act1$Interaction <- with(act1, interaction(Replicat, Food))
act2$Interaction <- with(act2, interaction(Replicat, Food))

cols <- c("royalblue4", "red4", "royalblue2", "red1")
names(cols) <- levels(act1$Interaction)

# Graphes sur des grid séparées

act1$Interaction <- factor(act1$Interaction, levels = c("1.5%", "1.0.5%", "2.5%", "2.0.5%"))
head(act1)
p1 <- ggplot(data=act1, aes(x=Time, y=Value))+
  geom_rect(aes(NULL,NULL,
                xmin = as.POSIXct("2020-02-20 17:56:00"), xmax = as.POSIXct("2020-02-21 05:55:00"),
                ymin = 0, ymax = +Inf,
                colour = NULL),
            fill = 'grey95', alpha = 0.1)+
  geom_rect(aes(NULL,NULL,
                xmin = as.POSIXct("2020-02-21 17:56:00"), xmax = as.POSIXct("2020-02-22 05:55:00"),
                ymin = 0, ymax = +Inf,
                colour = NULL),
            fill = 'grey95', alpha = 0.1)+
  geom_rect(aes(NULL,NULL,
                xmin = as.POSIXct("2020-02-22 17:56:00"), xmax = as.POSIXct("2020-02-23 05:55:00"),
                ymin = 0, ymax = +Inf,
                colour = NULL),
            fill = 'grey95', alpha = 0.1)+
  geom_line(aes(y = Value, colour= Interaction))+
  geom_ribbon(aes(ymin = Value-ci, ymax = Value+ci), fill = "grey70", alpha=0.5) +
  scale_x_datetime(breaks=seq(as.POSIXct("2020-02-20 18:00:00"),
                              as.POSIXct("2020-02-23 18:00:00"), "6 hours"), date_labels = "%H")+
  ylab("Activité moyenne (nb passage/10min)")+
  xlab("Temps (Heure)")+
  ggtitle("")+
  scale_colour_manual(values=cols, name="Légende",
                      labels=c("R1_5%", "R1_0.5%"))+
  facet_grid(Interaction ~ .)

p1
ggsave(plot=p1, filename="./img/MeanofactperR+F_R1.pdf")

p2 <- ggplot(data=act2, aes(x=Time, y=Value))+
  geom_rect(aes(NULL,NULL,
                xmin = as.POSIXct("2020-03-05 18:36:00"), xmax = as.POSIXct("2020-03-06 06:35:00"),
                ymin = 0, ymax = +Inf,
                colour = NULL),
            fill = 'grey95', alpha = 0.1)+
  geom_rect(aes(NULL,NULL,
                xmin = as.POSIXct("2020-03-06 18:36:00"), xmax = as.POSIXct("2020-03-07 06:35:00"),
                ymin = 0, ymax = +Inf,
                colour = NULL),
            fill = 'grey95', alpha = 0.1)+
  geom_rect(aes(NULL,NULL,
                xmin = as.POSIXct("2020-03-07 18:36:00"), xmax = as.POSIXct("2020-03-08 06:35:00"),
                ymin = 0, ymax = +Inf,
                colour = NULL),
            fill = 'grey95', alpha = 0.1)+
  geom_line(aes(y = Value, colour= Interaction))+
  geom_ribbon(aes(ymin = Value-ci, ymax = Value+ci), fill = "grey70", alpha=0.7) +
  scale_x_datetime(breaks=seq(as.POSIXct("2020-03-05 18:00:00"),
                              as.POSIXct("2020-03-08 18:00:00"), "6 hours"), date_labels = "%H")+
  ylab("Activité moyenne (nb passage/10min)")+
  xlab("Temps (Heure)")+
  ggtitle("")+
  scale_colour_manual(values=cols, name="Légende",
                      labels=c("R2_5%", "R2_0.5%"))+
facet_grid(Interaction ~ .)
p2

ggsave(plot=p2, filename="./img/MeanofactperR+F_R2.pdf")

#scale_color_manual(values = cols) choix des couleurs 
##ameliorer les legendes 

# Graph tous les replicats sur le même graphique avec IC 
p2 <- ggplot(data=act1, aes(x=Time, y=Value))+
  geom_line(aes(y = Value, colour= Interaction))+
  ylab("Mean of activity")+
  ggtitle("Mean of activity")+ 
 scale_colour_manual(values=cols, name="Légende",
                  labels=c("R1_5%", "R2_5%", "R1_0.5", "R2_0.5%"))

p2  
ggsave(plot=p2, filename="./img/Meantot2.pdf")


## Creation variable jour/nuit pour faire les analyses sur 12h.

# exemples 
mutate(flights,
       type_retard = case_when(
         dep_delay > 0 & arr_delay > 0 ~ "Retard départ et arrivée",
         dep_delay > 0 & arr_delay <= 0 ~ "Retard départ",
         dep_delay <= 0 & arr_delay > 0 ~ "Retard arrivée",
         TRUE ~ "Aucun retard"))


flights %>% 
  group_by(carrier) %>% 
  mutate(median_delay = median(dep_delay, na.rm = TRUE),
         delay_carrier = if_else(dep_delay > median_delay,
                                 "Supérieur", "Inférieur ou égal"))
## ajout d'une colonne au tableau initial 
# creation d'un vecteur qui permet d'avoir le creneau horaire qui nous interesse.
x <- c(18:23)
x2 <- c(0: 6)
xt <- c(x,x2)

# ajout de la colonne 

data10 <- mutate(data10, Jour_nuit = if_else(Hour%in%xt,
                                           "Nuit", "Jour"))
# Calcul moyenne par jour/nuit en fonction regime et réplicat et jour 

act2 <- data10 %>%
  group_by(Replicat, Food, Day, Jour_nuit) %>%
  transmute(mean12 = mean(Value, na.rm=TRUE)) %>% 
  distinct()

act2$Interaction <- with(act2, interaction(Replicat, Food))

# OU
act2b <- summarySE(data10, measurevar="Value", groupvars=c("Replicat","Food", "Day", "Jour_nuit"),na.rm=T)
act2b$Interaction <- with(act2b, interaction(Replicat, Food))


p3 <- ggplot(data=act2b, aes(x=Day, y=Value, group=Interaction, colour=Interaction))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci), width=.2)+
  ylab("Moyenne des activités (moyenne+95%IC)")+
  xlab("Jour")+
  ggtitle("Moyenne des activité par jour")+
  facet_grid(Jour_nuit ~ .)+
  scale_colour_manual(values=cols, name="Légende",
                      labels=c("R1_5%", "R2_5%", "R1_0.5", "R2_0.5%"))

p3 
ggsave(plot=p3, filename="./img/MeanDay.pdf")





