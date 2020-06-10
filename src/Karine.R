# ouverture data 
data10 <- readRDS("./output/data10.rds")# il faudra peut être changer le chemin selon ton ordi. 
data10<-readRDS("data10.rds")

# creation base avec le moyenne directement, je sais pas ce que tu vas utiliser donc dans le doute je met tout. 

library(Rmisc)
act2b <- summarySE(data10, measurevar="Value", groupvars=c("Replicat","Food", "Day", "Jour_nuit"),na.rm=T)
act2b$Interaction <- with(act2b, interaction(Replicat, Food))


