Galapagos2 <- read.txt("Galapagos2.txt, header=T")
attach(Galapagos2)
Galapagos2
plot(Area,Nspecies)

head(Galapagos2)

plot(LogArea, LogIdade)

model.area <- glm(log(Nspecies)~log(Area))
summary(model.area)

plot(LogIdade, LogSpecies)

plot(Idade, Area)


model.idade <- glm(log(Nspecies)~log(Idade))
summary(model.idade)

model.2 <- lm(log(Nspecies)~log(Idade)+log(Area))
summary(model.2)
