## Base de dados sobre riqueza de esp?cies e mam?feros em ilhas (script baseado no do Nicholas)

ilhas <- read.table(file = "ilhas.txt", header = TRUE)
head(ilhas)

attach(ilhas)

qqnorm(riqueza)
qqline(riqueza)

qqnorm(log(riqueza))
qqline(log(riqueza))


library(fitdistrplus)

descdist(data = dado$riqueza, boot=1000)

summary(glm(log(riqueza) ~ log(area) + ilha, dados, family = gaussian()))

summary(glm(log(riqueza) ~ 0 + log(area) + ilha, dados, family = gaussian()))

modelo4 <- glm(log(riqueza) ~ log(area) + ilha, dados, family = gaussian())

model.matrix(modelo4)


library(MuMIn)
options(na.action = "na.fail") 

full <- glm (log(riqueza)~ ilha + arquipelago + log(area) + log(produtividade) + 
               populacao + habitat + montanha + temperatura + 
               precipitacao, data = dados, family = gaussian() )
selecao <- dredge(full)

head(model.sel(selecao))

model80 <- glm(log(riqueza) ~ ilha + arquipelago + log(area) + 
                 populacao + habitat, data = dados, family = gaussian())

library(lme4)

modelo1 <- lmer(log(riqueza) ~ ilha + log(area) + (1|arquipelago), data= ilhas, REML=FALSE)
modelo1

modelo2 <- lmer(log(riqueza) ~ ilha + log(area) + (1|arquipelago), data= ilhas, REML=TRUE)
modelo2

modelo3 <- glmer(riqueza ~ ilha + log(area) + (1|arquipelago), data= ilhas, family = poisson())

modelo5 <- lmer(log(riqueza) ~ (1|ilha) + log(area) + (1|arquipelago), data= ilhas)

modelo6 <- glmer(riqueza ~ (1|ilha) + log(area) + (1|arquipelago), data= ilhas, family=poisson())

# USANDO O CONJUNTO DE MEDIDAS FEITAS NAS ILHAS, ELABORE DUAS OUTRAS HIP?TESES ALTERNATIVAS 
# ? ?REA PARA EXPLICAR A RIQUEZA DE ESP?CIES.

# FORMULE ESTAS HIP?TESES COMO MODELOS E COMPARE O SUPORTE DE CADA MODELO USANDO AIC

# QUAL OU QUAIS MODELOS T?M MAIOR APOIO?

