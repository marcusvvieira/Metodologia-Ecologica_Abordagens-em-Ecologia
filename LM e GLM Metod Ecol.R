## Script elaborado por Nicholas Marino (v.março 2017), modificado por Marcus

dados <- read.table("ilhas.txt", header = T)

qqnorm(dados$riqueza); qqline(dados$riqueza)


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

modelo6 <- lmer(log(riqueza) ~ ilha + log(area) + (1|arquipelago), 
                data= dados, REML=FALSE)

modelo7 <- lmer(log(riqueza) ~ ilha + log(area) + (1|arquipelago), 
                data= dados, REML=TRUE)

modelo8 <- glmer(riqueza ~ ilha + log(area) + (1|arquipelago), 
                 data= dados, family = poisson(), REML=TRUE)



# USANDO O CONJUNTO DE MEDIDAS FEITAS NAS ILHAS, ELABORE DUAS OUTRAS HIPÓTESES ALTERNATIVAS 
# À ÁREA PARA EXPLICAR A RIQUEZA DE ESPÉCIES.

# FORMULE ESTAS HIPÓTESES COMO MODELOS E COMPARE O SUPORTE DE CADA MODELO USANDO AIC

# QUAL OU QUAIS MODELOS TÊM MAIOR APOIO?
