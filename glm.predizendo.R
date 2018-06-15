## Base de dados sobre riqueza de esp?cies e mam?feros em ilhas

ilhas <- read.table(file = "ilhas.txt", header = TRUE)
head(ilhas)

# Modelo com vari?vel aleat?ria gaussiana
modelo.gaus <- glm(riqueza ~ log(area), family = gaussian(), data = ilhas)
modelo.gaus
summary(modelo.gaus)

newdat <- data.frame("area"=seq(1,100000))   # Gerando novos dados para predição (áreas de ilhas de 1 a 100000)
newdat

preds.gaus <- predict.glm(modelo.gaus, newdat, type = "response")   # Predições da riqueza baseadas no modelo e suas variáveis (área)


# Mesmo modelo, mas com vari?vel aleat?ria Poisson
modelo.pois <- glm(riqueza ~ log(area), family = poisson(), data = ilhas)
modelo.pois
summary(modelo.pois)

preds.pois <- predict.glm(modelo.pois, newdat, type = "response")

plot(ilhas$area, ilhas$riqueza, log="x")  # Plotando dados observados de riqueza ~ área em escala log
points(preds.gaus, col=2)                 # Adicionando pontos previstos baseados no modelo Gaussiano
points(preds.pois, col=3)                 # Agora pontos previstos no modelo Poisson


# Mesmo modelo com covariáveis
modelo.pois <- glm(riqueza ~ log(area) + ilha + arquipelago, family = poisson(), data = ilhas)
modelo.pois
summary(modelo.pois)

newdat <- data.frame("area"=seq(1,10000), "ilha" = rep("costeira", 10000), "arquipelago" = rep("grande", 10000))
newdat

preds <- predict.glm(modelo.pois, newdat, type = "response")

plot(ilhas$area, ilhas$riqueza, log="x")
points(preds, col = 3)


