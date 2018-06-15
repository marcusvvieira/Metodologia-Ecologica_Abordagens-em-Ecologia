localidade <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
localidade

y <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0)
y

x <- c(2, 3, 5, 4, 10, 15, 7, 9, 3, 1)
x

dados <- data.frame(localidade, y, x)
dados

plot(y ~ x)

model.bin <- glm(y ~ x, family = binomial, data = dados)
model.bin

summary(model.bin)

fitted(model.bin)   # Valores estimados, probabilidade de presença (1) para cada medida de x

y                   # Valores observados de presença/ausência (1/0) para cada medida de x

plot(fitted(model.bin) ~ x)
plot(y ~ x)

novas.obs <- seq(from = 0, to = 15, by = 0.01)    # Novos valores a serem estimadas as probabilidades de presença
novas.obs

newdata <- data.frame("x" = novas.obs)    # Fazendos os novos valores serem a variavel 'x' do modelo (model.bin)

preds <- predict.glm(model.bin, newdata, type="response")    # Valores preditos para os novos valores

plot(y ~ x)
lines(novas.obs, preds, col=2)  
legend(5.5,0.5, "valores preditos", text.col = 2, bty="n")
