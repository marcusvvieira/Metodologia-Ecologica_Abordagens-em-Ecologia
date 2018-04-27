## Regressão entre LogArea e LogRiqueza passo a passo por estimadores de Quadrados Mínimos


# Importar o arquivo de dados em formato .txt
Galapagos2 <- read.table("Galapagos2.txt", header=T, row.names=1)

## Confira se tudo correu bem
Galapagos2

## Torne as variáveis contidas em Galapagos disponíveis pelo nome
attach(Galapagos2)

## Plote LogArea x LogSpecies
## adicionando títulos para os eixos
plot(LogArea, LogSpecies, xlab="Log Island Area", ylab="Log Number of Species")
abline(lm(LogSpecies ~ LogArea))
abline(h=mean(LogSpecies), lty=2)

# SQ xy:  SOMATORIO (xi - média x) (yi - média)

## Calculando a inclinação como SQxy / SQx
b1 <- var(LogArea, LogSpecies) / var(LogArea)

## O intercepto

# y = b0 + b1 * x

# - b0 = - y + b1 * x

# b0 = y - b1 * x

# mean(LogSpecies) = b0 + b1*mean(LogArea)

b0 <- mean(LogSpecies) - b1*mean(LogArea)

## Conferindo usando o comando lm
lm(LogSpecies~LogArea)

## Criando um objeto para guardar o modelo
model1 <- lm(LogSpecies~LogArea)

## Valores estimados de y (LogSpecies)
predicted <- predict(model1)

segments(LogArea, LogSpecies, LogArea, predicted, col="darkgreen")
segments(LogArea, LogSpecies, LogArea, c(rep(mean(LogSpecies), 30)), lty=2, col="brown")

## quadrados dos desvios devida a regressão
dreg <- (predicted-mean(LogSpecies))^2
dreg 

## Soma dos Quadrados devida a regressão
SQreg <- sum(dreg)
SQreg

## quadrados dos desvios residuais
dresid <- ((LogSpecies-predicted)^2)
dresid

## Soma dos Quadrados residual
SQresid <- sum(dresid)
SQresid

## Soma dos Quadrados total
dtot <- ((LogSpecies-mean(LogSpecies))^2)
dtot

SQtot <- sum(dtot)
SQtot

## Demonstração que a SQ total é a soma das outras duas
SQresid + SQreg
SQtot

## Erro padrão da inclinação, SEb1: raiz(QMresid/SQLogArea) 
QMresid <- SQresid/(length(dresid)-2)
QMresid
SEB1 <- sqrt(QMresid/(var(LogArea)*(length(LogArea)-1)))
SEB1

## Erro padrão do intercepto, SEb0: SEB1 * (1/n + (média(LogArea)^2 / SQLogArea))
SEB0 <- sqrt(QMresid)*sqrt(1/length(LogArea) + mean(LogArea)^2 / (var(LogArea)*(length(LogArea)-1)))
SEB0

summary(model1)

## Valor de t da inclinação (b1):
b1/SEB1

## Valor de t do intercepto (b0):
b0/SEB0

## Intervalo de confiança da inclinação
b1 + qt(0.025, 15)*SEB1
b1 + qt(0.975, 15)*SEB1

## Intervalo de confiança do intercepto
b0 + qt(0.025, 15)*SEB0
b0 + qt(0.975, 15)*SEB0


## Calcular a razão F e sua probabilidade segundo Ho (F = 1)
F <- SQreg/QMresid; F
pf(F,1,15, lower.tail=FALSE)

## Probabilidade valor de b1 observado segundo Ho: b1=0
2*(pt(b1/SEB1,15,lower.tail=FALSE))

## Probabilidade valor de b0 observado segundo Ho: b0=0
2*(pt(b0/SEB0, 15, lower.tail=FALSE))

## Geração de 10000 valores de F por aleatorização
random.F <- NULL
for(i in 1:10000) {
  rLogSp <- sample(LogSpecies, replace = T)
  b1 <- var(LogArea, rLogSp) / var(LogArea)
  b0 <- mean(rLogSp) - b1*mean(LogArea)
  predicted <- (b0 + b1*LogArea)
  MSQreg <- sum((predicted-mean(LogSpecies))^2)/1
  MSQresid <- sum((predicted-rLogSp)^2)/(17-2)
  random.F[i] <- MSQreg/(MSQresid)
}

hist(random.F, freq=FALSE)

# sort(round(random.F, 3))

which(random.F >= F)
