## Importar os dados do peixe fictício
df <- read.csv("./tamanho_peixe.csv")

## Veja o que temos nesta planilha...
str(df)

## Anexar os dados
attach(df)

###+++++++++++++++++++++++++++++++++##
### PARTE I: ESTIMANDO PARÊMETROS ####
###+++++++++++++++++++++++++++++++++##
## Vamos trabalhar com uma subamostra dos dados (por enquanto!)
amostra <- sort(sample(Tamanho, 5, replace = FALSE))

## Vamos ver com o que estamos trabalhando...
plot(x = c(5, 12), y = c(0,0.6), type = 'n',
     xlab = "Tamanho", ylab = "Densidade de probabilidade")
points(x = amostra, y = rep(-0.02, length(amostra)), pch = 24, col = "black", bg = "black")

## Definir a FUNÇÃO DE DENSIDADE DE PROBABILIDADE (PDF)
normal <- function(media, desv, x){
  1/(desv*(sqrt(2*pi)))*exp(-((x-media)^2)/(2*desv))
}

## Definir hipóteses de distribuição dos dados
h1 <- normal(media = 7, desv = 1, x = amostra) # Hipótese 1 (média 7, sd = 1)
h2 <- normal(media = 10, desv = 1, x = amostra) # Hipótese 2 (média 10, sd = 1)
h3 <- normal(media = 10, desv = 1.5, x = amostra) # Hipótese 3 (média 10, sd = 1.5)

## Vamos incluir hipóteses de distribuição no gráfico
x <- seq(5, 12, length = 200) # este vetor serve apenas para traçar as distribuições no gráfico

# HIPÓTESE 1 (média 7, sd = 1)
lines(x = x, y = normal(media = 7, desv = 1, x = x),
      type = "l", lty = "dashed", lwd = 1.5, col="blue")
points(amostra, h1, pch = 19, col = "blue")

# HIPÓTESE 2 (média 10, sd = 1)
lines(x = x, y = normal(media = 10, desv = 1, x = x),
      type = "l", lty = "dashed", lwd = 1.5, col="green")
points(amostra, h2, pch = 19, col = "green")

# HIPÓTESE 3 (média 10, sd = 1.5)
lines(x = x, y = normal(media = 10, desv = 1.5, x = x),
      type = "l", lty = "dashed", lwd = 1.5, col="red")
points(amostra, h3, pch = 19, col = "red")

## Verossimilhança de cada hipótese é o produto dos valores de densidade 
vero1 <- prod(h1)
vero2 <- prod(h2)
vero3 <- prod(h3)

## Para trabalhar com valores mais agradáveis, vamos tirar o log-negativo estes valores de verossimilhança
vero1 <- log(vero1)
vero2 <- log(vero2)
vero3 <- log(vero3)

## Vamos estimar o valor do parâmetro (neste exemplo, a média) com o qual podemos obter o valor máximo de verossimilhança
vero <- data.frame(media = NA, vero = NA) # crie uma matriz para guardar os resultados

# Rode um loop que vai retornar o valor de verossimilhança para 200 intervalos de média entre 5 e 12
for(i in seq(5, 12, length = 200)){
  h <- normal(media = i, desv = 1, x = amostra)
  vero <- rbind(vero, c(i, -log(prod(h))))
}

# Plote a relação entre o valor de verossimilhança obtido para cada valor de média
plot(vero~media, data = vero, type = "l", col = "blue", xlab = "Média", ylab = "Verossimilhança")

# Veja o valor da média que obtem o valor máximo de verossimilhança
vero[which(vero$vero == max(na.omit(vero$vero))), "media"]

## Esta rotina para estimar o parâmetro está implementada no pacote 'stats4' com a função 'mle'
## Com esta rotina, podemos trabalhar de forma fácil com a estimativa de múltiplos fatores
## Por exemplo... estimar os valores da média E da variância que forneçam a máxima verossimilhança
library(stats4)

# A função 'mle' exige a função para a obter a verossimilhança da hipótese. Neste caso, vamos continuar trabalhando apenas com distribuição normal.
LL <- function(media, desv){
  vero <- normal(media = media, desv = desv, x = amostra)
  sum(log(vero))
}

# O parâmetro 'start' na função 'mle' fornece os valores de partida dos parâmetros para encontrar a máxima verossimilhança. É bom que estes valores de partida sejam próximos dos estimados (principalmente quando estamos estimando vários parâmetros)... pelo menos um palpite!!
mle(LL, start = list(media = 10, desv = 1))



###+++++++++++++++++++++++++++++++++++++++++++++++++##
## PARTE II: RELAÇÕES ESPÚRIAS E MÚLTIPLOS MODELOS ###
###+++++++++++++++++++++++++++++++++++++++++++++++++##
library(corrplot) # pacote para plotar gráfico de correlações

## Será que quanto mais covariáveis para explicar o modelo melhor??
## Vamos criar 10 variáveis ALEATÓRIAS, com 30 amostras de distribuições normais variadas...
variaveis <- data.frame(A = rnorm(30, mean = 1, sd = 1),
                        B = rnorm(30, mean = 50, sd = 2),
                        C = rnorm(30, mean = -30, sd = 0.5),
                        D = rnorm(30, mean = 100, sd = 1),
                        E = rnorm(30, mean = 0, sd = 1.5),
                        F = rnorm(30, mean = -1, sd = 0.5),
                        G = rnorm(30, mean = 240, sd = 3),
                        H = rnorm(30, mean = 1000, sd = 2),
                        I = rnorm(30, mean = 1, sd = 1),
                        J = rnorm(30, mean = 0, sd = 1))

## Medir a correlação entre cada variável e plotar num gráfico
corr <- cor(variaveis) ; corrplot(corr, method = "number", type = "lower")

## Pegue o par de variáveis com maior correlação (valor absoluto) e confira se a correlação é significativa...
x <- variaveis$B ; y <- variaveis$H
cor.test(x, y) # teste de correlação

## ... plote e veja!
xy <- lm(x ~ y) # ajustar um modelo linear
plot(x ~ y); abline(xy, col = "red")


## Voltando ao exemplo do peixe...
## Hipóteses para explicar o tamanho corporal
h1 <- lm(Tamanho ~ Temp)
h2 <- lm(Tamanho ~ Idade)
h3 <- lm(Tamanho ~ Temp + Idade)
h4 <- lm(Tamanho ~ Temp * Idade)

## Log-Verossimilhança
logLik(h1)
logLik(h2)
logLik(h3)
logLik(h4)

###+++++++++++++++++++++++++++++++##
## PARTE III: SELEÇÃO DE MODELOS ###
###+++++++++++++++++++++++++++++++##
library(MuMIn)

model.sel(h1, h2, h3, h4)
