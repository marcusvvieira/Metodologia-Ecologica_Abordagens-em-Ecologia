# Exercício 1: Teste X2 e teste exato de Fisher para os dados de herbivoria em Cecropia da aula 1
## criar objeto 'Cecropia', uma matriz com duas colunas, 'nc=2'
Cecropia <- matrix(c(72,34, 12,14), nc=2)
rownames(Cecropia) <- c("ausente", "presente") 
colnames(Cecropia) <- c("com.formigas", "sem.formigas")
Cecropia

chisq.test(Cecropia, correct=TRUE)

fisher.test(Cecropia)


#### Exercicio 2: analisar a relação entre área de ilhas e riqueza de espécies
## criar dois vetores com os dados usando a função 'c', de concatenar
area <- c(303, 379, 961, 295, 332, 47,  122, 11, 53, 2749)
riqueza <- c(3, 10, 20, 7, 8, 4, 8, 3, 5, 23)
area
riqueza
summary(area)
summary(riqueza)
mean(area)
varea <- var(area)
varea
sqrt(varea)
sd(x=area)
mean(riqueza)
var(riqueza)
sd(riqueza)
plot(area, riqueza, xlab="Área (ha)", ylab="Número de Espécies", ylim=c(0,30))
modelo1 <- lm(riqueza~area)
summary(modelo1)
abline(modelo1)


#### Exercício 3: importar um arquivo de dados das ilhas Galápagos
## e testar relação entre área da ilha e riqueza de espécies

## Importar o arquivo "GalapagosData.txt" do DIRETÓRIO DE TRABALHO
## com os títulos das colunas na 1a. linha e das linhas na 1a. coluna
Galapagos <- read.table("Galapagos.txt", header=T, row.names=1)

## Confira se tudo correu bem
Galapagos

## Torne as variáveis contidas em Galapagos disponíveis pelo nome
attach(Galapagos)

## Vendo os nomes das variáveis no arquivo
names(Galapagos)

## Plote LogArea x LogSpecies
plot(LogArea, LogSpecies)

## adicionando títulos para os eixos
plot(LogArea, LogSpecies, xlab="Log Island Area", ylab="Log Number of Species")

## mudando a cor dos pontos
plot(LogArea, LogSpecies, col="red", xlab="Log Island Area", ylab="Log Number of Species")


## Criando um modelo linear simples para a relação LogSpecies x LogArea
modelo1 <- lm(LogSpecies~LogArea)

# média e desv. padrão dos resíduos de cada observação (=no. espécies em cada ilha)
mean(resid(modelo1))
sd(resid(modelo1))

# sorteando 17 novos valores de residuos de uma dist. normal com mesma média e sd
resid.modelo1 <- rnorm(17, mean(resid(modelo1)), sd(resid(modelo1)))

# valores de riqueza de espécies preditos pela parte determinística do modelo
pred.data1 <- predict(modelo1)

# valores de riqueza de espécies preditos adicionados da incerteza (resíduos)
simul.data <- predict(modelo1) + resid.modelo1

# Plotando dados simulados de riqueza com área da ilha
plot(LogArea, simul.data, xlab="Log Island Area", ylab="Log Species Richness")

# Adicionando os valores observados de riqueza de espécies
points(LogArea, LogSpecies, col="red", bg="blue")

## O que o comando 'predict' faz:
modelo1
pred.data2 <- (1.3198 + 0.3306 * LogArea)
plot(pred.data1, pred.data2)


## Remover objetos antes de sair
rm(Galapagos)

