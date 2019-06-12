## Demonstrando o Teorema do Limite Central

runif(100,-10,10)
mean(runif(10,-10,10))
hist(runif(100,-10,10))

mean(runif(1000,-10,10))
hist(runif(1000,-10,10))

mean(runif(10000,-10,10))
hist(runif(10000,-10,10))

sort.unif <- function(quantos, repete, min, max) {
  resultado <- vector(length = repete) # cria vetor vazio
  for(i in 1:repete) {
    sorteio <- runif(quantos, min, max)
    resultado[i] <- mean(sorteio)
  }
  hist(resultado)
}

sort.unif(100, 100, -10, 10)



normais <- rnorm(100, 2, 4)
mean(normais)
sd(normais)
hist(normais)

n <- 100
x <- rnorm(n)
mean(x)
sd(x)

# Calculando o Intervalo de Confiança a média

qt(0.975, n-1)
mean(x)
sd(x)
se <- sd(x)/sqrt(n)
CI <- qt(0.975, n-1) * se
mean(x) - CI
mean(x) + CI

sort.norm <- function(quantos, repete, media, variancia) {
  resultado <- vector(length = repete)
  for(i in 1:repete) {
    sorteio <- rnorm(quantos, media, variancia)
    resultado[i] <- mean(sorteio)
  }
  hist(resultado)
}
sort.norm(100, 100, 0, 1)

hist(x,freq=F,ylim=c(0,0.5))
curve(dnorm(x), add=T)
dnorm(x)
