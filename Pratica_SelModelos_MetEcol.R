## Lendo o conjundo da dados sobre área de ilhas e riqueza de espécies
Galapagos2 <- read.table("Galapagos2.txt", header=T)
attach(Galapagos2)
head(Galapagos2)
plot(Area,Nspecies)

head(Galapagos2) # primeiras cinco linas de cada coluna do arquivo

plot(LogArea, LogSpecies)

# Modelo com Área da ilha como variável preditiva
model.area <- lm(LogSpecies~LogArea) # Usando GLM para obter verossimilhança e AIC
summary(model.area)

plot(log(Idade), log(Nspecies))

plot(Idade, Area)

# Modelo com Idade como variável preditiva
model.idade <- lm(LogSpecies~LogIdade)
summary(model.idade)

# Modelo com as duas variáveis preditivas
model.3 <- lm(LogSpecies~LogIdade+LogArea)
summary(model.3)

# Modelo nulo: apenas o intercepto e variação residual são estimados
model.null <- lm(LogSpecies~1)

AICcor <- function(model, k, n) {
  -2*logLik(model) + 2*k + 2*k*(k+1)/(n-k-1)
}

AICcor(model.area, 3, 17)


## FAÇA UMA SELEÇÃO ENTRE ESTES MODELOS USANDO AICc, DELTA, PESO DE AIC





## Função para calcular estatisticas e montar tabela de seleção de modelos

m.AICc <- function(modelos,n){ #n é o tamanho amostral  
  LL <- sapply(modelos,logLik)
  k <- sapply(lapply(modelos,logLik),attr,"df")
  AIC <- -2*LL+2*k
  AICc <- AIC+((2*k*(k+1))/(n-k-1))
  d.AICc <- AICc-min(AICc)
  w.AICc <- (exp(-0.5*d.AICc))/sum(exp(-0.5*d.AICc))
  data.frame(n.par=k,log.lik=LL,AICc=AICc,AIC=AIC,delta.AICc=d.AICc,w.AICc=w.AICc, 
             row.names=names(LL))[order(d.AICc),]
}

# Aplicando a função
m.AICc(list(model.area, model.idade, model.3, model.null), nrow(Galapagos2))

## Fazendo a mesma coisa com a biblioteca MuMin

library(MuMIn)

# Construindo a tabela 
global <- glm(LogSpecies~LogIdade+LogArea)
options(na.action = "na.fail") 
AIC.table <- dredge(global); AIC.table

write.csv(AIC.table, "AIC.table.csv") # Salvando o resultado

importance(AIC.table)
write.csv(importance(AIC.table), "importance AIC.csv") # Salvando o resultado


