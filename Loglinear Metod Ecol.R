# http://data.library.virginia.edu/an-introduction-to-loglinear-models/

# Exercício 1: Teste X2 e teste exato de Fisher para os dados de herbivoria em Cecropia da aula 1
## criar objeto 'Cecropia', uma matriz com duas colunas, 'nc=2'
Cecropia <- matrix(c(72,34, 12,14), nc=2)
Cecropia <- matrix(c(1449,500, 46, 281), nc=2)

rownames(Cecropia) <- c("ausente", "presente") 
colnames(Cecropia) <- c("com.formigas", "sem.formigas")
Cecropia

chisq.test(Cecropia, correct=TRUE)

fisher.test(Cecropia)

loglin(Cecropia, list(c(1,2)))

árvores <- array(data = c(911, 44, 538, 456, 3, 2, 43, 279), 
                 dim = c(2,2,2), 
                 dimnames = list("herbivoria" = c("não","sim"),
                                 "local" = c("interior","borda"),
                                 "formigas" = c("presente","ausente")))
árvores

ftable(árvores, row.vars = c("formigas","herbivoria"))

addmargins(árvores)

prop.table(árvores, margin = c(1,3))

prop.table(árvores, margin = c(2,3))

loglin(árvores, list(c(1, 2), c(1, 3), c(2, 3)))

## EXEMPLO DE COMO CALCULAR O VALOR ESPERADO DE CADA CELULA E RELAÇÃO COMO MODELO LOG-LINEAR

set.seed(1)
qtr <- sample(c("Cara","Coroa"),40,replace = T)
nik <- sample(c("Cara","Coroa"),40,replace = T)
table(qtr,nik)

## 0.5 * 0.5 = 0.25. Para o valor esperado 40 * 0.25 = 10. Qual foi o valor observado?

##  Esperao ij = n * Probabilidade i * Probabilidade j (40 * 0.5 * 0.5)

## log(Esperado) ij = log(n) + log(Prob. i) + log(Prob. j)

## APESAR DOS DADOS TEREM SIDO OBTIDOS POR UM PROCESSO DE AMOSTRAGEM ALEATÓRIO, 
## com Prob. Cara = Coroa = 0.5, OBSERVADO E ESPERADO NÃO SÃO IGUAIS. 
## QUAL A PROBABILIDADE DA HIPÓTESE NULA?

