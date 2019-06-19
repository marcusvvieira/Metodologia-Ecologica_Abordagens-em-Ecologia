# Qui-quadrado como teste de aderencia ('goodness-of-fit') a uma hipotese

# Frequencias observadas
obs <- c(6, 4); obs

# Funcao usada para teste de aderencia igual probabilidade de cada resultado
chisq.test(obs)

chisq.test(obs)$expected

# Agora com probabilidades diferentes para cada resultado
chisq.test(obs, p = c(0.4, 0.6))

chisq.test(obs, p = c(0.4, 0.6))$expected


##  Outro exemplo
obs<-c(10, 7, 10, 6, 14, 8, 11, 11, 12, 11)

# Probabilidade igual de cada resultado
1/10

chisq.test(obs)

chisq.test(obs)$expected

# Probabilidades diferentes esperadas para cada grupo de resultados
expect <-  c(0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.13, 0.13, 0.13, 0.13)

chisq.test(obs, p = expect)

chisq.test(obs, p = expect)$expected



## Terceiro exemplo (Apostila de R Beasley)
frequencias <- c(3,12,17,13,9,3,3)

categorias <- c(0, 1, 2, 3, 4, 5, 6)

# Numero total de nematodeos contados: no. contado em cada categoria + 2 celulas com mais de 6 nematodeos
#  sum(frequencias*categorias) + 2
# Estimativa de Lambda: media artimetica de nematodeos por celula
# lambda <- (sum(frequencias*categorias) + 2) / 60
# lambda é igual a 156

lambda <- 156

# Probabilidade de ocorrer celulas com 0, 1, 2 ... 6 nemátodeos para o lambda calculado
expect <- dpois(categorias, lambda); expect

# Mas a soma das probabilidades de 'expect' não soma 1 porque não inlclui probs. de contegens de 7, 8 e maiores
sum(expect)

# Para incluir estas contagens raras mas altas, maiores que 6, basta subtrair 1 - sum(expect)
# depois adicioar esta diferença à última linha, que passa a ser de fato prob de resultados 
# maiores ou iguais a 6.
maiores <- 1 - sum(expect)

expect[7]
expect[7] = expect[7] + maiores

expect

# Testando se agora as probabilidades somam 1.
sum(expect)


## Exemplo do Gotteli & Ellison: plantas raras em têm dinâmica estável em áreas protegidas, e declinam em áreas não-protegidas?

# Qui-quadrado
(18 - 11.75)^2/11.75 + (8 - 14.25)^2/14.25 + 
  (15 - 21.25)^2/21.5 + (32 - 25.75)^2/25.75

# probabilidae de valores iguais ou mais extremos e qui-quadrado
pchisq(9.399547, 1, lower.tail = F)

# valor cŕitico no quartil de 0.05, valor crítico d x
qchisq(0.05, 1, lower.tail = F)


## Entrando com os mesmos dados como uma matriz e usando 'chisq.test'
M <- matrix(c(18, 15, 8, 32), nrow = 2, dimnames = list(c("Diminui", "Estavel"), c("Nao-protege", "Protege")))

M

# Mesma coisa, mas entrando por linhas
M <- matrix(c(18, 8, 15, 32), nrow = 2, byrow = T, dimnames = list(c("Diminui", "Estavel"), c("Nao-protege", "Protege")))

M

# Se quiser mudar os nomes das linhas
rownames(M) <- c("A", "B")
M

# Ou das colunas
colnames(M) <- c("C", "D")
M


# Teste de indpepência entre frequências, sem correção de Yates
chisq.test(M, 1, correct = F)


## Outro exemplo, formando uma matriz por 'binding'

c_hum <- c(15,20,30,20,15)
c_bio <- c( 8,23,18,34,17)

# 'Binding' os dois vetores empilhando linhas
tab_2x5 <- rbind(c_hum,c_bio)

tab_2x5

chisq.test(tab_2x5)

# 'Binding' os dois vetores, mas agora emparelhando com colunas
tab_5x2 <- cbind(c_hum,c_bio)

tab_5x2

chisq.test(tab_5x2)
