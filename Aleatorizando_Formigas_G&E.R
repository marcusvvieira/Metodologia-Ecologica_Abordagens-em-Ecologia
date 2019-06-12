# Dados do Cap. 3 Gotelli & Ellison, contagens hipotéticas de formigueiros de uma espécie formiga
# na floresta e no campo

formigas <- c(9, 6, 4, 6, 7, 10, 12, 9, 12, 10)

habitat <- c(rep("floresta", 6), rep("campo", 4))

tabela <- data.frame(habitat, formigas)

summary(lm(formigas ~ habitat))

# Já que foi construída uma tabela, podemos usar 'tapply' para obter estatísticas de cada linha x coluna
tapply(formigas, habitat, mean)

# Como fazer a média apenas das linhas com formigas da floresta, depois campo.
mean(tabela$formigas[tabela$habitat=="floresta"])
mean(formigas[habitat=="floresta"])

mean(formigas[habitat=="campo"])

# Diferença observada entre as médias da floresta e campo
obs.dif <-abs(mean(formigas[habitat=="floresta"]) - mean(formigas[habitat=="campo"]))

obs.dif

# Guardando em rdn.sample uma amostra de 10 contagens sorteadas de 'formigas' SEM REPOSIÇÃO -> Aleatorização
rdn.sample <- as.matrix(sample(formigas, 10, rep = F))

rdn.sample

# Selecionando as seis primeiras contagens sorteadas
rdn.sample[1:6]

# Selecionando as quatro últimas contagens sorteadas
rdn.sample[7:10]

# Calculando a diferença entre médias de valores sorteados agora
abs(mean(rdn.sample[1:6]) - mean(rdn.sample[7:10]))

# Vamos repetir o mesmo procedimento 1000 vezes em um 'loop'
rdn.dif <- numeric(1000); rdn.dif
for(i in 1:1000) {
  rdn.sample <- as.matrix(sample(formigas, 10, rep = FALSE))
  rdn.dif[i] <- abs(mean(rdn.sample[1:6]) - mean(rdn.sample[7:10]))
}

# Histograma de frequencia de valores de diferença entre médias obtidas por sorteio.
hist(rdn.dif)

# Qual a posição no vetor rdn.dif de contagens sorteadas >= à diferença observada
which(rdn.dif>=obs.dif)

# Agora encontrando o valor das contagens nestas posições
rdn.dif[which(rdn.dif>=obs.dif)]

# Tamanho do vetor como N
length(rdn.dif[which(rdn.dif>=obs.dif)])

# Dividindo no. de rdn.dif >= obs.dif pelo N de sorteios
length(rdn.dif[which(rdn.dif>=obs.dif)]) / length(rdn.dif)

# Ordenando o vetor rdn.dif para poder encontrar o valor crítico, que delimita 95% das diferenças 
sort.rdn.dif <- sort(rdn.dif)

# Qual o valor de rdn.dif na posição equivalente da 95%
sort.rdn.dif[950]

# Os mesmos passos automatizados com a função 'quantile'
quantile(rdn.dif, 0.95)

abline(v = obs.dif, lty = 2, col = 2)
abline(v = quantile(rdn.dif, 0.95), lty = 2, col = 3)

