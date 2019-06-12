## Dados do Cap. 10 de Gotelli & Ellison

# Criando um objeto tipo matriz com os dados de floração nos três tratamentos
floracao <- matrix(c(10, 12, 12, 13, 9, 11, 11, 12, 12, 13,15, 16), 4,3)   
floracao
colnames(floracao) <- c("sem.manip", "controle", "tratamento")
floracao

# Criando um 'dataframe' com os dados de floração nos três tratamentos
flora <- c(10, 12, 12, 13, 9, 11, 11, 12, 12, 13,15, 16)
obs <- c(1:12)
fator <- rep(c("sem.manip", "control", "trat"), each=4)
floracao2 <- data.frame(obs, fator, flora)
floracao2

# Função para calcular uma estatística sobre a floração - a média no caso - por tratamento
tapply(flora, fator, mean)

floracao2
flora
mean(flora)


## Faça uma ANOVA testando a hipótese nula de médias iguais entre tratamentos
## são as contas da Tab. 10.1, 10.2 do Gotelli & Ellison
## lembre que deve usar pf(valor_de_F, gl_numerador, gl_denominador) para encontrar 
## a probabilidade de razões tão altas ou maiores que a encontrada, SEGUNDO A HIPÓTESE NULA




