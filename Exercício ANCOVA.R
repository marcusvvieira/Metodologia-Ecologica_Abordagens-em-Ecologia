##
## Exercício feito em aula para comparar mesmos dados como ANCOVA e ANOVA dois fatores 
## (dados de M. Crawley), Introduction to Statistics with R

fotoperiodo<-read.table("fotoperiodo.txt",header=T)
attach(fotoperiodo)
names(fotoperiodo)

# Fazendo como uma ANCOVA, sendo Genótipo o fator, Fotoperíodo a covariável
plot(Genotipo, Crescimento)
aov(Crescimento ~ Genotipo + Foto)
model <-lm(Crescimento ~ Genotipo + Foto)
summary.aov(model)

# Comparando o resultado como um modelo linear
summary(model)

## Agora tratando o Fotoperíodo como uma variável categórica, usando a função 'as.factor'
model.factor <- lm(Crescimento ~ Genotipo + as.factor(Foto))
summary.aov(model.factor)

# Comparando com o resultado como um modelo linear
summary(model.factor)

# Compare as tabelas de ANOVA e ANCOVA, que valores mudam de uma para outra?

# Qual resulta em maiores razões de F e o que muda que gera o maior valor do Quadrado Médio (Mean Square)?


## Particionando a variância: fazendo as contas
## obs. Olhe novamente a tabela de ANOVA de dois fatores do Gotelli & Ellison
## e a seção sobre cálculos de partição de variância, ou veja os slides da aula.

summary.aov(model)
var.Entre.Genotipo <- 5.575
var.Entre.asfactorFoto <- 7.058
var.Dentro <- 0.276
a <- 6
b <- 1
n <- 24

var.Genotipo <- ((var.Entre.Genotipo - var.Dentro)*(a-1)) / n
var.asfactorFoto <- ((var.Entre.asfactorFoto - var.Dentro)*(b-1)) / n

(var.Genotipo / var(Crescimento))
(var.asfactorFoto/var(Crescimento))

var.total <- var.Genotipo + var.asfactorFoto + var.Dentro



summary.aov(model.factor)
var.Entre.Genotipo <- 5.575
var.Entre.asfactorFoto <- 2.375
var.Dentro <- 0.308
a <- 6
b <- 4

var.Genotipo <- ((var.Entre.Genotipo - var.Dentro)*(a-1)) / a*b
var.asfactorFoto <- ((var.Entre.asfactorFoto - var.Dentro)*(b-1)) / a*b

(var.Genotipo / var(Crescimento))
(var.asfactorFoto/var(Crescimento))

var.total <- var.Genotipo + var.asfactorFoto + var.Dentro
