# Anova de um fator (oneway)

oneway <- read.csv("oneway.corrigido.csv")  # Concentração de ozônio (ppm) em dois jardins de plantios de alface
attach(oneway)
names(oneway)

# Plotando os valores de ozônio apenas em ordem de linhas 
plot(1:20,ozone,ylim=c(0,8),ylab="y",xlab="order",pch=21,bg="red")

# Adicionando uma linha no valor médio 
abline(h=mean(ozone),col="blue")
for(i in 1:20) lines(c(i,i),c(mean(ozone),ozone[i]),col="green")   # e linhas verticais até cada ponto

# Agora um plot com cores diferentes para cada jardim, adicionando depois a linha no valor médio de cada um
plot(1:20,ozone,ylim=c(0,8),ylab="y",xlab="ordem",
     pch=21,bg=as.numeric(garden))
abline(h=mean(ozone[garden=="A"]))
abline(h=mean(ozone[garden=="B"]),col="red")

# Linhas verticais de cada ponto à linha de média do seu jardim
index <- 1:length(ozone)
for (i in 1:length(index)){
  if (garden[i] == "A" )
    lines(c(index[i],index[i]),c(mean(ozone[garden=="A"]),ozone[i]))
  else 
    lines(c(index[i],index[i]),c(mean(ozone[garden=="B"]),ozone[i]), col="red")
}

# Soma dos Quadrados de Y (Sum of Squares of Y, SSY) 
SQY <- sum((ozone-mean(ozone))^2)
SQY

# Soma dos Quadrados Residual, dentro de cada jardim (Residual Sum of Squares, RSS) ou intra (within)
SQA.dentro <- sum((ozone[garden=="A"]-mean(ozone[garden=="A"]))^2)
SQB.dentro <- sum((ozone[garden=="B"]-mean(ozone[garden=="B"]))^2)
SQResid <- SQA.dentro + SQB.dentro
SQResid

# Soma dos Quadrados devido ao tratamento, ou 'entre' (between)
SQTrat <- SQY - SQResid
SQTrat

# Valor crítico de razão F para rejeitar a hipótese nula
qf(0.95,1,18)
1-pf(15.0,1,18)

# Fazendo a mesma coisa com a função 'aov'  
summary(aov(ozone~garden))

1-pf(34.1, 1, 18)

# Gráficos diagnósticos 
plot(aov(ozone~garden))

# plots for anova

comp <- read.csv("competition.csv")
attach(comp)
names(comp)

plot(clipping,biomass,xlab="Tratamento de competicao",
     ylab="Biomassa",col="lightgrey")

alturas <- tapply(biomass,clipping,mean)
barplot(alturas,col="green",ylim=c(0,700),
        ylab="Biomassa media",xlab="Tratamento de competicao")


# Barras de erro

barras.erro <- function(y,z) {
  x <- barplot(y,plot=F)
  n <- length(y)
  for (i in 1:n) 
    arrows(x[i],y[i]-z,x[i],y[i]+z,code=3,angle=90,length=0.15)
}

model <- aov(biomass~clipping)
summary(model)
table(clipping)

# Erro Padrão das diferenças entre como barras de erro (pooled standard error)
# (a variância residual é usada neste caso, isto é, ep = raiz(Soma dos Quadrados Residual/N médio de cada tratamento)

ep.dif <- sqrt(4961/6)
ep <- rep(ep.dif,5)
barplot(alturas,col="green",ylim=c(0,700),
        ylab="Biomassa media",xlab="Tratamento de competicao")
barras.erro(alturas,ep)

# Intervalo de Confiança como barras de erro
ic <- ep*qt(0.975,5)
barplot(alturas,col="green",ylim=c(0,700),
        ylab="Biomassa media",xlab="Tratamento de competicao")
barras.erro(alturas,ic)
