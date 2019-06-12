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

# # Menor Diferença Significativa (Least Square Difference) como barras de erro
# mds <- qt(0.975,10)*2*ep    # valor crítico de t transformado para bicaudal (*2) e unidades originais (*ep.dif) 
# barras.mds <- rep(mds,5)/2
# barplot(alturas,col="green",ylim=c(0,700),
#         ylab="Biomassa media",xlab="Tratamento de competicao")
# barras.erro(alturas,barras.mds)


# Experimentos fatoriais

weights <- read.csv("growth.csv")
attach(weights)

barplot(tapply(gain,list(diet,supplement),mean),beside=T)

labels <- levels(diet)
shade <- c(0.2,0.6,0.9)

barplot(tapply(gain,list(diet,supplement),mean),beside=T,
        ylab="Ganho de peso",xlab="Suplemento",ylim=c(0,30))

legend(locator(1),labels, gray(shade))

tapply(gain,list(diet,supplement),mean)
model <- aov(gain~diet*supplement)
summary(model)
tapply(gain,list(diet,supplement),length)

x <- as.vector(barplot(tapply(gain,list(diet,supplement),mean),
                       beside=T,ylim=c(0,30)))
y <- as.vector(tapply(gain,list(diet,supplement),mean))
z <- rep(0.656,length(x))
for( i in 1:length(x) ) 
  arrows(x[i],y[i]-z[i],x[i],y[i]+z[i],length=0.05,code=3,angle=90)

legend(locator(1),labels,gray(shade))

model <- lm(gain~diet+supplement)
summary(model)

supp2 <- factor(supplement)
levels(supp2)

model2 <- lm(gain~diet+supp2)
anova(model,model2)

# Experimentos de plots subdivididos (split plot)

yields <- read.csv("splityield.csv")
attach(yields)
names(yields)

model <- aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density))
summary(model)

interaction.plot(fertilizer,irrigation,yield)
interaction.plot(density,irrigation,yield)


# Efeitos aleatorios e pseudoreplicacao

rats <- read.csv("rats.csv")
attach(rats)
names(rats)

Treatment <- factor(Treatment)
Rat <- factor(Rat)
Liver <- factor(Liver)

# Forma ERRADA de fazer

model <- aov(Glycogen~Treatment)
summary(model)

# Forma CERTA

yv <- tapply(Glycogen,list(Treatment,Rat),mean)
( yv <- as.vector(yv) )

treatment <- factor(c(1,2,3,1,2,3))
model <- aov(yv~treatment)
summary(model)


# Análise de Componentes da Variancia (decompondo a variancia)

model2 <- aov(Glycogen~Treatment+Error(Treatment/Rat/Liver))
summary(model2)



