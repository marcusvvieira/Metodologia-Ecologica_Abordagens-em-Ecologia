##############################################################################
## Distribuição normal, Teorema do Limite Central e distribuição t-Student ###
##############################################################################
# (Baseado no Cap. 5 Crawley Statstics with R) ###############################


### Demonstração do teorema do limite central

hist(runif(10000)*10,main="")  # Criando um vetor com 10000 linhas, "0" em todas

# Gerando um vetor com 10000 médias de 5 valores aleatÃ³rios
medias<-numeric(10000)

for (i in 1:10000){
  medias[i]<- mean(runif(5)*10)
}

# Histograma da distribuição das médias
hist(medias,ylim=c(0,1500))

mean(medias)
sd(medias)

# Sobreposição com curva normal esperada para esta média e desv. padrão (sd)
xv<-seq(0,10,0.1)
yv<-dnorm(xv,mean=5, sd=1.29)*5000
plot(xv,yv, type="l", xlab="cm", ylab="Probabilidade")
lines(xv, yv, type="l")



### Curva NORMAL PADRÃO: média 0 e unidades de sd
nd<-seq(-4,4,0.01)
y<-dnorm(nd)  # para gerar a probabilidade de cada um dos valores em nd (formalmente, a "densidade de probabilidade", dai o "dnorm")
plot (nd,y,type="l", xlab="Z", ylab="Probabilidade")

# OU
mean(nd)
curve(dnorm(x, 0, 1), -4, 4)

# Encontrando as probabilidades cumulativas de valores (em unidades de desvio padrão)
pnorm(-2)
pnorm(-1)
pnorm(-3)

1-pnorm(-3)  # compare com o anterior, pnorm(-3)

# Agora em unidades originais da variável 'medias' que tem média=5, sd=1.29
pnorm(2.5, mean=5, sd=1.29)
pnorm(7.5, mean=5, sd=1.29)


# Agora fazendo o contrário: tendo o quartil ou percentil, e querendo o valor da observação no quartil
qnorm(c(0.025,0.975))   # Dois percentis de uma só vez, os limites inferior e superior do Int. Conf. 95%

abline(v=0, lty=1, col="red", lwd=2)  # lty=2: line type = 2 (pontilhado)

abline(v=qnorm(0.025), lty=2, col="green", lwd=2)

abline(v=qnorm(0.975), lty=2, col="green", lwd=2)


# O que significam estes valores? Que unidades são esssas? 
qnorm(c(0.025,0.975), mean=5, sd=1.29)

# Refazendo o histograma, adicionando linhas verticais nestes pontos
hist(medias,ylim=c(0,1500))

abline(v=mean(medias), lty=1, col="red", lwd=2)  # lty=2: line type = 2 (pontilhado)

abline(v=qnorm(0.025, 5, 1.29), lty=2, col="green", lwd=2)

abline(v=qnorm(0.975, 5, 1.29), lty=2, col="green", lwd=2)


### Curva normal não-padrão 
ht<-seq(150,190,0.01)
plot(ht,dnorm(ht,170,11.55134),type="l",ylab="Probability density",xlab="Height")

# OU: 
curve(dnorm(x, 170, 8), 150, 190)


### Transformando valores observados para z: variável original em unidades de desv. padrão (sd)

mean(ht)
sd(ht)

(160 - mean(ht))/sd(ht)

pnorm((160 - mean(ht))/sd(ht))

(180 - mean(ht))/sd(ht)

pnorm((180 - mean(ht))/sd(ht))


### Probabilidas cumulativas sob curva normal padrÃ£o
pnorm(-0.8657008)
pnorm(0.8657008)
1-pnorm(0.8657008)
pnorm(0.8657008)-pnorm(-0.8657008)

### As mesmas probabilidades sobre a curva normal com media e var da amostra

pnorm(160, mean(ht), sd(ht))
pnorm(180, mean(ht), sd(ht))

qnorm(0.025)
qnorm(0.975)

qnorm(0.025)
qnorm(0.975)

qnorm(0.025, mean(ht), sd(ht))
qnorm(0.975, mean(ht), sd(ht))


### Relação entre valores esperados da pela distribuição normal e t-studentt para os mesmos dados
xvs<-seq(-4,4,0.01)  # gerando uma sequencia de dados
plot(xvs,dnorm(xvs),type="l",lty=2,ylab="Probabilidade",xlab="")
lines(xvs,dt(xvs,df=1))
lines(xvs,dt(xvs,df=2))
lines(xvs,dt(xvs,df=10))
lines(xvs,dt(xvs,df=20))
lines(xvs,dt(xvs,df=1000))
lines(xvs,dt(xvs,df=100))


### Relação entre valor de t e graus de liberdade para o quartil 0.975
plot(c(0,30),c(0,10),type="n",xlab="Degrees of freedom",ylab="Students t value")
lines(1:30,qt(0.975,df=1:30))




