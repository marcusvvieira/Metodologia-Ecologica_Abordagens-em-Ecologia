## Relação entre valor de t, distribuição normal e graus de liberdade

# Plotando probabilidades esperadas segundo uma distribuição normal padrão entre -4 e 4
xvs<-seq(-4,4,0.01)
plot(xvs,dnorm(xvs),type="l",lty=2,ylab="P(x) (densidade de probabilidade)",xlab="x (desvios padr?o)")
# Adicionando valores esperados se fosse uma distribuição t com 5 graus de liberdade
lines(xvs,dt(xvs,df=5))

# Relação entre graus de liberdade e valor de t
plot(c(0,30),c(0,10),type="n",xlab="Graus de liberdade",ylab="Valor t de Student")
lines(1:30,qt(0.975,df=1:30))

## QUAL O VALOR DE t APROXIMADO DEPOIS DE GL > 30? 

# Dados para o teste t
t.test.data <- read.table("t.test.data.txt", header=T)
attach(t.test.data)
names(t.test.data)

ozone<-c(gardenA,gardenB)
label<-factor(c(rep("A",10),rep("B",10)))
boxplot(ozone~label,notch=T,xlab="Jardim",ylab="Ozonio")

# Variâncias
s2A<-var(gardenA); s2A
s2B<-var(gardenB); s2B

# Valor de t para a diferença
(mean(gardenA)-mean(gardenB))/sqrt(s2A/10+s2B/10)

    # OU

(mean(gardenA)-mean(gardenB))/sqrt((s2A+s2B)/10)

# Probabilidade deste valor de t ou mais extremo
2*pt(-3.872983,18)


# Função do R para teste t de duas amostras
t.test(gardenA, gardenB)


## TESTANDO DIFERENÇA DE VARIANCIAS COM A RAZÃO F
f.test.data<-read.table("f.test.data.txt",header=T)
attach(f.test.data)
names(f.test.data)

# Variâncias são diferentes
var(gardenB)
var(gardenC)
# Médias são iguais!!
mean(gardenB)
mean(gardenC)

F.ratio1 <-var(gardenC)/var(gardenB)
F.ratio1

# Probabilidade da razão F obtida segundo Ho 
# ("bicaudal": a hipótese alternativa não prevê qual variância deve ser maior, se jardimC ou jardimB)

2*(1-pf(F.ratio1,9,9))

# OU

F.ratio2 <- var(gardenB)/var(gardenC)
F.ratio2
(pf(F.ratio2,9,9))

(1-pf(F.ratio1,9,9)) + (pf(F.ratio2,9,9))  
# Probabilidade de valores iguais ou menores que JardimB/JardimC (Cauda à esquerda de F.ratio2) +
# + Prob. valores iguais ou maiores que JardimC/JardimB (Cauda à direita de F.ratio1)

# Função do R para um teste de razão F simples (não é uma "Análise de Variância" ainda!)
var.test(gardenC,gardenB)

