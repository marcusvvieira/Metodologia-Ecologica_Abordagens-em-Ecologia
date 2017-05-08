## O arquivo está em formato 'comma separated values", csv, 
## facilmente lido tanto no R como no Excel.
esponjas <- read.table("Esponjas.txt", header = T)
attach(esponjas)
esponjas

## Depois de fazer as contas e criar objetos SQMédia.dentro, 
## SQMédia.entre, F, e P, compare com os resultados da função aov
summary(aov(Crescimento~Tratamento))

## Para fazer o ANOVA como modelo linear no R
lm(Crescimento~Dummy1 + Dummy2 + Dummy3)
summary(lm(Crescimento~Dummy1 + Dummy2 + Dummy3))

## Para as médias de por categoria de Tratamento a função tapply
## aplica uma função a cada categoria, no caso, a média
tapply(Crescimento, Tratamento, mean)

## Boxplot no R
plot(Tratamento, Crescimento)

## função para calcular Tukey HSD
TukeyHSD(aov(Crescimento~Tratamento))

## Para plotar HSDs e Int. Confiança
plot(TukeyHSD(aov(Crescimento~Tratamento)))
