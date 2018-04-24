moeda<-c("cara","coroa")
amostra<-sample(moeda,100,replace=TRUE)
(amostra[amostra=="coroa"]) # quais as jogadas do sorteio que deram coroa? Utilizamos == para especificar igualdade
(amostra[amostra!="coroa"]) # quais as jogadas do sorteio que foram diferentes de coroa? Utilizamos !=   para designar desigualdade
length(amostra[amostra!="coroa"]) # comece a pensar em como fazer as coisas de maneira mais elegante e funcional
# Exercite jogando dados

table(amostra)

barplot(table(amostra))


dados <- c(1:6)

dados <- c(1:6)
dados
jogos <- sample(dados, 100, replace = TRUE)
jogos
table(jogos)
jogos
barplot(table(jogos))
jogos <- sample(dados, 100, replace = TRUE)
barplot(table(jogos))

x<-rnorm(100)
sort(x)
order(x) 

x <- c(1:12)
xmat <- matrix(x, ncol=4)
xmat
matrix(x,nrow=3,byrow=T)

rownames(xmat)<-c("lagos","rios","lagoas")
xmat
colnames(xmat)<-c("RJ","SP","SC","RS")
xmat

xmat<-cbind(xmat,c(13,14,15))
colnames(xmat)<-c("RJ","SP","SC","RS","ES")
xmat

# para adicionar linhas utilize rbind(x,y). Podemos criar matrizes rapidamente
rbind(c(1,2,3),c(4,5,6)) # o mesmo vale para cbind

y<-c(13:24)
ymat<-matrix(y,ncol=4)
ymat
colnames(ymat)<-dimnames(xmat)[[2]][1:4]
ymat

som.col<-apply(xmat,2,sum) # o n?mero dois especifica que a soma ser? calculada por colunas. 

a<-c(1:5)
b<-c(1:3)
lista<-list(a,b) # agora tente acessar cada elemento da lista utilizando duplo colchetes

x<-list(a=rnorm(10), b=rnorm(10))
sapply (x,mean)

x<-data.frame("a"=rnorm(10), "b"=rnorm(10))
lapply(x,mean)

media<-function(x){
  sum<-sum(x)
  n<-length(x)
  valor<-sum/n
  return(valor) # volta o valor do c?lculo
}

sorteio <- function(n.jogadas, repete) {
  resultado <- vector(length = repete)
  for(i in 1:repete) {
    jogada <- sample(moeda, n.jogadas, replace = TRUE)
    resultado[i] <- length(jogada[jogada=="coroa"])
  }
  hist(resultado)
  }