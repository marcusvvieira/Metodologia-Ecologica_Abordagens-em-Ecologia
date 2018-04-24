################## anova #################


# Simulação de dados de crescimento de uma planta em diferentes tipos de solo


#dados
set.seed(2)
are<-rpois(10,5)
are

set.seed(2)
arg<-rpois(10,10)
arg

set.seed(2)
hum<-rpois(10,7)
hum

dados<-data.frame(are,arg,hum)
dados

sucesso<-c(are,arg,hum)
solo<-c(rep(c("are","arg","hum"),each=10))
obs<-1:30
dados2=data.frame(obs,solo,sucesso)  
dados2


boxplot(dados)



############################
#     gráfico              #
###########################


par(mfrow=c(2,2),las=1,bty="l")
plot(dados2$sucesso[dados2$solo=="are"]~dados2$obs[dados2$solo=="are"], ylab="Variável resposta ", xlab="observações", pch=16, col="orange", xlim=c(1,30),ylim=c(0,18), main="Efeito do solo")
points(dados2$sucesso[dados2$solo=="arg"]~dados2$obs[dados2$solo=="arg"],pch=17, col="brown")
points(dados2$sucesso[dados2$solo=="hum"]~dados2$obs[dados2$solo=="hum"],pch=18, col="darkgreen")

#variação total
media.geral=mean(c(are,arg,hum))

plot(dados2$sucesso[dados2$solo=="are"]~dados2$obs[dados2$solo=="are"], ylab="Variável resposta", xlab="observações", pch=16, col="orange", xlim=c(1,30),ylim=c(0,18), main="Variação total")
points(dados2$sucesso[dados2$solo=="arg"]~dados2$obs[dados2$solo=="arg"],pch=17, col="brown")
points(dados2$sucesso[dados2$solo=="hum"]~dados2$obs[dados2$solo=="hum"],pch=18, col="darkgreen")
abline(h=media.geral)
segments(dados2$obs,c(rep(media.geral,30)),dados2$obs,dados2$sucesso,col=c(rep(c("orange","brown","darkgreen"),each=10)))

#variação intra grupos
media.are=mean(dados2$sucesso[dados2$solo=="are"])
media.arg=mean(dados2$sucesso[dados2$solo=="arg"])
media.hum=mean(dados2$sucesso[dados2$solo=="hum"])

plot(dados2$sucesso[dados2$solo=="are"]~dados2$obs[dados2$solo=="are"], ylab="Variável resposta", xlab="observações", pch=16, col="orange", xlim=c(1,30),ylim=c(0,18), main="Variação Intra Grupos")
points(dados2$sucesso[dados2$solo=="arg"]~dados2$obs[dados2$solo=="arg"],pch=17, col="brown")
points(dados2$sucesso[dados2$solo=="hum"]~dados2$obs[dados2$solo=="hum"],pch=18, col="darkgreen")

lines(dados2$obs[dados2$solo=="are"],c(rep(media.are,10)),col="orange")
segments(dados2$obs[dados2$solo=="are"],c(rep(media.are,10)),dados2$obs[dados2$solo=="are"],dados2$sucesso[dados2$solo=="are"],col="orange")

lines(dados2$obs[dados2$solo=="arg"],c(rep(media.arg,10)),col="brown")
segments(dados2$obs[dados2$solo=="arg"],c(rep(media.arg,10)),dados2$obs[dados2$solo=="arg"],dados2$sucesso[dados2$solo=="arg"],col="brown")

lines(dados2$obs[dados2$solo=="hum"],c(rep(media.hum,10)),col="darkgreen")
segments(dados2$obs[dados2$solo=="hum"],c(rep(media.hum,10)),dados2$obs[dados2$solo=="hum"],dados2$sucesso[dados2$solo=="hum"],col="darkgreen")

#variação entre grupos
media.grupos<-mean(c(media.arg,media.are,media.hum))

plot(c(rep(media.are,10))~c(1:10),ylab="Variável resposta", xlab="observações", pch=16, col="orange", xlim=c(1,30),ylim=c(0,18), main="Variação Entre Grupos")
points(c(rep(media.arg,10))~c(11:20),pch=17, col="brown")
points(c(rep(media.hum,10))~c(21:30),pch=18, col="darkgreen")
abline(h=media.grupos)
segments(dados2$obs[dados2$solo=="are"],c(rep(media.are,10)),dados2$obs[dados2$solo=="are"],c(rep(media.grupos,10)),col="orange")
segments(dados2$obs[dados2$solo=="arg"],c(rep(media.arg,10)),dados2$obs[dados2$solo=="arg"],c(rep(media.grupos,10)),col="brown")
segments(dados2$obs[dados2$solo=="hum"],c(rep(media.hum,10)),dados2$obs[dados2$solo=="hum"],c(rep(media.grupos,10)),col="darkgreen")

legend("topright",c("are","arg","hum"),pch=c(16,17,18), col=c("orange","brown","darkgreen"),cex=0.8)




###################################
##########  TABELA ANOVA ###########
####################################

#criação da tabela
tab=matrix(data=NA,nrow=3,ncol=5)
colnames(tab)=c("desvio QuadrÃ¡tico","Graus de liberdade","Desvio mÃ©dio","RazÃ£o de variÃ¢ncias","Probabilidade")
rownames(tab)=c("Entre Grupos","Intra grupos","Total")
tab

#simulação dos dados
set.seed(2)
are<-rpois(10,5)
are

set.seed(2)
arg<-rpois(10,10)
arg

set.seed(2)
hum<-rpois(10,7)
hum


dados<-data.frame(are,arg,hum)
dados
sucesso<-c(are,arg,hum)
solo<-c(rep(c("are","arg","hum"),each=10))
obs<-1:30
dados2=data.frame(obs,solo,sucesso)  
dados2

#ss.total: Diferença de cada observaÃ§Ã£o da media geral. Na prática: soma do quadrada das diferenÃ§a entre cada observaÃ§Ã£o e a media geral dos dados
media.geral=mean(c(are,arg,hum))#mÃ©dia geral dos dados
media.geral
dif.geral=dados-media.geral #diferença entre cada observação e a media geral dos dados
dif.geral
ss.solos=dif.geral^2 #quadrado das diferenÃ§as
ss.solos
ss.total=sum(ss.solos)#soma dos quadrados
ss.total  
tab[3,1]=ss.total
tab

#ss.intra:  diferenças entre cada grupo e sua media
media.solos=apply(dados,2, mean)
media.solos

ss.are=sum((are-media.solos["are"])^2)
ss.are
ss.arg=sum((arg-media.solos["arg"])^2)
ss.arg
ss.hum=sum((hum-media.solos["hum"])^2)
ss.hum
ss.intra=ss.are+ss.arg+ss.hum
ss.intra
tab[2,1]=ss.intra
tab

#ss.entre: diferenças entre as medias de cada um dos grupos

ss.entre=10*sum((media.solos-media.geral)^2)
ss.entre
tab[1,1]=ss.entre
tab

ss.intra+ss.entre
ss.total  

# graus de liberdade
tab[3,2]=(3*10)-1 #total: n.grupos*n.replicas-1
tab
tab[2,2]=3*(10-1) #intra: n.grupos(n.replicas-1)
tab
tab[1,2]=3-1 #entre: n.grupos-1
tab

#Cálculo do F
ms.entre=ss.entre/2
ms.entre
tab[1,3]=ms.entre
tab

ms.intra=ss.intra/27  
ms.intra
tab[2,3]=ms.intra
tab

Fischer=ms.entre/ms.intra
Fischer
tab[1,4]=Fischer
tab

#p
prob=pf(q=Fischer,df1=2,df2=27, lower.tail=FALSE) #unicaudal
prob
#significativo se p<0.05


#### anova no R
head(dados2)

mod<-aov(dados2$sucesso~dados2$solo)
summary(mod)
anova(aov(dados2$sucesso~dados2$solo))


#post-hoc
TukeyHSD(x=mod, 'dados2$solo', conf.level=0.95)
boxplot(dados)







#gráfico fischer
par(mfrow=c(1,1))
curve(expr=df(x, 2,27),main="Distribuição F de Fisher (df=2,27)", xlab="Valor F",ylab="Densidade Probabilística (df)",xlim=c(0,10), ylim=c(0, 0.2))
abline(v=Fischer,col="red")
abline(h=0, lty=2)
xf=seq(Fischer,10,0.01)
ydf=df(xf,2,27)
polygon(c(Fischer,xf),c(0,ydf),col="red")

2