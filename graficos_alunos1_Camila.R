
setwd("C:/Users/Camila/Dropbox/Metodologia estatística")
getwd()

########### Aula PLOT #################

### o básico do básico

# plots

Z <- rnorm(20)
Z
plot(Z)
plot(Z, type="p")
plot(Z, type="l")
plot(Z, type="b")
plot(Z, type="h")


### alguns objetos para nos guiar
set.seed(20)
area<- sort(c(rpois(30,400),round(rnorm(12,800,200))))
set.seed(4)
riqueza<-c(round(rpois(42,c(12,4,8))))
cobflorestal<- rep(c(30,50,10),each=2,times=7)
sites <- c(paste("site",1:42)) 

dt1 <- data.frame(sites,area,riqueza,cobflorestal)
dt1



sexo <- rep(c("F","M"),each=3, times=7)
dieta<- rep(c("H","G","O"),each=2,times=7)
especie <- rep(c("Akodon A","Akodon B"), each=3,times=7)
set.seed(42)
peso <- round(rnorm(42, mean=20,sd=4),2)

#Faça um data frame com as variáveis acima na seguinte ordem: sexo,dieta,especie,peso. Nomeie o dataframe como dt2.



### os plots!!!!
plot(area)
plot(riqueza~area)
plot(riqueza~cobflorestal)
plot(riqueza,area)
plot(y=riqueza,x=area)
  #Cartesiana - plot(x,y)  é diferente de Formula - plot(y~x)
  #Ambas as formas são corretas, mas como a grande maioria das análises feitas são no formato y~x, em vez de x,y, acaba ficando mais fácil usar y~x.

head(pressure) #dados da relação de pressão de vapor e temperatura
plot(pressure) 
plot(pressure$pressure~pressure$temperature)
plot(pressure ~ temperature, data=pressure)
    plot(riqueza~area, data=dt1)

head(dt1)
plot(dt1)
pairs(dt1)

?plot
?par


#### pch

plot(riqueza~area)
plot(riqueza~area,pch=16)
plot(riqueza~area,pch=6)
plot(riqueza~area,pch="@")
plot(riqueza~area,pch=c(LETTERS[1:12]))
#pch chart --> slide


### las --> label style,  posição da legenda do eixo em relação ao eixo
plot(riqueza~area,pch=16)
plot(riqueza~area,pch=16, las=3)
plot(riqueza~area,pch=16, las=2)
plot(riqueza~area,pch=16, las=1)



###bty --> border type
plot(riqueza~area,pch=16, las=1, bty="n")
plot(riqueza~area,pch=16, las=1, bty="u")
plot(riqueza~area,pch=16, las=1, bty="o")
plot(riqueza~area,pch=16, las=1, bty="l")



### tcl --> length of axis ticks (relativo ao tamanho do texto)
plot(riqueza~area,pch=16, las=1, bty="l", tcl=-0.3)
plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3)


#####col --> Cores
plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3, col="blue")
#pch do 21 ao 24 ermitem alterar cor da borda
plot(riqueza~area,pch=24, las=1, bty="l", tcl=0.3, col="blue",bg="red")


plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3, col=c(1:9))
plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3, col=rainbow(12))
plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3, col=terrain.colors(12))
plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3, col=heat.colors(12))
plot(riqueza~area,pch=16, las=1, bty="l", tcl=0.3, col=cm.colors(12))

#R color chart --> slide
# R color cheat

