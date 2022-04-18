library(dplyr)
library(ggpubr)
library(pwr)

#Se sabe que una máquina que envasa detergentes industriales llena bidones 
#con un volumen de producto que sigue una distribución normal con desviación
#estándar de 1 litro. Usando una muestra aleatoria de 100 botellas, el 
#ingeniero a cargo de la planta requiere determinar si la máquina está llenando 
#los bidones con una media de 10 litros.



#1.-Si el ingeniero está seguro de que el verdadero volumen medio no puede ser 
#superior a 10 litros y piensa rechazar la hipótesis nula cuando la muestra 
#presente una media menor a 9,7 litros, ¿cuál es la probabilidad de que cometa 
#un error de tipo I?

#Definir hipotesis
#H0: mu=10
#H1: mu<10

n<-100
#Definir valor nulo
mu <- 10

#Definir cota inferior del intervalo de confianza
inf <- 9.7

#Definir desviación estandar de la muestra
sd_muestra<-1/sqrt(100)

#Definir error  estandar
SE <- sd_muestra

alfa <- pnorm(9.7, 
           mean = mu,
           sd = SE,
           lower.tail = TRUE)

cat("El nivel de significación es de alfa es de ",alfa)

#2.-Si el verdadero volumen medio de los bidones fuera de 9.8 litros, ¿cuál 
#sería la probabilidad de que el ingeniero, que obviamente no conoce este dato, 
# cometa un error de tipo II?

true_mean=9.8
null_mean=10

#en el caso de que la hipotesis nula es verdadera
x<- seq(null_mean -5*SE, null_mean + 5*SE, 0.01)
y<-dnorm(x,mean=null_mean,sd=SE)
g<-ggplot(data=data.frame(x,y),aes(x))

g<-g+stat_function(fun=dnorm,args=list(mean=null_mean,sd=SE),colour="red",size=1)

g<-g+ylab("")
g<-g+scale_y_continuous(breaks=NULL)
g<-g+scale_x_continuous(name="Diferencia de volumen medio")

g<-g+theme_pubr()

#Colorear la region de rechazo
Z_critico<-qnorm(alfa,mean=null_mean,sd=SE,lower.tail = TRUE)
q_critico_inferior<-null_mean-Z_critico
q_critico_superior<-null_mean+Z_critico
g<-g+geom_area(data=subset(data.frame(x,y),x >= Z_critico),
                 aes(y = y),
                 colour="red",
                 fill="red",
                 alpha=0.5)
  
g<-g+stat_function(fun=dnorm,args=list(mean=true_mean,sd=SE),colour="blue",size=1)
x1<- seq(null_mean -5*SE, null_mean + 5*SE, 0.01)
y1<-dnorm(x,mean=true_mean,sd=SE)

g<-g+geom_area(data=subset(data.frame(x1,y1),
                           x1 >= Z_critico),
               aes(x=x1,y=y1),
               colour="blue",
               fill="blue",
               alpha=0.5)
print(g)

p_tipoII <- 1 - power.t.test(n=100,
                    delta=0.2,
                    sd=1,
                    sig.level=alfa,
                    power=NULL,
                    type = "one.sample",
                    alternative="one.sided")$power

cat("La probabilidad de obtener un error de tipo dos es de ",p_tipoII)

#3.-Como no se conoce el verdadero volumen medio, genere un gráfico del poder 
#estadístico con las condiciones anteriores, pero suponiendo que el verdadero
#volumen medio podría variar de 9,5 a 10 litros.


# Usamos 10 - 9.5 = 0.5
# Además es prueba unilateral

efecto1 <- seq(0,0.5,0.001)

poder_estadistico <- power.t.test(n = n,
                                delta=efecto1,
                                sd=1,
                                power=NULL,
                                sig.level = alfa,
                                type="one.sample",
                                alternative="one.sided")$power

g1 <- ggplot(data.frame(efecto1, poder_estadistico),
             aes(efecto1, poder_estadistico,
                 colour="red"))
g1 <- g1 + geom_line()
g1 <- g1 + labs(colour="")
g1 <- g1 + ylab("Poder estadistico")
g1 <- g1 + xlab("Tamaño del efecto")
g1 <- g1 + theme_pubr()
g1 <- g1 + ggtitle("Curva de poder estadistico para volumen que varia entre 9.5 y 10")
print(g1)

#4.-Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían 
# revisarse para conseguir un poder estadístico de 0,75 y un nivel de
# significación de 0,05?

n1 <- power.t.test(n=NULL,
               delta=0.2,
               sig.level = 0.05,
               power=0.75,
               sd=1,
               type = "one.sample",
               alternative="one.sided")$n


cat("La cantidad de bidones para conseguir un poder estadistico de 0.75 con un nivel de significancia de 0.05 es de ",n1)

#5.-¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de 
# cometer un error de tipo I a un 1% solamente?

# En este caso, P(error Tipo I) = alpha = 1%

n2 <- power.t.test(n=NULL,
                 delta=0.2,
                 sig.level = 0.01,
                 power=0.75,
                 sd=1,
                 type = "one.sample",
                 alternative="one.sided")$n

cat("La cantidad de bidones para conseguir un poder estadistico de 0.75 con un nivel de significancia de 0.01 es de ",n2)
