#Cargar librerias
library(dplyr)
library(tidyr)
library(ggpubr)

#Cargar datos de la poblacion, establecer tamaño de la población y calcular medidas estadisticas
poblacion <- read.csv2("C:\\Users\\Maxi\\Desktop\\iME\\EP02\\EP02 Datos Casen 2017.csv", encoding = "UTF-8")
tamaño <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

#1.- Definir semilla general 5000 datos con una distribución aproximadamente normal
set.seed(984348)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#2.- Conseguir la desviacion normal estadarizada (Z), mediante el calculo de la media
# y la desviación estandar
media <- mean(ingreso.normal)
desviacion_estandar <- sd(ingreso.normal)

  #Crear lista con la desviación normal estandar
Z<-list()
for (x in ingreso.normal) {
  Z<-c(Z,(media-x)/desviacion_estandar)
  
}

df_z<-data.frame("Ingresos estandarizado"=unlist(Z))
g1<-gghistogram(df_z,x="Ingresos.estandarizado",
                bins=100,
                xlab="Ingresos estandarizado",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean")

#3.- Obtener la distribución de ji-cuadrado con 5 y 13 grados de libertad seleccionando
# k (grados de libertad) aleatoriamente 5000 veces

  #Generear distribución de ji-cuadrado con 5 grados de libertad
df1 <- 5
chiq1 <- list()
for(i in 1:5000){
  ran<-sample(Z,size = df1)
  total<-0
  for (j in ran){
    total <- total+ j*j
  }
  
  chiq1<-c(chiq1,total)
}
df_c1<-data.frame("Ingresos"=unlist(chiq1))
g2<-gghistogram(df_c1,x="Ingresos",
                bins=100,
                xlab="Ingresos",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean")



  #Generear distribución de ji-cuadrado con 13 grados de libertad
df2 <- 13

chiq2 <- list()
for(i in 1:5000){
  ran<-sample(Z,size = df2)
  total<-0
  for (j in ran){
    total <- total+ j*j
  }
  chiq2<-c(chiq2,total)
}

df_c2<-data.frame("Ingresos"=unlist(chiq2))
g3<-gghistogram(df_c2,x="Ingresos",
                bins=100,
                xlab="Ingresos",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean")


#4.-Generar Distribución de F con 5 y 13 grados de libertad
#Se utiliza las distribucciones de ji-cuadrado obtenidas anteriomente y sus grados
# de libertad para obtener las distribución F
  
f<-list()
for (i in 1:5000){
  x1<-chiq1[[i]]
  x2<-chiq2[[i]]
  f<-c(f,(x1/df1)/(x2/df2))
}


df_f<-data.frame("Ingresos"=unlist(f))
g4<-gghistogram(df_f,x="Ingresos",
                bins=100,
                xlab="Ingresos",
                ylab="Frecuencia",
                color="blue",
                fill="blue",
                add="mean")






#Actividad 2

#6.-Definir la semilla
set.seed(21232)
n.repeticiones <- 20

ensayo <- function(x)
  ifelse(sample(poblacion[["sexo"]], 1) == "Mujer", 1, 0)

veinte.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Calcular p; la probabilidad de que una mujer sea escogida de la muestra
cantidad.mujeres<-sum(veinte.repeticiones)
p<-cantidad.mujeres/n.repeticiones


#7.-Generar una distribucion normal
binomil<-list()
for(i in 1:n.repeticiones){
  binomil<-c(binomil,choose(n.repeticiones,i)*p^i*(1-p)^(n.repeticiones-i))
}


df_bin <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(binomil))
g5<-ggbarplot(df_bin,x="K",y="Probabilidad",fill="Blue")


#8.-Generar una distribución geometrica 
geo<-list()
for (i in 1:n.repeticiones){
  geo<-c(geo, (1-p)^(i-1)*p)
}

df_geo <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(geo))
g6<-ggbarplot(df_geo,x="K",y="Probabilidad",fill="Blue")


#Generar una distribución binomial negativa
bin.neg<-list()
for (i in 1:n.repeticiones){
  pr<-choose(n.repeticiones-1,i-1)*p^(i)*(1-p)^(n.repeticiones-i)
  bin.neg<-c(bin.neg,pr)
}

df_bin.neg <- data.frame("K"=c(1:n.repeticiones),"Probabilidad"=unlist(bin.neg))
g7<-ggbarplot(df_bin.neg,x="K",y="Probabilidad",fill="Blue")

