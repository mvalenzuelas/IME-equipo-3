#Cargar librerias
library(TeachingDemos)
library(dplyr)
library(ggpubr)
#Cargar dataset de resultados de los juegos olimpicos que indica la raza y los
#tiempos de los atletas actuales y anteriores
data<-read.csv2("C://Users//Maxi//Desktop//IME//EP04//EP04-datos.csv")

#1.-
#El Comité Olímpico cree que el mejor tiempo medio de los atletas negros después 
#de ingresar al programa de entrenamiento es de 9,63 segundos. ¿Soportan los 
#datos esta afirmación?

#Definir hipotesis
#H0: mu =  9.63
#Ha: mu != 9.63

#Definir nivel de significancia y valor nulo
alfa<-0.05

#Filtrar datos de atletas negros
atletas_negros<-data%>%filter(Raza=="Negra")

#Verificar si la población sigue una distribución normal mediante el 
#test de shapiro
normalidad<-shapiro.test(atletas_negros[["Posterior"]])

#Como los valores son independientes, el tamaño de la muestra es menor a 30 y
#se corroboro mediante el test de shapiro que la población se distribuye de 
#forma cercana a la normal; se utiliza la prueba T para evaluar la hipotesis

#Calcular estadisticos de la muestra
media<-mean(atletas_negros$Posterior)
dv<-sd(atletas_negros$Posterior)

#Realiza la prueba t
prueba_1<-t.test(atletas_negros[["Posterior"]],mu=9.63,alternative = "two.sided",
                 stdev = dv,conf.level = 1-alfa)
print(prueba_1)

#Hay suficiente evidencia para rechazar la hipotesis nula con una significancia 
# de 0.05; por ende los datos no soportan la afirmación planteada

#¿Sugieren los datos que la mejor marca de los atletas blancos se reduce en 
# menos de 1,16 segundos tras el entrenamiento?

#Establecer hipotesis del problema
#H0: mu_dif = -1.16
#H1: mu_dif > -1.16

#Establecer vector con la diferencia de tiempo de los atletas blancos
atletas_blancos<-data%>%filter(Raza=="Blanca")
dif_blanco<-atletas_blancos$Posterior-atletas_blancos$Previo

#Verificar utilizando el test de shapiro si la distribución es normal
normalidad<-shapiro.test(dif_blanco)

#Como la distribución es normal, con tamaño menor a 30 e independiente se 
#utiliza la prueba t de student

#Realizar la prueba de t student
prueba_2<-t.test(dif_blanco,y=NULL,
               alternative = "greater",
               mu=-1.16,
               conf.level = 1-alfa)
print(prueba_2)

#Existe suficiente evidencia para rechazar la hipotesis nula con un nivel de 
#significancia de 0.05 por ende los datos sugieren que la mejor marca de los 
#atletas blancos se reduce en menos de 1,16 segundos tras el entrenamiento

#3.-¿Es posible afirmar que, en promedio, los atletas blancos superan a los 
# orientales por más de 1,16 segundos antes del entrenamiento?

#Plantear hipotesis
#H0: mu_blanca-mu_oriental = 1.16
#H1: mu_blanca-mu_oriental > 1.16

# Elegimos columnas que sirven

data_antes <- data %>% select(Raza, Previo)

# Obtenemos las muestras de ambas razas en variables separadas, blanca y oriental

datos_blancos <- data_antes %>% filter(Raza == 'Blanca')
datos_blancos <- datos_blancos$Previo

datos_orientales <- data_antes %>% filter(Raza == 'Oriental')
datos_orientales <- datos_orientales$Previo

# condiciones: las muestras son pequeñas (< 30), independientes y pasan el test de normalidad
# debido a que p1 y p2 son bastante mayores a 0.05.


p1 <- shapiro.test(datos_blancos)
p2 <- shapiro.test(datos_orientales)

print(p1)
print(p2)

# Usando el t-test para dos muestras independientes
prueba_3 <- t.test(y=datos_blancos,
                   x=datos_orientales,
                   paired=FALSE,
                   alternative= "greater",
                   mu=1.16,
                   conf.level=1-alfa
                   )
print(prueba_3)

#Existe suficiente evidencia como para rechazar la hipotesis alternativa con un
# nivel de significancia del 0.05. Por ende es posible afirmar que en promedio 
# los atletas blancos superan a los orientales por más de 1,16 segundos antes 
# del entrenamiento




