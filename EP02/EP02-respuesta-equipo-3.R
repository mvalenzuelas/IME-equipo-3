#Cargar librerias
library(dplyr)
library(ggpubr)

#Se cargan los datos desde el csv; Datos casen 2017
datos <- read.csv2("C:\\Users\\Maxi\\Desktop\\iME\\EP03\\Casen 2017.csv")

#Se seleccionan desde el dataset las columnas sexo, region e ytot
ingresos_generos <- datos%>%select(sexo,region,ytot)

#Se filtran las observaciones de personas pertenecientes a la region metropolitana
ingresos_generos_rm <-ingresos_generos%>%filter(region=="Regi√≥n Metropolitana de Santiago")

#Se filtra por genero al grupo muestral de las personas pertenecientes a la region metropolitana
ingresos_h_rm <- ingresos_generos_rm%>%filter(sexo=="Hombre")
ingresos_m_rm <- ingresos_generos_rm%>%filter(sexo=="Mujer")

#Se construye un histograma de los ingresos totales de hombres de la region metropolitana
g1 <- gghistogram(ingresos_h_rm,x="ytot",bins=144,
                  xlab="Ingresos de Hombres",ylab="Frecuencia",
                  color="blue", fill="blue"
                  ,add="mean")
  #Los datos se representan en el histograma desviados a la izquierda


#Se construye un histograma de los ingresos totales de mujeres de la region metropolitana
g2 <- gghistogram(ingresos_m_rm,x="ytot",bins=144,
                  xlab="Ingresos de Mujeres",ylab="Frecuencia",
                  color="red", fill="orange"
                  ,add="mean")
  #Los datos se representan en el histograma desviados a la izquierda

#Calcular el promedio de las muestras por genero en la region metropolitana
promedio_h_rm <- mean(ingresos_h_rm$ytot)
promedio_m_rm <-mean(ingresos_m_rm$ytot)

#Calcular mediana de las muestras por genero en la regiÛn metropolitana
mediana_h_rm <- median(ingresos_h_rm$ytot)
mediana_m_rm <- median(ingresos_m_rm$ytot)

#Calcular coeficiente de variabilidad de las muestras por genero en la region metropolitana
cv_h <- sd(ingresos_h_rm$ytot)/promedio_h_rm
cv_m <- sd(ingresos_m_rm$ytot)/promedio_m_rm
  #Los valores de los CV son muy altos y superiores a 1 esto es debido a la presencia de datos
  # atipicos, por ende de sugiere que el promedio no es una medidad de tendencia central
  # que represente correctamente los datos devido al grado de dispersion de los datos
  # por ende se utiliza la mediana


#Comparando las medianas obtenidas de los ingresos totales de hombres y mujeres 
# de la region metropolitana se puede determinar que existe una diferencia en los 
# ingresos de homres y mujeres. Donde las mujeres en la region metropolitana ganan un
# 28.29% menos que los hombres adem·s de encontrarse en el margen del sueldo minimo.


  
