#Cargar librerias
library(dplyr)
library(tidyr)
library(ggpubr)

# Lectura del archivo 
poblacion <- read.csv2("F://Escritorio//1_2022//IME//IME-equipo-3//Estudio//EP06//datos-pep.csv", encoding = "UTF-8")
sd_poblacion <- sd(poblacion[["estatura"]])
#1.- Definir semilla general 40 datos con una distribución aproximadamente normal
set.seed(523)
muestra <- sample(poblacion[["estatura"]], 40)
media_muestra <- mean(muestra)
sd_muestra <- sd(muestra)

#2.- Test de normalidad
normalidad <- shapiro.test(muestra)
print(normalidad)
# Test de Shapiro arroja 0.53, mayor a un alpha de 0.05, por lo que se verifica
# el supuesto de normalidad

#3.- Definir hipótesis

#Lenguaje natural
#H0 -> Estatura promedio de los soldados es de 1.65
#H1 -> Estatura promedio de los soldados es mayor a 1.65

#Lenguaje matemático
#H0 -> mu = 1.65
#H1 -> mu > 1.65

# Definir nivel de significación
alfa <- 0.05

# Ejecutar prueba
#pruebaz <- z.test(media_muestra, mu = 1.65, alternative = "greater", stdev = sd_poblacion, conf.level = 1-alfa)
prueba <- t.test(muestra, mu = 1.65, alternative = "greater", conf.level = 1-alfa)
