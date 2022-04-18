library(dplyr)
library(Hmisc)


Especialidad <- c('Pediatría','Obstetricia','Dermatología','Psiquiatría','Medicina Interna','Oncología', 'Neurología','Anestesiología','Radiología')
Mujeres <- c(54,71,35,30,45,44,56,21,17)
Hombres <-c(52,66,41,42,65,62,88,40,35)

datos <- data.frame(Especialidad,Mujeres,Hombres)

#Equipo 1:

# Pregunta 1
#1-  Estudios previos habían determinado que la proporción de autoras en la especialidad de dermatología era de 61%.
#    ¿Respaldan estos datos tal estimación?

# Se filtra la matriz de datos con los datos correspondientes a la pregunta
Dermatologia <- datos %>% filter(Especialidad == 'Dermatología')
Total <- Dermatologia %>% summarise( Total = Mujeres + Hombres)
Dermatologia <- Dermatologia %>% mutate(Total)

# Valores conocidos
n <- as.vector(t( Dermatologia %>% select(Total) ))
p_exito <- as.vector(t(Dermatologia %>% summarise(p_exito = Mujeres/Total)))
alfa <- 0.05
valor_nulo <- 0.61

# Verificación condiciones
# En el enunciado del ejemplo no se indica que las instancias del problema fueron escogidas de manera aleatoria, 
# pero asumiremos que se cumple para la realización del ejercicio. También podemos asumir que 
# representan menos del 10 % del total de instancias posibles, con lo que se verifica la independencia de las observaciones
# Por otra parte, se cumple que la proporción de exito por el total de observaciones es mayor a 10, por lo que se cumple
# la condición de exito-fracaso.
cat("(Condición exito fracaso) Proporción de exito por el total de observaciones", n*p_exito)
# Ahora solo queda verificar la segunda condición con el valor nulo, por lo que si multiplicamos el valor nulo por
# el total tenemos que es mayor a 10, por lo que se cumplen todas las condiciones para aplciar la prueba.
cat("(Condición exito fracaso valor nulo) Valor nulo por el total de observaciones", n*valor_nulo)
#  Con esto último se verifican entonces las condiciones de normalidad.

# Hipotesis
# H0: La proporción de mujeres en dermatología es 61%
# HA: La proporción de mujeres en dermatología es distinto a 61%
#
# Lenguaje matemático
# Denotando p como la proporción de mujeres que pertenecen a dermatología y considerando 
# el valor hipotético p0 = 0,61 para este parámetro:
# H0: p = p0
# HA: p!= p0


# Cantidad de exitos
exitos_mujeres_derm <- as.vector(t(Dermatologia %>% select(Mujeres)))


# Prueba de Wilson bilateral para una proporción
prueba1 <- prop.test ( exitos_mujeres_derm , n = n , p = valor_nulo,
                        alternative = "two.sided", conf.level = 1 - alfa )

print(prueba1)

# Respuesta 1
# Dado que el valor nulo no se encuentra en el intervalo de confianza y además el valor p obtenido es menor al nivel
# de significación alfa = 0.05, tenemos suficiente evidencia para rechazar H0 en favor de HA.
# Entonces, dado que rechazamos que la proporción de mujeres en dermatología es 61%,
# se puede afirmar con 95 % de confianza que los datos no respaldan la estimación.


# Pregunta 2
#2-  Según estos datos, ¿es igual la proporción de autoras en las áreas de oncología y medicina interna?

# Se filtra la matriz de datos con los datos correspondientes a la pregunta
# Datos oncología
Oncologia <- datos %>% filter(Especialidad == 'Oncología')
Total_onc <- Oncologia %>% summarise( Total_onc = Mujeres + Hombres)
Oncologia <- Oncologia %>% mutate(Total_onc)
# Datos medicina
Medicina_Interna <- datos %>% filter(Especialidad == 'Medicina Interna')
Total_med <- Medicina_Interna %>% summarise( Total_med = Mujeres + Hombres)
Medicina_Interna <- Medicina_Interna %>% mutate(Total_med)

# Valores mujeres oncología
n_onc <- as.vector(t(Oncologia %>% select(Total_onc)))
exitos_mujeres_onc <- as.vector(t(Oncologia %>% select(Mujeres)))

# Valores mujeres medicina interna
n_med <- as.vector(t(Medicina_Interna %>% select(Total_med)))
exitos_mujeres_med <- as.vector(t(Medicina_Interna %>% select(Mujeres)))

# Verificación condiciones
# En el enunciado del ejemplo no se indica que las instancias del problema fueron escogidas de manera aleatoria, 
# pero asumiremos que se cumple para la realización del ejercicio. También podemos asumir que 
# representan menos del 10 % del total de instancias posibles, con lo que se verifica la independencia de las observaciones
# De esta forma, en ambos casos, las observaciones son independientes entre si.
# Adicionalmente, ambas muestras son independientes entre sí, pues ambas categorías se excluyen mutuamente. 
# ahora verificando la segunda condición
cat("(Condición exito fracaso) Proporción de mujeres en oncología por el total de observaciones", exitos_mujeres_onc/n_onc,"\n")
cat("(Condición exito fracaso) Proporción de mujeres en medicina interna por el total de observaciones", exitos_mujeres_med/n_med,"\n")
# Con esto se verifica la condición de exito-fracaso para ambos casos

# Dado que la hipótesis nula supone que no hay diferencia entre las proporciones, hay que verificar la condición de exito-fracaso
# usando la proporción agrupada:
p_agrupada <- (exitos_mujeres_med + exitos_mujeres_onc)/(n_onc + n_med)
cat("(Condición exito fracaso p1) Proporción de mujeres en oncología", p_agrupada*n_onc,"\n")
cat("(Condición exito fracaso p1) Proporción de hombres en oncología", (1-p_agrupada)*n_onc,"\n")

cat("(Condición exito fracaso p2) Proporción de mujeres en medicina interna", p_agrupada*n_med,"\n")
cat("(Condición exito fracaso p2) Proporción de hombres en medicina interna", (1-p_agrupada)*n_med,"\n")

# Con esto último se verifican entonces las condiciones de normalidad para la diferencia de proporciones.

# Hipotesis
# H0: La proporción de mujeres en las áreas de oncología y medicina interna es igual
# HA: La proporción de mujeres en las áreas de oncología y medicina interna es distinta

# Lenguaje matemático
# Denotando p1 como la proporción de mujeres que pertenecen a oncología y
# p2 como la proporción de mujeres que pertenecen a medicina interna:
# H0: p1 - p2 = 0
# H1: p1 - p2 != 0

# Valores prueba
n_2 <-c(n_onc , n_med)
exitos_2 <- c(exitos_mujeres_onc , exitos_mujeres_med)
alfa <- 0.05

# Prueba de Wilson bilateral para dos proporciones.
prueba2 <- prop.test ( exitos_2 , n = n_2 , alternative = "two.sided",
                        conf.level = 1 - alfa )
print(prueba2)

# Respuesta 2
# Dado que el valor nulo se encuentra en el intervalo de confianza y además el valor p obtenido es bastante mayor
# al nivel de significación alfa = 0.05, se falla al rechazar la hipotesis nula.
# En consecuencia, se puede afirmar con 95 % de confianza que pareciera no haber diferencia
# en las proporciones de mujeres en las áreas de oncología y medicina interna.


# Pregunta 3
#3-  Suponiendo que la diferencia en la proporción de autoras en la especialidad de obstetricia y la de neurología es de 0,19.
#    ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza del 97,5% y poder estadístico de 75%, si
#    se intenta mantener aproximadamente la misma proporción de gente estudiada en cada caso?

# Datos obstetricia
Obstetricia <- datos %>% filter(Especialidad == 'Obstetricia')
n_obs <- Obstetricia %>% summarise( Total_obs = Mujeres + Hombres)
Obstetricia <- Obstetricia %>% mutate(n_obs)
exitos_mujeres_obs <- as.vector(t(Obstetricia %>% select(Mujeres)))

# Datos neurología
Neurología <- datos %>% filter(Especialidad == 'Neurología')
n_neu <- Neurología %>% summarise( Total_neu = Mujeres + Hombres)
Neurología <- Neurología %>% mutate(n_neu)
exitos_mujeres_neu <- as.vector(t(Neurología %>% select(Mujeres)))

# Datos prueba
diferencia <- 0.19
alfa <- 0.025
poder <- 0.75
p1 <- exitos_mujeres_obs/n_obs
p2 <- exitos_mujeres_neu/n_neu
fraction <- n_obs/(n_obs+n_neu)

tams <- unlist(bsamsize(p1, p2, fraction, alfa, poder))
tam_p1 <- ceiling(tams[1])
tam_p2 <- ceiling(tams[2])
cat("Los tamaños necesarios de las proporciones para que se cumplan las condiciones son: p1 = ", tam_p1," y p2 = ", tam_p2, " \n")




