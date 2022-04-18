library(ggpubr)
library(tidyr)
library(pwr)

#Equipo 1:

# Hipotesis
# H0: La máquina llena los bidones con una media igual a 10 litros
# HA: La máquina llena los bidones con una media distinta a 10 litros

# Pregunta 1

# 1- Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente una media menor
#    a 9,7 litros o mayor a 10,3 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?
valorNulo <- 10
n <- 100
desv <- 1
media <- 10   #mu
desv_muestral <- desv/sqrt(n)     #sigma

probabilidad <- 2*pnorm(9.7, mean = media, sd = desv_muestral, lower.tail = TRUE)
cat ("(Respuesta 1) La probabilidad de cometer un error de tipo 1 es =", probabilidad)

# Pregunta 2
# 2- Si el verdadero volumen medio de los bidones fuera de 10,2 litros, ¿cuál sería la probabilidad
#    de que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

valorNulo <- 10
n <- 100
desv <- 1
mediaVerdadera <- 10.2   #mu
efecto <- (mediaVerdadera - valorNulo)

poder_pregunta2 <- power.t.test(n, delta = efecto, sd = desv, sig.level = 0.01, type = "one.sample"
                                ,alternative = "two.sided")$power

cat ("Probabilidad de correctamente rechazar H0 cuando es falsa (1-beta) = ", poder_pregunta2)

beta <- 1-poder_pregunta2

cat ("(Respuesta 2) Probabilidad de cometer error de tipo II (beta) = ", beta)


# 3- Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico con las
#    condiciones anteriores, pero suponiendo que el verdadero volumen medio podría variar de 9,5 a 10,5 litros.
valorNulo <- 10
n <- 100
desv <- 1
mediaVerdadera3 <- seq(9.5, 10.5, 0.01)   #mu
efecto3 <- (mediaVerdadera3 - valorNulo)

poder_pregunta3 <- power.t.test(n, delta = efecto3, sd = desv, sig.level = 0.01, type = "one.sample"
                                ,alternative = "two.sided")$power

datos <- data.frame(efecto3, poder_pregunta3)

# Grafica curva de poder .
g <- ggplot ( datos ,aes(efecto3,poder_pregunta3))
g <- g + geom_line ()
g <- g + labs ( colour = "")
g <- g + ylab (" Poder estadístico ")
g <- g + xlab (" Tamaño del efecto ")
g <- g + theme_pubr ()
g <- g + ggtitle (" Curvas de poder pregunta 3")
cat ("(Respuesta 3) Se muestra gráfico")
print (g)

# 4- Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para conseguir
#    un poder estadístico de 0,75 y un nivel de significación de 0,05?
desv <- 1
valorNulo <- 10
mediaVerdadera <- 10.2   #mu
delta <- (mediaVerdadera - valorNulo)/desv
alfa <- 0.05
poder <- 0.75

resultado4 <- pwr.t.test(n = NULL, d = delta, sig.level = alfa, power = poder, type="one.sample",
                              alternative="two.sided")

n_result <- ceiling (resultado4[["n"]])

cat ("(Respuesta 4) Debería ser una cantidad (n) de bidones igual a ", n_result, " con un alfa = 0.05\n")


# 5- ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error 
#    de tipo I a un 1% solamente?
valorNulo <- 10
desv <- 1
mediaVerdadera <- 10.2   #mu
delta <- (mediaVerdadera - valorNulo)/desv
alfa <- 0.01
poder <- 0.75

resultado5 <- pwr.t.test(n = NULL, d = delta, sig.level = alfa, power = poder, type="one.sample",
                         alternative="two.sided")

n_result5 <- ceiling (resultado5[["n"]])

cat ("(Respuesta 5) Debería ser una cantidad (n) de bidones igual a ", n_result5, " con un alfa = 0.01\n")

