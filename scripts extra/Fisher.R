#EP 07 - Grupo 4

library(ggpubr)
library(dplyr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)


"
---Pregunta 1---
Estudios científicos han descubierto que la inteligencia musical está altamente relacionada con la
inteligencia matemática. Pensando en mejorar la oferta de actividades culturales y recreativas, una
Universidad ha examinado la preferencia de talleres de un grupo de 8 estudiantes de carreras científicas y
11 de carreras humanistas, encontrando que 6 de los primeros y 5 de los segundos participaron de talleres
musicales. ¿Existe relación entre el tipo de carrera que cursan los estudiantes y su participación en
talleres musicales?

"
cat("\n\n---------------------------------\n\n")
cat("---Pregunta 1---\n\n")
#En primera instancia se genera la tabla a partir del enunciado, separando entre quienes asisten a un taller
#musical y la carrera en la que están.

asiste   <- c(5,6)
no_asiste <- c(6,2)
tabla <- as.table(rbind(asiste,no_asiste))
dimnames(tabla) <- list(Asiste_a_taller = c("si","no"),
                        Carrera = c("Humanista","Cientìfica"))
print(tabla)
cat("\n\n----------------\n\n")

# Para el análisis del problema, se decide utilizar la prueba de Fisher, debido que 
# es aquella que permite determinar si las variables a evaluar son independientes. Esta prueba además
# es adecuada para el problema, debido a que las muestras por grupos son menores a 5 en algunos casos, por
# lo que de por si necesita una prueba de independencia para pruebas pequeñas.

#-----Prueba de Fisher------

#H0 : Las variables asistencia y carrera son independientes
#HA : Las variables asistencia y carrea son dependientes

alpha <- 0.05
prueba1 <- fisher.test(tabla, 1-alpha)
print(prueba1)


#Conclusión: Se obtuvo un p_valor>alpha, por lo que se rechaza la hipótesis nula en favor de la alternativa,
#           y por lo tanto, se concluye que las variables asistencia y carrera están relacionadas (son dependientes
#           la una de la otra).










cat("\n\n---------------------------------\n\n")
cat("---Pregunta 2---\n\n")

"
---Pregunta 2---
Siempre tenaz en su lucha para erradicar a los vampiros de la faz de la tierra, Van Helsing desea probar
una vacuna que, según él, causará una grave enfermedad en estos seres una vez que beban la sangre de
sus víctimas. Para ello, ha almacenado una gran cantidad de dosis de su propia sangre, separadas en dos
grupos: uno de ellos contiene el químico de la vacuna, mientras el otro está completamente limpio.
Adicionalmente, Van Helsing cuenta con 13 vampiros cautivos, a los que alimentó con sangre limpia por
una semana. Luego de un periodo de limpieza (durante el cual los vampiros fueron alimentados con su
dieta normal, por lo que eliminaron todo rastro de la sangre de Van Helsing), repitió el experimento con la
sangre que contiene la vacuna. Para ambos casos, registró cuántos vampiros enfermaron, con los
siguientes resultados:
- 2 vampiros no presentaron enfermedad alguna con ninguna de las dietas de Van Helsing.
- 3 vampiros enfermaron tras ambas dietas de Van Helsing.
- 1 vampiro enfermó con la sangre limpia de Van Helsing, pero no con la sangre que contiene la
vacuna.
- 9 vampiros enfermaron con la sangre que contiene la vacuna, pero no con la sangre limpia de Van
Helsing.
¿Es posible decir que la vacuna de Van Helsing está asociada a una enfermedad en los vampiros?

"

#Para abarcar el problema propuesto se procede a generar la tabla de contingencia correspondiente

enferma_ambas <- do.call("rbind", replicate(3, c("Enferma","Enferma"), simplify = FALSE))
enferma_vac   <- do.call("rbind", replicate(9, c("Enferma","No Enferma"), simplify = FALSE))
enferma_limp  <- do.call("rbind", replicate(1, c("No Enferma","Enferma"), simplify = FALSE))
no_enferma    <- do.call("rbind", replicate(2, c("No Enferma", "No Enferma"), simplify = FALSE))

datos <- rbind(enferma_ambas,enferma_vac,enferma_limp,no_enferma)
datos <- data.frame(datos)
datos <- rename(datos, "Vacuna" = X1, "Limpia" = X2)

Vacuna <- datos[,1]
Limpia <- datos[,2]
tabla2 <- table(Vacuna,Limpia)
print(tabla2)
cat("\n\n----------------\n\n")

#Como se puede observar, se trata de un caso de variables dicotómicas pareadas,y a su vez los grupos no poseen
#a lo menos 5 datos, por lo que para evaluar el problema, se decide utilizar una prueba de McNemar.

#-----Prueba de McNemar------

#H0: No hay relación con la vacuna de Helsing y una enfermendad de los vampiros.
#HA: Si hay relación con la vacuna de Helsing y una enfermendad de los vampiros.
 

#--- Se selecciona un nivel de 95% de confianza (Para un alpha = 0.05) 

prueba2 <- mcnemar.test(tabla2)
print(prueba2)

#Respuesta: La prueba de McNemar arrojó un p_valor < alpha, por lo que se rechaza la hipótesis nula en favor de la alternativa,
#           concluyendo con un 95% de confianza que puede existir una relación o asociación entre la vacuna de Helsing y una
#           enfermedad en los vampiros de prueba.










cat("\n\n---------------------------------\n\n")
cat("---Pregunta 3---\n\n")

"
---Pregunta 3---
El 21 de marzo de 2022 se realizó un estudio acerca de la aprobación al presidente Gabriel Boric en una
comunidad universitaria, obteniéndose los resultados que se muestran en la tabla. ¿Existe relación entre el
estamento de la comunidad y la aprobación del presidente?
"

#Primero se genera la tabla a trabajar

aprueba   <- c(96,103,21)
desaprueba <- c(119,132,34)
tabla3 <- as.table(rbind(aprueba,desaprueba))
dimnames(tabla3) <- list( Opcion = c("Aprueba","Desaprueba"),
                        Estamento = c("Estudiante","Profesor","Funcionario"))
print(tabla3)
cat("\n\n----------------\n\n")

#En este caso, se estudian valores mucho mayores a las pruebas anteriores, por lo que se decide utilizar una
#prueba del tipo chi-cuadrado para independencia.

#Condiciones:
#     -Las observaciones son independientes, asumiendo que han sido personas seleccionadas al azar, además de
#   representar claramente menos del 10% de la población universitaria.
#     -Los grupos de la muestra presentan mínimo 5 datos.



#-----Prueba Chi-cuadrado de independencia ------

#Hipótesis:
#H0 : Las variables estamento y opcion son independientes
#HA : Las variables estamento y opcion son dependientes

#Para la evaluación de la prueba se decide utilizar un alpha del 0.05, un valor exigente, debido a que se 
#observa poca dispersión en los datos de la tabla.

prueba3 <- chisq.test(tabla3)
print(prueba3)

#Respuesta: Se obtuvo un p_valor > alpha -> por lo tanto, no se rechaza la hipótesis nula,
#           de modo que con un 95% de confianza se concluye que no debiese existir una relación entre el estamento
#           y su respectiva decisión sobre la aprobación del presidente Boric.

cat("\n\n---------------------------------\n\n")
cat("---Pregunta 4---\n\n")

"
Pregunta 4
La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes
en asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
asignaturas, indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad?
"

#Se planta la semilla indicada
set.seed(592)
#Se leen todos los datos
datos4  <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/IME/EP07/EP07 Datos.csv")
#Se obtiene la muestra de 50
muestra <- datos4 [ sample ( nrow (datos4), size = 50 ),]
instancia <- 1:50
muestra <- data.frame(instancia, muestra[,2],muestra[,3],muestra[,4])
muestra <- rename(muestra,"Instancia" =instancia ,"Cálculo"=muestra...2., "Álgebra"=muestra...3., "Física"=muestra...4.)

#Transformando los valores R y A a 0 y 1 en la muestra
muestra[,2]<-replace(muestra$Cálculo, muestra$Cálculo=="R", 0)
muestra[,2]<-replace(muestra$Cálculo, muestra$Cálculo=="A", 1)

muestra[,3]<-replace(muestra$Álgebra, muestra$Álgebra=="R", 0)
muestra[,3]<-replace(muestra$Álgebra, muestra$Álgebra=="A", 1)

muestra[,4]<-replace(muestra$Física, muestra$Física=="R", 0)
muestra[,4]<-replace(muestra$Física, muestra$Física=="A", 1)

print(muestra)

#Como se puede observar, los datos posee como variable de respuesta de tipo dicotómica, y la variable
#independiente posee 3 observaciones (valores pareados), por lo que para la evaluación de la diferencia
#de estos valores se requiere utilizar una prueba de Cochran.

#Condiciones:
#   -Variable de respuesta es dicotomica (A o R)
#   -Variable independiente es categórica (Cálculo, Álgebra y Física)
#   -Las observaciones son independientes entre si: El valor de aprobación o reprobación
#               no influye entre las asignaturas.
#   -El tamaño de la muestra es mayor a n*k>24 : 50*3 -> 150>24 (Se cumple)

#---Como la muestra cumple con todas las condiciones, se procede a realizar la prueba.


#Transformando los datos a formato largo

datos_long <- muestra %>% pivot_longer(c("Cálculo","Álgebra","Física"),
                                    names_to = "Asignatura",
                                    values_to="Situación")

datos_long[["Instancia"]] = factor(datos_long[["Instancia"]])
datos_long[["Asignatura"]] = factor(datos_long[["Asignatura"]])
cat("\n\n----------------\n\n")


#--Hipótesis
#H0: La proporción de aprobación es la misma para todos los grupos.
#HA: La proporción de aprobación es distinta para al menos un grupo.

prueba4 <- cochran.qtest(Situación~Asignatura|Instancia, data=datos_long,alpha=0.05)
print(prueba4)

#Conclusión: Se obtiene un p_valor>alpha, por lo que no se rechaza la hipótesis nula. Por lo tanto,
#           la prueba de cochran asegura con un 95% de confianza  que la proporción de aprobación es 
#           la misma para todos los grupos. Al presentar igualdad, no se necesita indagar en las diferencias
#           por lo que no es necesario realizar una prueba post-hoc.



