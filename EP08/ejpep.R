library (dplyr)
library(pwr)

#Lea el siguiente enunciado:
 # Un estudio sobre la mortalidad de pacientes con fallas coronarias, la principal causa de muerte en
#el mundo, determinó que no había una diferencia significativa en los niveles de creatina quinasa
#entre los pacientes que presentaron anemia, una carencia de suficientes glóbulos rojos sanos para
#transportar oxígeno por el cuerpo, durante el periodo de control y los pacientes que no tuvieron esta
#afección. 
#Usando como semilla el valor 127, tome una muestra aleatoria de 25 pacientes que no
#presentaron anemia durante el periodo de control y 25 pacientes que sí la presentaron y, explicando y
#justificando paso a paso el procedimiento seguido (hipótesis contrastadas, prueba estadística usada,
#verificación de condiciones, etc.), realice un análisis inferencial, con un nivel de significación de 0,05,
#que determine si la afirmación hecha por el estudio es correcta de acuerdo a los datos obtenidos.
#Además, determine el poder estadístico de la prueba realizada. Además, determine el poder
#estadístico de la prueba realizada.

set.seed(127) # semilla inicial

no_anemia <- datos %>% filter(anaemia == 0)
no_anemia <- no_anemia[["creatinine_phosphokinase"]]

si_anemia <- datos %>% filter(anaemia == 1)
si_anemia <- si_anemia[["creatinine_phosphokinase"]]
muestra_si_anemia<-sample(si_anemia,25)
muestra_no_anemia<-sample(no_anemia,25)



# VERIFICAR T DE STUDENT
# SE VERIFICA NORMALIDAD DE AMBAS MUESTRAS POR SEPARADO
normalidad_no <- shapiro.test (muestra_no_anemia)
print (normalidad_no)
normalidad_si <- shapiro.test (muestra_si_anemia)
print (normalidad_si)

# OBTENER AMBAS MUESTRAS
media_no_anemia <- mean(muestra_no_anemia)
media_si_anemia <- mean(muestra_si_anemia)


# OBTENER SD DE AMBAS MUESTRAS
sd_si_anemia <- sd(muestra_si_anemia)
sd_no_anemia <- sd(muestra_no_anemia)

alfa <- 0.05
n= 25

# T DE STUDENT, MUESTRAS INDEPENDIENTES (NO PAREADAS)
prueba <- t.test(x = muestra_no_anemia,
                 y = muestra_si_anemia, 
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1- alfa)

print(prueba)

# DEVEST AGRUPADA PARA CALCULAR D DE COHEN
sp <- sqrt((sd_si_anemia^2*(n - 1)+sd_no_anemia^2*(n - 1)) / (n+n-2))

# Se calcula la d de cohen
factor_correccion <- (n+n-3)/(n+n-2.25)
d <- ((media_si_anemia-media_no_anemia)/sp)*factor_correccion

# CALCULAR PODER
poder <- pwr.t.test ( n =n,
                      d =d,
                      sig.level = alfa,
                      power = NULL,
                      type = "two.sample",
                      alternative = "two.sided")
print(poder)

# Tiene una prob de 66% de rechazar correctamente h0 cuando en realidad es falsa, y una prob
# de 34% de cometer un error de tipo 2. 
