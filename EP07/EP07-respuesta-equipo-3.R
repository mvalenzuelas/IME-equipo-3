library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

#1.-

#H0: adolescentes y adultos jóvenes comparten de igual manera el gusto por las películas de superheroes
#H1: adolescentes y adultos jóvenes no comparten de igual manera el gusto por las películas de superheroes

adolescentes<-c(8,4)
adulto_joven<-c(5,10)

tabla<-as.table(rbind(adolescentes,adulto_joven))

dimnames(tabla)<-list(etario=c("adolescentes","adulto joven"),
                      preferencia=c("Seguidor","No seguidor"))
print(tabla)

prueba1<-fisher.test(tabla)
print(prueba1)

#Con un nivel de significancia alfa=0.05 existe suficiente evidencia para 
#aceptar la hipotesis nula. Es decir los adolescentes y adultos jovenes 
#comparten gustos por las peliculas de superheroes

#2.-
#Establecer hipotesis
#H0: No existen cambios en los trastornos del lenguaje al alimentar a uno de los gemelos con proteina B1
#H1: Existen cambios en los trastornos del lenguaje al alimentar a uno de los gemelos con proteina B1
gemelos<-seq(1:35)
alimentadoB1<-c(rep("Sin problema",10),rep("Problema",2),rep("Problema",6),rep("Sin problema",17))
sinB1<-c(rep("Sin problema",10),rep("Problema",2),rep("Sin problema",6),rep("Problema",17))

datos2<-data.frame(gemelos,alimentadoB1,sinB1)

tabla2<-table(sinB1,alimentadoB1)

prueba2<-mcnemar.test(tabla2)

print(prueba2)

#Con un nivel de significancia de alfa =0.05 existe suficiente evidencia para 
#rechazar la hipotesis nula.Por ende se puede establecer que los investigadores 
#acertaron en el estudio realizado

#3.-
#Establecer hipotesis 
#H0: Las variables lugar de origen de la sangre y tipo de sangre predilecto son independientes
#H1: Las variables lugar de origen de la sangre y tipo de sangre predulecto estan relacionadas

europa<-c(14,25,10,3)
america<-c(8,13,11,16)

tabla3<-as.table(rbind(europa,america))
dimnames(tabla3)<-list(lugar=c("Europa","America"),
                       tipo=c("A","B","AB","O"))
prueba3<-fisher.test(tabla3,0.05)

#Con un nivel de significancia de alfa 0.05 existe suficiente evidencia para
#rechazar la hipotesis nula, por ende se puede establecer una relaicon entre la
#el tipo de sangre que prefieren los vampiros con el lugar de origen del vampiro

#4.-
#Establecer hipotesis
#H0: La proporciOn de estudiantes aprobados es igual en todos los ramos
#H1: La proporcion de estudiantes aprobados es distinta en al menos un ramo



archivo<-read.csv2("EP07 Datos.csv")
set.seed(278)
datos3<-sample_n(archivo,50)
datos3<-datos3%>%pivot_longer(c("Calculo","Algebra","Fisica"),
                             names_to="Asignatura",
                             values_to="Estado"
                             )
datos3[["Asignatura"]]<-factor(datos3[["Asignatura"]])
datos3[["Estado"]]<-factor(datos3[["Estado"]])

prueba4<-cochran.qtest(Estado~Asignatura|Id,data=datos3,alpha=0.05)
print(prueba4)

#Con un nivel de significancia de alfa=0.05 existe suficiente evidencia para 
#rechazar la hipotesis alternativa. Por ende se puede concluir que la proporción
#de aprobación de los 3 ramos es similar.