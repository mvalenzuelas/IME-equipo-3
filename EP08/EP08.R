library(dplyr)
library(ggpubr)

data<-read.csv2("EP08 Datos.csv")
data_facil <- data%>%filter(dificultad=="Baja")

#data_computacion <- data_facil%>%filter(area == "Computación")
#data_literatura <- data_facil%>%filter(area == "Literatura")
#data_quimica <- data_facil%>%filter(area == "Quimica")

data_asignaturas <- data_facil%>%filter(area == "Literatura" | area == "Computación" | area == "Química")
data_asignaturas <- data_facil%>%select(id, tiempo)
data_asignaturas[["alumno"]] <- factor(data_asignaturas[["id"]])


#Comprobación de normalidad

g <- ggqqplot(data_asignaturas,
                 x = "alumno",
                 y = "tiempo",
                 color = "alumno")

g <- g + face_wrap(~ alumno)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)
