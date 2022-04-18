
#Se cargan las variables de las fechas y la cantidad de fallecidos por region
#REgiones es una variable nomilal
#Las cantidades de casos covid por dá es una variable de razon
library(dplyr)
library(tidyr)

datos<-read.csv2("C:\\Users\\Maxi\\Desktop\\iME\\EP01\\EP01 Datos Covid.csv")
RM<-datos[datos$Region=="Metropolitana",2:length(datos)]
RM<-RM%>%select(ends_with("2021"))
maximo<-max(RM)
RM<-RM%>%pivot_longer(colnames(RM),names_to="Fecha",values_to="Contagios")
fecha_maximo<-RM%>%filter(Contagios==maximo)

total<-datos[datos$Region=="Total",2:length(datos)]
total<-total%>%select(ends_with("2021"))
colnames(total)<-format(as.Date(colnames(total),format="X%d.%m.%Y"),"%m")
total<-total%>%pivot_longer(colnames(total),names_to="Periodo",values_to="Contagios")
total_por_mes<-total%>%group_by(Periodo)%>%summarise(total=sum(Contagios))











