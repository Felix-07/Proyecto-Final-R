# Programación y Estadística con R

#Lenguaje de programación: R

#Equipo:11
#Integrantes:
  
#Ximena Ávila Villagómez
#Ana Katherine Cuevas Flores
#Félix Alberto Nieto García
#Daniel Gómez Avín
#Santiago Peña Rodríguez
#Alejandro De Fuentes Martínez

#Resumen: 
# El trabajo es sobre el analísis de datos de partido de futbool 
#de la seleccion española.

#Librerias utilizadas
library("ggplot2") #Para graficación
library("dplyr") #Para la manipulación y operaciones con data frames
library("rsample")

#Postwork 1

#1. Importa los datos de soccer de la temporada 2019/2020 de 
#la primera división de la liga española a R que se encuentran en el 
#enlace:  https://www.football-data.co.uk/spainm.php

LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

DatosT1920 <- read.csv(LinkT1920)
head(DatosT1920) #Primeros registros 

str(DatosT1920) #Estructura de los datos


#2. Se extrae las columnas que contienen los números de goles anotados por los 
#equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que
#jugaron como visitante (FTAG)

Goles <- select(DatosT1920, FTHG,FTAG)

#3. Se elaboran tablas de frecuencias relativas para estimar las siguientes 
#probabilidades marginales del equipo que juega en casa, la probabilidad marguinal 
#para el equipo que juega como visitante y la probabilidad conjunta.

ProbConjunta <- prop.table(table(Goles$FTHG, Goles$FTAG ))
colnames(ProbConjunta) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles")
rownames(ProbConjunta) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles", "6 goles")

print('Probabilidad conjunta')
round(ProbConjunta,4)



ProbCasa<- prop.table(apply(ProbConjunta,MARGIN = 1,sum))
ProbVisitante<- prop.table( apply(ProbConjunta,MARGIN = 2,sum))
ProbMarginal <- cbind(ProbCasa,c(ProbVisitante, NaN))
colnames(ProbMarginal)[2] <- 'ProbVisitante'
ProbMarginal


Probabilidad<-rbind(cbind(ProbConjunta,ProbCasa),c(ProbVisitante,NaN))
rownames(Probabilidad)[8] <- 'ProbVisitante'
Probabilidad


#Graficas de las probabilidades marginales
layout(matrix(c(1:2), nrow=2, byrow=FALSE))
barplot(ProbCasa, main = "Goles de casa",xlab="Goles",ylab="Frecuencia relativa",
        col = "steelblue")

barplot(ProbVisitante, main = "Goles de los visitante",xlab="Goles",ylab="Frecuencia relativa",
        col = "steelblue")

#Postwork 2
#1. Se importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 
#2019/2020 de la primera división de la liga española, los datos los puedes 
#encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

LinkT1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LinkT1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"


download.file(url = LinkT1920, destfile = "SP1.1920.csv", mode = "wb")
download.file(url = LinkT1819, destfile = "SP1.1819.csv", mode = "wb")
download.file(url = LinkT1718, destfile = "SP1.1718.csv", mode = "wb")

LigaEspanola <- lapply(dir()[141:143] , read.csv, ) 
#Si tienes más archivos en tu carpeta, tienes que colorar indices en el dir()[a:b]

#2. Las estructuras de de los datas frames al usar las funciones

str(LigaEspanola)
View(LigaEspanola)
summary(LigaEspanola)
head(LigaEspanola)

#3. Selección de   las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR;
#esto para cada uno de los data.
LigaEspanola <- lapply(LigaEspanola, select, Date, HomeTeam:FTR)

#4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos 
#data frames sean del mismo tipo.
DatosLE <- do.call(rbind, LigaEspanola)
DatosLE <- mutate(DatosLE, Date = as.Date(Date, "%d/%m/%Y"))
str(DatosLE)
head(DatosLE)

#Postwork 3

#1. Con el último data frame obtenido en el postwork de la sesión 2, elabora 
#tablas de frecuencias relativas para estimar las siguientes probabilidades: 
#Probabilidad marginal equipo de casa, probabilidad marginal de equipo visitante,
#probabilida conjunta.

ProbConjuntaLE <- prop.table(table(DatosLE$FTHG,DatosLE$FTAG))
colnames(ProbConjuntaLE) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles", "6 goles")
rownames(ProbConjuntaLE) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles", "6 goles","7 goles","8 goles")

print('Probabilidad conjunta')
round(ProbConjuntaLE,5)

ProbCasaLE<- prop.table(apply(ProbConjuntaLE,MARGIN = 1,sum))
ProbVisitanteLE<- prop.table( apply(ProbConjuntaLE,MARGIN = 2,sum))
ProbMarginalLE <- cbind(ProbCasaLE,c(ProbVisitanteLE, NaN,NaN))
colnames(ProbMarginalLE)[2] <- 'ProbVisitanteLE'
ProbMarginalLE


#2. Realiza lo siguiente:
  
#Un gráfico de barras para las probabilidades marginales estimadas del número de
#goles que anota el equipo de casa.
#Un gráfico de barras para las probabilidades marginales estimadas del número de
#goles que anota el equipo visitante.
#Un HeatMap para las probabilidades conjuntas estimadas de los números de goles
#que anotan el equipo de casa y el equipo visitante en un partido.

layout(matrix(c(1:2), nrow=2, byrow=FALSE))
barplot(ProbCasaLE, main = " Probabilidad del Numero de Goles \n que Anota el Equipo en Casa", 
        xlab = "Numero de Goles", ylab = "Probabilidad", col = "#58d68d")

barplot(prop.table(table(DatosLE$FTAG)), main = "Probabilidad del Número de Goles \n que Anota el Equipo Visitante", 
        xlab = "NÃºmero de Goles", ylab = "Probabilidad", col = "#85c1e9")


heatmap(ProbConjuntaLE,
        scale = "none",
        Rowv = NA, Colv = NA, 
        col = cm.colors(256),
        xlab="Goles del Equipo Visitante", ylab="Goles del Equipo en Casa",
        main = "Probabilidad del Marcador de Goles")

#Postwork 4
#Investigación sobre la dependencia o independencia del número de goles anotados
#por el equipo de casa y el número de goles anotados por el equipo visitante 
#mediante un procedimiento denominado bootstrap.

#1. Obtén una tabla de cocientes al dividir estas probabilidades conjuntas por
#el producto de las probabilidades marginales correspondientes.

ProbCo =round(ProbConjuntaLE/t(ProbVisitanteLE %*% t(ProbCasaLE)),5)
ProbCo

#2.Mediante un procedimiento de boostrap, obtén más cocientes similares a los 
#obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona 
#en cuáles casos le parece razonable suponer que los cocientes de la tabla en 
#el punto 1, son iguales a 1 (en tal caso tendríamos independencia de las 
#variables aleatorias X y Y).

set.seed(839287482)
Nmuestras = 2 # Número de muestras bootstrap
DatosLE_boot <- bootstraps(DatosLE, times = Nmuestras)

Primera_boot <- DatosLE_boot$splits[[1]]
Segunda_boot <- DatosLE_boot$splits[[2]]
#Informacion de la primera muestra
Primera_boot
Segunda_boot

PrMuestraB = as.data.frame(Primera_boot)
SeMuestraB = as.data.frame(Segunda_boot)
head(PrMuestraB)
head(SeMuestraB)

#Se repite el proceso para obtener las probabilidades
#Se crea una función que lo haga automáticamente
ProbConjMarg <- function(x,y){
  
  ProbConjunta <- prop.table(table(x,y))
  ProbMargX<- prop.table(apply(ProbConjunta,MARGIN = 1,sum))
  ProbMargY<- prop.table(apply(ProbConjunta,MARGIN = 2,sum))
  
  heatmap(ProbConjunta ,
          scale = "none",
          Rowv = NA, Colv = NA, 
          col = cm.colors(256),
          xlab="Goles del Equipo Visitante", ylab="Goles del Equipo en Casa",
          main = "Probabilidad del Marcador de Goles")
  
  return(list(ProbConjunta,ProbMargX,ProbMargY))
}

ProbMuestra1<-ProbConjMarg(PrMuestraB$FTHG,PrMuestraB$FTAG )
round(ProbMuestra1[[1]],5)

ProbMuestra2<-ProbConjMarg(SeMuestraB$FTHG,SeMuestraB$FTAG) 
round(ProbMuestra2[[1]],5)

#Comparación
print("Inicial")
ProbCo

print("Primera muestra")
ProbCoM1 <- round(ProbMuestra1[[1]]/t(ProbMuestra1[[3]] %*% t(ProbMuestra1[[2]])),5)
ProbCoM1 

print("Segunda muestra")
ProbCoM2 <-round(ProbMuestra2[[1]]/t(ProbMuestra2[[3]] %*% t(ProbMuestra2[[2]])),5)
ProbCoM2



#Como se puede observar en los heatmaps de los muestreos, la distribución es
#similar al de la población. Para obtener una representación aún más similar 
#a la real, sería necesario aumentar el tamaño de muestras.









