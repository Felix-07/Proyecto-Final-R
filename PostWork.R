# Programaci�n y Estad�stica con R

#Lenguaje de programaci�n: R

#Equipo:11
#Integrantes:
  
#Ximena �vila Villag�mez
#Ana Katherine Cuevas Flores
#F�lix Alberto Nieto Garc�a
#Daniel G�mez Av�n
#Santiago Pe�a Rodr�guez
#Alejandro De Fuentes Mart�nez

#Resumen: 
# El trabajo es sobre el anal�sis de datos de partido de futbool 
#de la seleccion espa�ola.

#Librerias utilizadas
library("ggplot2") #Para graficaci�n
library("dplyr") #Para la manipulaci�n y operaciones con data frames
library("rsample")

#Postwork 1

#1. Importa los datos de soccer de la temporada 2019/2020 de 
#la primera divisi�n de la liga espa�ola a R que se encuentran en el 
#enlace:  https://www.football-data.co.uk/spainm.php

LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

DatosT1920 <- read.csv(LinkT1920)
head(DatosT1920) #Primeros registros 

str(DatosT1920) #Estructura de los datos


#2. Se extrae las columnas que contienen los n�meros de goles anotados por los 
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
#2019/2020 de la primera divisi�n de la liga espa�ola, los datos los puedes 
#encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

LinkT1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LinkT1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"


download.file(url = LinkT1920, destfile = "SP1.1920.csv", mode = "wb")
download.file(url = LinkT1819, destfile = "SP1.1819.csv", mode = "wb")
download.file(url = LinkT1718, destfile = "SP1.1718.csv", mode = "wb")

LigaEspanola <- lapply(dir()[141:143] , read.csv, ) 
#Si tienes m�s archivos en tu carpeta, tienes que colorar indices en el dir()[a:b]

#2. Las estructuras de de los datas frames al usar las funciones

str(LigaEspanola)
View(LigaEspanola)
summary(LigaEspanola)
head(LigaEspanola)

#3. Selecci�n de   las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR;
#esto para cada uno de los data.
LigaEspanola <- lapply(LigaEspanola, select, Date, HomeTeam:FTR)

#4. Aseg�rate de que los elementos de las columnas correspondientes de los nuevos 
#data frames sean del mismo tipo.
DatosLE <- do.call(rbind, LigaEspanola)
DatosLE <- mutate(DatosLE, Date = as.Date(Date, "%d/%m/%Y"))
str(DatosLE)
head(DatosLE)

#Postwork 3

#1. Con el �ltimo data frame obtenido en el postwork de la sesi�n 2, elabora 
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
  
#Un gr�fico de barras para las probabilidades marginales estimadas del n�mero de
#goles que anota el equipo de casa.
#Un gr�fico de barras para las probabilidades marginales estimadas del n�mero de
#goles que anota el equipo visitante.
#Un HeatMap para las probabilidades conjuntas estimadas de los n�meros de goles
#que anotan el equipo de casa y el equipo visitante en un partido.

layout(matrix(c(1:2), nrow=2, byrow=FALSE))
barplot(ProbCasaLE, main = " Probabilidad del Numero de Goles \n que Anota el Equipo en Casa", 
        xlab = "Numero de Goles", ylab = "Probabilidad", col = "#58d68d")

barplot(prop.table(table(DatosLE$FTAG)), main = "Probabilidad del N�mero de Goles \n que Anota el Equipo Visitante", 
        xlab = "Número de Goles", ylab = "Probabilidad", col = "#85c1e9")


heatmap(ProbConjuntaLE,
        scale = "none",
        Rowv = NA, Colv = NA, 
        col = cm.colors(256),
        xlab="Goles del Equipo Visitante", ylab="Goles del Equipo en Casa",
        main = "Probabilidad del Marcador de Goles")

#Postwork 4
#Investigaci�n sobre la dependencia o independencia del n�mero de goles anotados
#por el equipo de casa y el n�mero de goles anotados por el equipo visitante 
#mediante un procedimiento denominado bootstrap.

#1. Obt�n una tabla de cocientes al dividir estas probabilidades conjuntas por
#el producto de las probabilidades marginales correspondientes.

ProbCo =round(ProbConjuntaLE/t(ProbVisitanteLE %*% t(ProbCasaLE)),5)
ProbCo

#2.Mediante un procedimiento de boostrap, obt�n m�s cocientes similares a los 
#obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona 
#en cu�les casos le parece razonable suponer que los cocientes de la tabla en 
#el punto 1, son iguales a 1 (en tal caso tendr�amos independencia de las 
#variables aleatorias X y Y).

set.seed(839287482)
Nmuestras = 2 # N�mero de muestras bootstrap
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
#Se crea una funci�n que lo haga autom�ticamente
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

#Comparaci�n
print("Inicial")
ProbCo

print("Primera muestra")
ProbCoM1 <- round(ProbMuestra1[[1]]/t(ProbMuestra1[[3]] %*% t(ProbMuestra1[[2]])),5)
ProbCoM1 

print("Segunda muestra")
ProbCoM2 <-round(ProbMuestra2[[1]]/t(ProbMuestra2[[3]] %*% t(ProbMuestra2[[2]])),5)
ProbCoM2



#Como se puede observar en los heatmaps de los muestreos, la distribuci�n es
#similar al de la poblaci�n. Para obtener una representaci�n a�n m�s similar 
#a la real, ser�a necesario aumentar el tama�o de muestras.








