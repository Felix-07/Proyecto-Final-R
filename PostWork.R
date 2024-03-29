# Programaci�n y Estad�stica con R

#Lenguaje de programaci�n: R

#Equipo:11

#Integrantes:
  
#Ximena �vila Villag�mez
#Ana Katherine Cuevas Flores
#F�lix Alberto Nieto Garc�a
#Daniel G�mez Av�n
#Alejandro De Fuentes Mart�nez

#Resumen: 
# El trabajo es sobre el an�lisis de datos de partido de f�tbol de la selecci�n
#espa�ola. En el proceso se utilizaron temas, librer�as y funciones vistas en 
#las ocho clases del m�dulo "Programaci�n y Estad�stica con R". 

#-------------------------------------------------------------------------------
#Librerias utilizadas

library(ggplot2)        #Para graficaci�n de datoa
library(dplyr)          #Para la manipulaci�n y operaciones con data frames
library(rsample)        #Para obtener diferentes tipos de muestreo
library(lubridate)      #Para el tratamiendo de fechas
library(mongolite)      #Para la Conexi�n con bases de datos de Mongo
library(fbRanks)        #Crear modelos para clasificar a los equipos usando los goles anotados
library(shiny)          #Crear web apps interactivas
library(class)          #Funciones para la clasificaci�n
library(stringr)        #Manejo de cadena de caracteres
library(shinydashboard) #Creaci�n de  dashboard

#-------------------------------------------------------------------------------

#Postwork 1 - Introducci�n a R y Software


#Objetivo: Realizar lectura de datos a R, observar algunas caracter�sticas los
#data frames para su manipulaci�n y combinar m�ltiples data frames en un �nico data frame.


#1. Importa los datos de soccer de la temporada 2019/2020 de 
#la primera divisi�n de la liga espa�ola a R que se encuentran en el 
#enlace:  https://www.football-data.co.uk/spainm.php

LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv" #Link de los datos
DatosT1920 <- read.csv(LinkT1920) #Leer datos
head(DatosT1920) #Primeros registros 

str(DatosT1920) #Estructura de los datos


#2. Se extrae las columnas que contienen los n�meros de goles anotados por los 
#equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que
#jugaron como visitante (FTAG)

Goles <- select(DatosT1920, FTHG,FTAG) #Se extraen las columnas deseadas

#3. Se elaboran tablas de frecuencias relativas para estimar las siguientes 
#probabilidades marginales del equipo que juega en casa, la probabilidad marguinal 
#para el equipo que juega como visitante y la probabilidad conjunta.

ProbConjunta <- prop.table(table(Goles$FTHG, Goles$FTAG )) #Probabilidad conjunta
#Se renombras las filas y columnas
colnames(ProbConjunta) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles")
rownames(ProbConjunta) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles", "6 goles")
#Se visualizan los datos
print('Probabilidad conjunta')
round(ProbConjunta,4)


#Se obtiene la probabilidad marginal
ProbCasa<- prop.table(apply(ProbConjunta,MARGIN = 1,sum))
ProbVisitante<- prop.table( apply(ProbConjunta,MARGIN = 2,sum))
#Se reune la informaci�n en un dataframe y se visualiza
ProbMarginal <- cbind(ProbCasa,c(ProbVisitante, NaN))
colnames(ProbMarginal)[2] <- 'ProbVisitante'
ProbMarginal


#Se junta la informaci�n en una sola matriz
Probabilidad<-rbind(cbind(ProbConjunta,ProbCasa),c(ProbVisitante,NaN))
rownames(Probabilidad)[8] <- 'ProbVisitante'
Probabilidad

#-------------------------------------------------------------------------------

#Postwork 2 - Programaci�n y manipulaci�n de datos en R

#Objetivo: Importar multiples datos a R, filtrar filas, seleccionar variables,
#transformar variables y en general manipular los datos para llevarlos a una 
#forma deseada.

#1. Se importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 
#2019/2020 de la primera divisi�n de la liga espa�ola, los datos los puedes 
#encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php

# Links de los csv a descargar
LinkT1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LinkT1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

#Se descargan los archivos
download.file(url = LinkT1920, destfile = "SP1.1920.csv", mode = "wb")
download.file(url = LinkT1819, destfile = "SP1.1819.csv", mode = "wb")
download.file(url = LinkT1718, destfile = "SP1.1718.csv", mode = "wb")

#Se vizualizan los datos de la carpeta donde se encuentra el script
dir()

#Se selecciona los archivos csv
LigaEspanola <- lapply(dir()[9:11] , read.csv) 
#Si tienes m�s archivos en tu carpeta, tienes que colorar indices en el dir()[a:b]

#2. Las estructuras de de los datas frames al usar las funciones

#Se visualiza informaci�n sobre los dataframes importados previamente
str(LigaEspanola)
View(LigaEspanola)
summary(LigaEspanola)
head(LigaEspanola)

#3. Selecci�n de   las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR;
#esto para cada uno de los data.

#Selecci�n en cada uno de los archivos
LigaEspanola <- lapply(LigaEspanola, select, Date, HomeTeam:FTR)

#4. Aseg�rate de que los elementos de las columnas correspondientes de los nuevos 
#data frames sean del mismo tipo.

#Se unen los datos seleccionados
DatosLE <- do.call(rbind, LigaEspanola)
#Se cambia a tipo fecha la columna Date
DatosLE <- mutate(DatosLE, Date = as.Date(Date, "%d/%m/%Y"))
str(DatosLE)

#-------------------------------------------------------------------------------

#Postwork 3 - An�lisis Exploratorio de Datos (AED o EDA) con R

#Objetivo: Realizar descarga de archivos desde internet, generar nuevos
#data frames y visualizar probabilidades estimadas con la ayuda de gr�ficas


#1. Con el �ltimo data frame obtenido en el postwork de la sesi�n 2, elabora 
#tablas de frecuencias relativas para estimar las siguientes probabilidades: 
#Probabilidad marginal equipo de casa, probabilidad marginal de equipo visitante,
#probabilida conjunta.

#Se obtiene la probabilidad conjunta
ProbConjuntaLE <- prop.table(table(DatosLE$FTHG,DatosLE$FTAG))
#Se cambia los nombres de las filas y columnas
colnames(ProbConjuntaLE) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles", "6 goles")
rownames(ProbConjuntaLE) <- c("0 goles", "1 gol", "2 goles", "3 goles", "4 goles", "5 goles", "6 goles","7 goles","8 goles")
#Se visualiza
print('Probabilidad conjunta')
round(ProbConjuntaLE,5)


#Se obtiene la probabilidad marginal de cada variable
ProbCasaLE<- prop.table(apply(ProbConjuntaLE,MARGIN = 1,sum))
ProbVisitanteLE<- prop.table( apply(ProbConjuntaLE,MARGIN = 2,sum))
#Se juntan los datos en un s�lo dataframe y se visualiza
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
        xlab = "Numero de Goles", ylab = "Probabilidad", col = "#85c1e9")

heatmap(ProbConjuntaLE,
        scale = "none",
        Rowv = NA, Colv = NA, 
        col = cm.colors(256),
        xlab="Goles del Equipo Visitante", ylab="Goles del Equipo en Casa",
        main = "Probabilidad conjunta del Marcador de Goles")

#-------------------------------------------------------------------------------

#Postwork 4 - Algunas distribuciones, teorema central del l�mite y contraste de hip�tesis

#Objetivo: Investigar la dependencia o independecia de las variables aleatorias 
#X y Y, el n�mero de goles anotados por el equipo de casa y el n�mero de goles
#anotados por el equipo visitante.


#1. Obt�n una tabla de cocientes al dividir estas probabilidades conjuntas por
#el producto de las probabilidades marginales correspondientes.

#Multiplicaci�n de las probabilidades marginales P(A)P(B)
ProductoProbMarg <- t(ProbVisitanteLE %*% t(ProbCasaLE))
#Cociente de la probabilidad conjunta entre la marginal
ProbCo <- round(ProbConjuntaLE/ ProductoProbMarg,5)
ProbCo

#Promedio de la matriz
mean(ProbCo) 

#2.Mediante un procedimiento de boostrap, obt�n m�s cocientes similares a los 
#obtenidos en la tabla del punto anterior. Esto para tener una idea de las 
#distribuciones de la cual vienen los cocientes en la tabla anterior. Menciona 
#en cu�les casos le parece razonable suponer que los cocientes de la tabla en 
#el punto 1, son iguales a 1 (en tal caso tendr�amos independencia de las 
#variables aleatorias X y Y).

#Funci�n que realiza el proceso de obtener las proabilidades marginales, conjunta y la divisi�n entre ellas. 
#Regresa como resultado el media dada una muestra

Comprobacion <- function(x,y){
  
  ProbConjunta <- prop.table(table(x,y))
  ProbMargX<- prop.table(apply(ProbConjunta,MARGIN = 1,sum))
  ProbMargY<- prop.table(apply(ProbConjunta,MARGIN = 2,sum))
  
  ProbCo <- ProbConjunta/t(ProbMargY %*% t(ProbMargX))
  
  return(round(mean(ProbCo),3))
}

#Semilla para el muestreo bootstraps
set.seed(33421)
Nmuestras <- 1000 # N�mero de muestras bootstrap
#muestras
DatosLE_boot <- bootstraps(DatosLE, times = Nmuestras)


#Obtiene un vector con las medias de la comprobaci�n todas las muestras realizadas por el bootstrap
medias <- c()
for(i in 1:Nmuestras){
  muestra <- as.data.frame(DatosLE_boot$splits[[i]])
  med <- Comprobacion(muestra$FTAG,muestra$FTHG)
  medias <- c(medias,med)
}

print(paste("La media es",mean(medias),'y a desviaci�n estandar es',sd(medias)))

#Se grafica la distribuci�n de medias
ggplot() + 
  geom_histogram(aes(medias)) + 
  geom_vline(aes(xintercept = mean(medias))) +
  ggtitle('Distribuci�n de las medias muestrales.')

t.test(medias, alternative='two.sided',
       conf.level=0.95, mu=1)

#-------------------------------------------------------------------------------

#Postwork 5 - Regresi�n lineal y clasificaci�n

#Objetivo: Realizar predicciones de los resultados de partidos para una 
#fecha determinada y entender como una variable de respuesta est� relacionada
#con varias variables explicativas.


#1. A partir del conjunto de datos de soccer de la liga espa�ola de las temporadas 
#2017/2018, 2018/2019 y 2019/2020, crea el data frame SmallData, que contenga las
#columnas date, home.team, home.score, away.team y away.score; Con ayuda de la funci�n
#write.csv guarda el data frame como un archivo csv con nombre soccer.csv. 

#Links de los datos a descargar
LinkT1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
LinkT1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
LinkT1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"

Links <- list(LinkT1718,LinkT1819,LinkT1920)

#Se crea un dataframe vac�o para a�adir los datos de interes
SmallData <- data.frame(date = character(),
                        home.team = character(),
                        home.score = numeric(),
                        away.team  = character(),
                        away.score = numeric())


#Se descarga cada csv, se a�ada al dataframe
for(i in 1:length(Links)){
  D <- read.csv(Links[[i]])
  N <- select(D, date = Date, home.team = HomeTeam,home.score = FTHG, away.team = AwayTeam, away.score = FTAG)
  SmallData <- rbind(SmallData,N)
  print(paste('Dimensi�n con',i,'fichero cargado:',dim(SmallData)[1]))
}

#Se tranforma la columna date a tipo fecha
SmallData <- mutate(SmallData,date= as.Date(SmallData$date, format = "%d/%m/%y"))
str(SmallData)

#Se guardan los datos en un csv
write.csv(SmallData, file="soccer.csv", row.names = FALSE)

#2. Con la funci�n create.fbRanks.dataframes del paquete fbRanks importe el 
#archivo soccer.csv a R y al mismo tiempo asignelo a una variable llamada 
#listasoccer. Se crear� una lista con los elementos scores y teams que son 
#data frames listos para la funci�n rank.teams. Asigna estos data frames a 
#variables llamadas anotaciones y equipos.


#Se cargarn los datos
listasoccer <- create.fbRanks.dataframes("soccer.csv")
str(listasoccer)

#Se guardan los datos que utilizaremos para la predicci�n
anotaciones <- listasoccer$scores
equipos <- listasoccer$teams

#3. Con ayuda de la funci�n unique crea un vector de fechas (fecha) que no se
#repitan y que correspondan a las fechas en las que se jugaron partidos. Crea 
#una variable llamada n que contenga el n�mero de fechas diferentes. Posteriormente,
#con la funci�n rank.teams y usando como argumentos los data frames anotaciones 
#y equipos, crea un ranking de equipos usando �nicamente datos desde la fecha 
#inicial y hasta la pen�ltima fecha en la que se jugaron partidos, estas fechas
#las deber� especificar en max.date y min.date. Guarda los resultados con el 
#nombre ranking.


#Se crea un vector de fechas
fechas <- unique(listasoccer$scores$date) #Ya est�n en orden
n <- length(fechas)
print(paste('Existen',n,'fechas diferentes'))


#Se obtiene la primera y ultima fecha
rango <- fechas[c(1,n-1)]
print(paste('Existe una diferencia de',diff(rango), 'entre',fechas[1],'y',fechas[n-1]))


#Se crea un ranking 
ranking <- rank.teams(scores=anotaciones, teams = equipos, max.date = rango[2], min.date = rango[1])

#4. Finalmente estima las probabilidades de los eventos, el equipo de casa 
#gana, el equipo visitante gana o el resultado es un empate para los partidos 
#que se jugaron en la �ltima fecha del vector de fechas fecha. Esto lo puedes 
#hacer con ayuda de la funci�n predict y usando como argumentos ranking y 
#fecha[n] que deber� especificar en date

#Se hace una predicci�n de la ultima fecha
prediccion <- predict.fbRanks(ranking,date = fechas[n])

#-------------------------------------------------------------------------------

#Postwork 6 - Series de tiempo

#Objetivo: Crear y manejar series de tiempo en R para determinar patrones.

#Importa el conjunto de datos match.data.csv a R


#Se descargan los datos
LinkDatos <- 'https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-06/Postwork/match.data.csv'
DatosMatch <- read.csv(LinkDatos)

#Se cambia la columna date a tipo fecha
DatosMatch <- mutate(DatosMatch,date= as.Date(DatosMatch$date, format = "%Y-%m-%d") )
str(DatosMatch)

#1. Agrega una nueva columna sumagoles que contenga la suma de 
#goles por partido.

#Se a�ada una columna con la suma de los goles anotados por los dos equipos
DatosMatch<- mutate(DatosMatch,total.goles=DatosMatch$home.score+DatosMatch$away.score)
head(DatosMatch)

#2. Obt�n el promedio por mes de la suma de goles.

#Se hace una agrupaci�n por a�o y mes
goles<- DatosMatch %>% 
  group_by( Yr =year(date),Mn = month(date)) %>% 
  summarise(mean = mean(total.goles))

#3. Crea la serie de tiempo del promedio por mes de la suma de goles hasta 
#diciembre de 2019.

#Se crea una serie de tiempo
SerieGolM <- ts(goles, st= c(2010,8), end = c(2019,12), fr = 12)

#4. Grafica la serie de tiempo.

#Se grafica la serie de tiempo
plot(SerieGolM, xlab = "Tiempo", ylab = "Promedio de goles", main = "Serie del promedio de goles de la Liga Espa�ola",
     sub = "Agrupaci�n mensual: Agosto de 2010 a Diciembre de 2019")

#-------------------------------------------------------------------------------

#PostWork 7 - RStudio Cloud - Github, conexiones con BDs y lectura de datos externos

#Objeivo: Realizar el alojamiento de un fichero .csv a una base de datos (BDD),
#en un local host de Mongodb a trav�s de R

#Utilizando el manejador de BDD Mongodb Compass
#1. Alojar el fichero match.data.csv en una base de datos llamada match_games, 
#nombrando al collection como match

#Se hace una conexi�n con la base de datos
Bdconect <- mongo(collection='match',
                  db='match_games',
                  url = "",
                  verbose = FALSE)

#Se descarga los datos
LinkM <-'https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-07/Postwork/match.data.csv'
DatosMATCH <-read.csv(LinkM)

#Se inserta los datos a la base de datos
Bdconect$insert(DatosMATCH)

#2. Una vez hecho esto, realizar un count para conocer el n�mero de registros
#que se tiene en la base

#Se cuentan el n�mero de documentos
NDoc <- Bdconect$count('{}')

print(paste('Se guardaron',NDoc,'documentos'))

#3. Realiza una consulta utilizando la sintaxis de Mongodb en la base de datos,
#para conocer el n�mero de goles que meti� el Real Madrid el 20 de diciembre de
#2015 y contra que equipo jug�, �perdi� � fue goleada?

#Se hace una consulta
consulta <- Bdconect$find(query = '{"date" : "2015-12-20", "home_team" : "Real Madrid" }')
print(consulta)
#Real madrid contra Vallecano y Gan� Real madrid 

#4. Por �ltimo, no olvides cerrar la conexi�n con la BDD

#Se cierra la conexi�n
rm(Bdconect)

#-------------------------------------------------------------------------------

#Postwork 8 - Dashboards con Shiny - Entorno GUI

#Objetivo: Observar el resultado de la toma de desiciones consecutivas, cuando 
#estas se basan en datos hist�ricos y generar dashboards que muestren
#informaci�n de an�lisis, ya sea en forma de tablas y gr�ficas


setwd("D:/Program Files/RStudio/TRY1/www")
pData<-read.csv("match.data.csv")
choiceV<- names(pData)

ui<- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Postwork 8"),
    
    dashboardSidebar(
      
      sidebarMenu(
        
        menuItem("Data Table",tabName = "data_table", icon = icon("table")),
        menuItem("Postwork 3", tabName = "img",icon = icon("file-picture-o")),
        menuItem("Graficas de Barras", tabName = "Dashboard", icon = icon("dashboard")),
        menuItem("Frecuencias Relativas", tabName = "FR", icon = icon("file-picture-o"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem( tabName = "data_table",
                 fluidRow(
                   titlePanel(h5("Data Table")),
                   dataTableOutput("data_table")
                 )),
        tabItem(tabName = "img",
                titlePanel(h5("Imagenes")),
                img(src="PW31.png",
                    height=400,
                    width=400),
                img(src="PW33.png",
                    height=400,
                    width=400)
        ),
        tabItem(tabName = "Dashboard",
                fluidRow(
                  titlePanel(h3("Gráficos de barras")),
                  selectInput("x","Seleccionar variable X",
                              choices = c(choiceV[3],choiceV[5])),
                  plotOutput("plot1", height = 400, width = 700)
                )),
        tabItem(tabName = "FR",
                titlePanel(h4("Gráficos del código momios")),
                img(src="MMS1.png", height=400, width=700),
                img(src="MMS2.png", height=400, width=700))
      )
    )
  )
)


server<- function(input, output){
  output$data_table<-renderDataTable({pData},
                                     options=list(aLengthMenu=c(20,50,80),
                                                  iDisplayLength=10)
  )
  output$plot1<-renderPlot({
    x<-pData[,input$x]
    pData%>% ggplot(aes(x))+
      geom_bar()+
      facet_wrap("pData$away.team")+
      labs(x=input$x, y="goles")+
      ylim(0,100)
  })
  
}
shinyApp(ui,server)
