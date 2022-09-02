#Marlen Gutierrez Barrientos 
#31/08/2022
#Laboratorio 2


# PARTE 1- IMPORTAR DATOS -------------------------------------------------

#Importar desde archivos csv

trees <- read.csv("DBH_1.csv", header = TRUE)
head(trees)
getwd()

#Ingresar datos directo en la consola

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3, 
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)

#Accesar datos de internet

#Datos de URL no seguras (http)

prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url)
head(profepa)

prof_url_2 <- paste0("http://www.profepa.gob.mx/innovaportal/",
                     "file/7635/1/accionesInspeccionfoanp.csv")
profepa2 <- read.csv(prof_url_2)
head(profepa2)

#Datos URL seguras (https): Dropbox y Github

library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

head(conjunto) #muestra las primeras seis filas de la BD

library(readr)
file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")
inventario <- read_csv(file)
head(inventario)


# PARTE 2- OPERACIONES CON LA BASE DE DATOS --------------------------------

mean(trees$dbh)
sd(trees$dbh)

#Indica la sumatoria de los individuos en el objeto tree con un dbh menor (<) a 10
sum(trees$dbh<10)
which(trees$dbh<10)
trees.13 <- trees[!(trees$parcela=="2"),]
trees.13
trees.1 <- subset(trees, dbh <=10)
head(trees.1)
mean(trees$dbh)
mean(trees.1$dbh)


# PARTE 3- REPRESENTACIÓN GRÁFICA -----------------------------------------

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")
hist(mamiferos$total_sleep)
hist(mamiferos$total_sleep, #datos
     xlim = c(0,20), ylim = c(0,14), #cambiar los limites de x & y
     main = "Total de horas de sueño de las 39 especies", #cambiar titulo
     xlab = "Horas de sueño", #cambiar eje de las x
     ylab = "Frecuencia", #cambiar eje de y
     las = 1, #cambiar orientacion de y
     col = "#996600") #cambiar color de las barras

data("chickwts")
head(chickwts[c(1:2, 42:43, 62:64),])

feeds <- table(chickwts$feed)
feeds
barplot(feeds)
barplot(feeds[order(feeds,decreasing = TRUE)])
barplot(feeds[order(feeds, decreasing = F)], horiz = TRUE, las =1,
        col=colorRampPalette(c("goldenrod1","gold", "lightgoldenrod1"))(6),
        xlab = "Número de Pollos", main = "Frecuencias por tipos de alimentacion")



