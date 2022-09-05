#Marlen Gutierrez Barrientos 
#3/09/2022
#Tarea 4 BOX PLOT E HISTOGRAMAS 


# PROBLEMA 1 -------------------------------------------------------------

set.seed(9875)
size <- 1000
x2 <- round(runif(n = size, min = 0, max = 10), 2)

h1 <- hist(x2, las =1, col = "blue")
h1
h1$breaks

h1 <- hist(x2, xaxt = "n",
           breaks = c(0, 2, 4, 6, 8, 10),
           col = "orange")
axis(1,h1$mids)

h2 <- hist(x2, xaxt = "n",
           breaks = c(0, 1, 2, 4, 7, 10),
           col = "purple1")
axis(1,h2$mids)


# PROBLEMA 2 --------------------------------------------------------------

#a.¿Cuál distribución parece estar sesgada a la derecha? A
#b.¿Cuál distribución parece estar sesgada a la izquierda? D
#c.¿Cuál distribución parece ser simétrica o en forma de "campana"? C
#d.¿Cuál distribución parece ser bimodal? B
#e.¿Cuál distribución parece mostrar una falta de intervalos? C


# PROBLEMA 3 --------------------------------------------------------------

data(quakes)
tabla <- table(quakes$mag)

mags <- hist(quakes$mag, xaxt = "n",
             # breaks = c(en caso de necesitar aqui se puede especificar),
             col = "#e6ac00", xlab = "Magnitud de los terremotos",
             ylab = "Frecuencias",
             main = "",
             las = 1,
             ylim = c(0,260))
axis(1, mags$mids)

#a.¿Cómo describiría la forma de esta distribución de las magnitudes de los terremotos? SESGADA A LA DERECHA
#b.Mencione un intervalo donde ocurren tipicamente las magnitudes. 4.4 - 4.6
#c.Determine el rango de las magnitudes(Range = Max - Min).
Ran = max(quakes$mag)- min(quakes$mag)

#d.¿Qué porcentaje de los terremotos ocurren con magnitud en la clase 5.3 (5.1:5.4)?
library(dplyr)

magn <- quakes%>% 
filter(mag == "5.3")%>% 
select(mag,stations)

porcent_5.3 <- (length(magn$mag)/length(quakes$mag))*100
porcent_5.3

#e.¿Qué porcentaje de los terremotos tiene una magnitud igual o mayor a 5.0?
magn2 <- quakes%>% 
  filter(mag >="5")%>% 
  select(mag,stations)
magn2

porcent_5.0 <- (length(magn2$mag)/length(quakes$mag))*100
porcent_5.0


#f.¿Qué porcentaje de los terremotos tienen una magnitud menor o igual a 4.6?

magn3 <- quakes%>%
  filter(mag <="4.6")%>%
  select(mag,stations)
magn3

porcent_4.6 <- (length(magn3$mag)/length(quakes$mag))*100
porcent_4.6


# PROBLEMA 4 --------------------------------------------------------------

#¿Qué porcentaje de las observaciones en una distribución se encuentran entre el primer y el tercer cuartil?
#b


# PROBLEMA 5 --------------------------------------------------------------

#a.¿Cuál especie tiene el diametro mas pequeño? C 
#b.¿Cuál especie tiene el diametro mas grande? F
#c.¿Cuál especie tiene el diametro minimo mas alto? F
#d.¿Cuál especie tiene la mediana de diametro mas pequeña? C
#e.¿Cuál especie tiene la mediana de diametro mas grande? H
#f.¿Cuál especie tiene el menor rango de diametro? F
#g.¿cuál especie tiene el rango intercuartil (Q3-Q1) mas grande? C
#h.¿Cuál especie tiene el rango intercuartil (Q3-Q1) mas pequeño? F
#i.¿Cuál especie tiene una distribucion simetrica? NINGUNA
#j.¿Cuál especie tiene el sesgo positivo mas marcado? C


# PROBLEMA 6 --------------------------------------------------------------

fires <- c(78, 44, 47, 105, 126, 181, 277, 210, 155)
fires

#1 VALOR MINIMO
minimo <- min(fires)

#2 VALOR MAXIMO
maximo <- max(fires)

#3 RANGO
rango <- maximo - minimo

#4 Q1 (25%)
quantile(fires,0.25)

#5 Q2 (50%)
quantile(fires,0.50)

#6 Q3 (75%)
quantile(fires,0.75)

#7 MEDIA
mean(fires)

#8 VARIANZA
var(fires)

#9 DESVIACION ESTANDAR
sd(fires)

#10 REALIZE UN BOXPLOT 
boxplot(fires, main = "Incendios Forestales", col = "green")
data
