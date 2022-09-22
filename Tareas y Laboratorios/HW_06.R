#MARLEN GUTIERREZ BARRIENTOS
#TAREA 6
#21/09/2022
#REGRESION LINEAL


# EJERCICIO 1 -------------------------------------------------------------

erupc <- read.csv("erupciones.csv", header = T)
erupc

plot(erupc$eruptions, erupc$waiting,
     pch = 19, col = "pink",
     xlab = "Erupciones",
     ylab = "Tiempo de espera")

#ESTADISTICAS DESCRIPTIVAS

##MEDIA
mean(erupc$eruptions)
mean(erupc$waiting)

##DESVIACION ESTANDAR
sd(erupc$eruptions)
sd(erupc$waiting)

##VARIANZA
var(erupc$eruptions)
var(erupc$waiting)

##COEFICIENTE DE CORRELACION
erup.cor <- cor.test(erupc$eruptions, erupc$waiting)
erup.cor

# r= 0.9008112 
#¿Es significativa la correlacion? Si, entre ambas variables

##REGRESION LINEAL
#HO: No hay correlacion significativa entre la erupcion y el tiempo de espera
#H1: Hay correlacion significativa entre la erupcion y el tiempo de espera

erupc.lm <- lm(erupc$eruptions ~ erupc$waiting)
erupc.lm

#VALOR DEL INTERCEPTO ALFA
# α = -1.87402

#VALOR DE LA PENDIENTE
# β = 0.07563

summary(erupc.lm)

#ANALISIS DE REGRESION
#P-value= 0.00000000000000022

#SON SIGNIFICATIVAS LAS REGRESIONES: INTERCEPTO Y LA PENDIENTE
#Si, son altamente significativas

#DURACION EN MINUTOS DE LA PROXIMA ERUPCION 

#80
-1.874016 + (0.075628*80)

#40
-1.874016 + (0.075628*40)

#45
-1.874016 + (0.075628*45)

#53
-1.874016 + (0.075628*53)

#61
-1.874016 + (0.075628*61)
