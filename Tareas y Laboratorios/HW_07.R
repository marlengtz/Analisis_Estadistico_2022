#MARLEN GUTIERREZ BARRIENTOS
#TAREA 7 - COMPARACIÓN DE MEDIAS
#13/09/2022


# PROCEDIMIENTO GENERAL PARA LAS PRUEBAS DE t UNA MUESTRA -----------------

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94, 80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)

#Determinar el número de observaciones
n <- length(costal)
n

#Determinar la media
costa.media <- mean(costal)
costa.media

#Desviación estándar 
costa.sd <- sd(costal)
costa.sd

#El denominador se le conoce como error estándar de la media (se)
costa.se <- costa.sd/sqrt(n)
costa.se

#Entonces podemos calcular el valor de T
costa.T <- (costa.media - 80)/ costa.se
costa.T

#Finalmente el valor de (p) puede ser calculado
pt(costa.T, df = n-1) #Arriba se declaro "n" como número de costales observados

#Prueba de Shapiro
shapiro.test(costal)

#Prueba de Comparación de medias
t.test(costal, mu = 80)


# EJERCICIO 1 -------------------------------------------------------------

#¿Cuál es el valor de p? R= Shapiro=0.5815, t.test=0.02264
#¿Cuántos grados de libertad tiene el experimento? R= df=43
#¿Cuál es la hipótesis aceptada? R= H1-Hipótesis alternativa
#¿Existe evidencia de que el valor medio promedio de los costales observados es 
#menor (significativamente) a los que anuncia el producto? 
#R= Si al calcular la media el valor es menor que los 80kg que promete el producto,le faltan 1.08932 kg.


# EJERCICIO 2 -------------------------------------------------------------

azufre <- c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8,
            22.7, 15.2, 23.0, 29.6, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9, 
            24.6, 19.4, 12.3, 15.9, 11.2, 14.7, 20.5, 26.6, 20.1, 17.0,
            22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 13.3, 18.1)

naz <- length(azufre)
naz

azufre.media <- mean(azufre)
azufre.media

azufre.sd <- sd(azufre)
azufre.sd

#El denominador se le conoce como error estándar de la media (se).
azufre.se <- azufre.sd/ sqrt(naz)
azufre.se

#Entonces podemos calcular el valor de T
azufre.T <- (azufre.media - 17.5)/ azufre.se
azufre.T

#Finalmente, el valor de (p) puede ser calculado.
pt(azufre.T, df = naz-1)

#Prueba de Shapiro
shapiro.test(azufre)

#Prueba de comparacion de medias 
t.test(azufre, mu = 17.5)

#¿Cuál es el valor de p? R= shapiro=0.8654, t.test=0.1893
#¿Cuáles son los intervalos de confianza al 95%? R= 16.87912, 20.53588
#¿Cuántos grados de libertad tiene el experimento? R= df=39
#¿Cuál es la hipótesis aceptada? R= H1-Hipótesis alternativa
#¿Existe evidencia de que el valor medio promedio de las emisiones observadas es
#mayor (significativamente) a la declarada en los procedimientos de seguridad de la empresa? 
#R= Si es mayor, hay una diferencia a la permitida de 1.2075 toneladas de emisiones de óxido de azufre al año.



# EJERCICIO 3 -------------------------------------------------------------


temperaturas <- paste0("https://raw.githubusercontent.com/mgtagle/MCF-202_Agosto_2021/main/TEMPAIRE_DIA.csv")
temp <- read.csv(temperaturas)
head(temp)
temp

ntemp <- length(temp$temp_media)
ntemp

temp.media <- mean(temp$temp_media)
temp.media

temp.sd <- sd(temp$temp_media)
temp.sd

temp.se <- temp.sd/ sqrt(ntemp)
temp.se

#Entonces podemos calcular el valor de T
temp.T <- (temp.media - 24)/ temp.se
temp.T

#Finalmente, el valor de (p) puede ser calculado.
pt(temp.T, df = ntemp-1)

#Prueba de Shapiro
shapiro.test(temp$temp_media)

#Prueba de comparación de medias 
t.test(temp$temp_media, mu = 24)

#¿Cuál es el valor de p? R= Shapiro= 3.923e-14, t.test= 0.03615
#¿Cuáles son los intervalos de confianza al 95%? R= 23.28216 23.97599
#¿Cuántos grados de libertad tiene el experimento? R= df=845
#¿Cual es la hipótesis aceptada? R= H1-Hipótesis alternativa
#¿Existe evidencia de que el valor medio promedio de las emisiones observadas es
#mayor (significativamente) a la declarada en los procedimientos de seguridad de la empresa?
#R= Si, hay 0.37092 grados de diferencia.








