#MARLEN GUTIERREZ BARRIENTOS
#LABORATORIO 5
#21/09/2022
#CORRELACION


# EJERCICIO 1- EL CUARTETO DE ANSCOMBE ------------------------------------

#Graficar un cuadro de 2x2
op = par(mfrow = c(2, 2), mar = c(4.5, 4, 1, 1))
plot(anscombe$x1, anscombe$y1, pch = 20)
plot(anscombe$x2, anscombe$y2, pch = 20)
plot(anscombe$x3, anscombe$y3, pch = 20)
plot(anscombe$x4, anscombe$y4, pch = 20)
par(op)

#COEFICIENTE DE CORRELACION

#Funcion lineal modem (lm)
conjdat1.lm <- lm(anscombe$y1 ~ anscombe$x1)

#Grafica de dispersion del conjunto de datos 1
plot(anscombe$x1, anscombe$y1, pch = 20)
abline(conjdat1.lm, col = "red")

#EXAMINAR LA RELACION QUE EXISTE ENTRE DOS MUESTRAS MEDIANTE UNA CORRELACION
#R= Es una relacion lineal simple

#EXPLORE LOS DATOS GRAFICAMENTE Y EXPLIQUE
#R= Es una correlacion positiva

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#R= HO: No hay correlacion significativa, H1= Hay correlacion significativa

#APLIQUE LA PRUEBA CORRESPONDIENTE

#PRUEBA DE SHAPIRO
shapiro.test(anscombe$x1)

#COEFICIENTES DE CORRELACION (R)
conjdat1 <- cor.test(anscombe$x1, anscombe$y1)
conjdat1

summary(conjdat1.lm)

#REPORTE LOS DATOS
# r= 0.8164205
# GRADOS DE LIBERTAD= 9
# P-VALUE= 0.00217
# HIPOTESIS ALTERNATIVA, LA CORRELACION ES SIGNIFICATIVA 

##DATOS 2

#Funcion lineal modem (lm)
conjdat2.lm <- lm(anscombe$y2 ~ anscombe$x2)

#Grafica de dispersion del conjunto de datos 2
plot(anscombe$x2, anscombe$y2, pch = 20)
abline(conjdat2.lm, col = "red")

#EXAMINAR LA RELACION QUE EXISTE ENTRE DOS MUESTRAS MEDIANTE UNA CORRELACION
#R= Es una relacion no lineal

#EXPLORE LOS DATOS GRAFICAMENTE Y EXPLIQUE
#R= Es una relacion no lineal

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#R= HO: No hay correlacion significativa, H1= Hay correlacion significativa

#APLIQUE LA PRUEBA CORRESPONDIENTE
#PRUEBA DE SHAPIRO
shapiro.test(anscombe$x2)

#COEFICIENTES DE CORRELACION (R)
conjdat2 <- cor.test(anscombe$x2, anscombe$y2)
conjdat2

summary(conjdat2.lm)

#REPORTE LOS DATOS
# r= 0.8162365
# GRADOS DE LIBERTAD= 9
# P-VALUE= 0.002179
# HIPOTESIS ALTERNATIVA, LA CORRELACION ES SIGNIFICATIVA 

##DATOS 3

#Funcion lineal modem (lm)
conjdat3.lm <- lm(anscombe$y3 ~ anscombe$x3)

#Grafica de dispersion del conjunto de datos 3
plot(anscombe$x3, anscombe$y3, pch = 20)
abline(conjdat3.lm, col = "red")

#EXAMINAR LA RELACION QUE EXISTE ENTRE DOS MUESTRAS MEDIANTE UNA CORRELACION
#R= Es una relacion lineal simple

#EXPLORE LOS DATOS GRAFICAMENTE Y EXPLIQUE
#R= Es una correlacion positiva

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#R= HO: No hay correlacion significativa, H1= Hay correlacion significativa

#APLIQUE LA PRUEBA CORRESPONDIENTE
#PRUEBA DE SHAPIRO
shapiro.test(anscombe$x3)

#COEFICIENTES DE CORRELACION (R)
conjdat3 <- cor.test(anscombe$x3, anscombe$y3)
conjdat3

summary(conjdat3.lm)

#REPORTE LOS DATOS
# r= 0.8162867
# GRADOS DE LIBERTAD= 9
# P-VALUE= 0.002176
# HIPOTESIS ALTERNATIVA, LA CORRELACION ES SIGNIFICATIVA 

##Datos 4

#Funcion lineal modem (lm)
conjdat4.lm <- lm(anscombe$y4 ~ anscombe$x4)

#Grafica de dispersion del conjunto de datos 4
plot(anscombe$x4, anscombe$y4, pch = 20)
abline(conjdat4.lm, col = "red")

#EXAMINAR LA RELACION QUE EXISTE ENTRE DOS MUESTRAS MEDIANTE UNA CORRELACION
#R= Es una relacion no lineal

#EXPLORE LOS DATOS GRAFICAMENTE Y EXPLIQUE
#R= Es una relacion no lineal

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#R= HO: No hay correlacion significativa, H1= Hay correlacion significativa

#APLIQUE LA PRUEBA CORRESPONDIENTE
#PRUEBA DE SHAPIRO
shapiro.test(anscombe$x4)

#COEFICIENTES DE CORRELACION (R)
conjdat4 <- cor.test(anscombe$x4, anscombe$y4)
conjdat4

summary(conjdat4.lm)

#REPORTE LOS DATOS
# r= 0.8165214
# GRADOS DE LIBERTAD= 9
# P-VALUE= 0.002165
# HIPOTESIS ALTERNATIVA, LA CORRELACION ES SIGNIFICATIVA 





