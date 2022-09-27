#Marlen Gutierrez Barrientos
#CLASE 7
#ANALISIS DE VARIANZA 
#27/09/2022


# DATOS -------------------------------------------------------------------

#HO:No hay diferencia significativa entre la avena y los tres tratamientos de suelo
#H1: Si hay diferencia significativa entre la avena y los tres tratamientos de suelo

arena <- c(6, 10, 8, 6, 14, 17, 9, 11, 7, 11)
arcilla <- c(17, 15, 3, 11, 14, 12, 12, 8, 10, 13)
limo <- c(13, 16, 9, 12, 15, 16, 17, 13, 18, 14)

prod <- c(arena, arcilla, limo)

suelo <- gl(3,10,30, labels = c("arena", "arcilla", "limo")) #gl para generar niveles

avena <- data.frame(suelo, prod)


# FUNCION tapply ----------------------------------------------------------

tapply(avena$prod, avena$suelo, length)
tapply(avena$prod, avena$suelo, mean)
tapply(avena$prod, avena$suelo, sd)
tapply(avena$prod, avena$suelo, var)

#Revisar la homogeneidad de las varianzas

bartlett.test(avena$prod, avena$suelo)
#NO HAY DIFERENCIAS SIGNIFICATIVAS PORQUE EL VALOR ES MAYOR A 0.05

fligner.test(avena$prod, avena$suelo)
#NO HAY DIFERENCIA SIGNIFICATIVA (VALOR DE P-VALUE)

#Revisar los datos de forma gráfica 

boxplot(avena$prod ~ avena$suelo)

#La arcilla tiene un valor outlier
#De acuerdo con el boxplot se ven diferencias significativas entre los suelos arenoso y limoso, pero entre arcilla y limo no hay ni arena-arcilla

#Si la varianza del error es mas grande que la del tratamiento no hay diferencia significativa 
#si la varianza del error es mas pequeña que la del tratamiento hay diferencias significativa 
#EL ANALISIS DE VARIANZA NO TE DICE CUAL ES DIFERENTE A CUAL SOLAMENTE SI HAY DIFERENCIAS 

#Suma de cuadrados total
SST <- sum((avena$prod - mean(avena$prod))^2)

#Suam de cuadrados del error
arena - mean(arena)
arcilla - mean(arcilla)
limo - mean(limo)

arena.sum <- sum((arena -mean(arena))^2)
arcilla.sum <- sum((arcilla -mean(arcilla))^2)
limo.sum <- sum((limo -mean(limo))^2)

SSe <- arena.sum + arcilla.sum + limo.sum
SSe

#Suma total del tratamiento
SStr <- SST - SSe
SStr

#Cuadrado medio
#Es la suma de la varianza 

tapply(avena$prod, avena$suelo, var)
CME <- mean(tapply(avena$prod, avena$suelo, var))
CMtr <- SStr/2
Fcal <- CMtr/CME
Fcal

#Calcular F tabulada
Ftab <- qf(0.95, 2, 27)
Ftab

#Cuando el valor de F es menor que el tabulado no hay diferencia significativa 
#Cuando el valor de F calculado es mayor que el F tabulado hay diferencia significativa

probF <- 1-pf(Fcal,2,27) #Grados de libertad, valor del denominador y numerador
probF


#ANOVA procedimiento simplificado

avena.aov <- aov(avena$prod ~ avena$suelo) #Nos da la tabla de resultados
summary(avena.aov)

par(mfrow=c(2,2))
plot(aov(avena$prod ~ avena$suelo))
par(mfrow=c(1,1))

#LA PRUEBA DE TUKEY NOS DICE SI HAY DIFERENCIAS SIGNIFICATIVAS 

TukeyHSD(avena.aov, conf.level = 0.95) 
#En el valor de p adj nos dice si hay diferencias significativas

plot(TukeyHSD(avena.aov)) #Se grafica y si toca la linea de cero no hay diferencias

#SI NO HAY DIFERENCIAS SIGNIFICATIVAS NO HAY QUE USAR TUKEY 









