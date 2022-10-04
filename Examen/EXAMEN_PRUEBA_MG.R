#MARLEN GUTIERREZ BARRIENTOS
#EXAMEN PRUEBA
#04/10/2022

suelo <- read.csv("obs.csv")
head(suelo)

suelo$zone <- as.factor(suelo$zone)
suelo$wrb1 <- as.factor(suelo$wrb1)

#ACTIVIDAD 1
summary(suelo$Clay1)
summary(suelo$Clay2)
summary(suelo$Clay5)

#P1-¿Cual es la tendencia del contenido de Arcilla con respecto a la profundidad?
#R= La arcilla aumenta conforme a la profundidad  

#ACTIVIDAD 2
stem(suelo$Clay1, scale = 1)

#P2¿Los datos de contenido de arcilla siguen una distribución simétrica o con sesgo?
#R=Tiene sesgo a la izquierda 

#ACTIVIDAD 3
boxplot(suelo$Clay1)

#P3¿Existe evidencia de outliers?
#R= Si

#P4 ¿Puede identificar cuáles observaciones son mediante una simple restricción de datos? Pista: observe losvalores mediante el boxplot y haga la restricción
#R= Si, se crea un vector con la funcion subset 

restr <- subset(suelo$Clay1,suelo$Clay1 <65)
restr
boxplot(restr)


#ACTIVIDAD 4
mean(suelo$Clay1)

#P5¿Estime si el contenido de Arcilla promedio en los suelos tropicales de 30 % es significativamente diferente que la media observada en el campo experimental Tropenbos Cameroon Programme (TCP)?
#R=


#ACTIVIDAD 5
#P6¿Existe una relación positiva, negativa o para nada relacionados, entre los perfiles superior (Clay1) e inferior (Clay5) con el contenido de Arcilla?
#R= Relacion positiva
cor.test(suelo$Clay1, suelo$Clay5)


#P7¿La correlación es estadísticamente significativa?
#R= Si


#ACTIVIDAD 6
#P8¿Es posible determinar una ecuación significativa para predecir el comportamiento del contenido de arcilla en el perfil inferior Clay5?
#R= Si
#P9¿Cuál es la ecuación final para predecir el comportamiento del contenido de arcilla en el perfil más profundo (30-50 cm)?
#R= y= alfa + beta(x)

#P10¿Son ambos parámetros α y β significativos?
#R= Si, son significativos 
suelo.lm <- lm(suelo$Clay5 ~ suelo$Clay1)
suelo.lm 
summary(suelo.lm)

#P11¿Cuál es el porcentaje de varianza explicado por el método aplicado?
#R= 89% 


#ACTIVIDAD 7
#P12¿Existe una forma de identificar la variación entre las cuatro zonas que se encuentran en el estudio?
#R=Si

#P13 Realice una inspección visual del contenido de arcilla en el perfil 30-50 cm (Clay5) y las cuatro zonas(zone) presentes en el área de estudio. ¿Existen indicios de que las cuatro zonas son diferentes en cuanto al contenido de arcilla en el perfil de 30 a 50 cm.?
#R= Si hay diferencia en el contenido de arcillas en cada una de las zonas
plot(suelo$Clay5 ~ suelo$zone, col = "red")

#P14¿Observa alguna tendencia en los datos en las diferentes zonas?
#R=No hay tendencia de los datos en las diferentes zonas

by(suelo$Clay5, suelo$zone, summary)


#ACTIVIDAD 8 
#P15¿Existen diferencias significativas entre el contenido de arcilla del perfil 30-50 cm y las zonas del estudio?
#R= Si hay diferencias significativas entre el contenido de arcilla del perfil 30-50 cm y las zonas del estudio

suelo.aov <- aov(suelo$Clay5 ~ suelo$zone)
summary(suelo.aov)


#P16En caso de existir diferencias ¿Cuáles zonas son diferentes estadísticamente entre si en el contenido de arcilla en el perfil de 30-50 cm?
#R= En la zonas 1 y 2 no hay diferencias pero en todas las demás zonas si hay diferencias

TukeyHSD(suelo.aov, conf.level = 0.95) 
plot(TukeyHSD(suelo.aov))



