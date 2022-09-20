#MARLEN GUTIERREZ BARRIENTOS
#CLASE 6
#20/09/2022
#CORRELACION


# IMPORTAR DATOS ----------------------------------------------------------

ebano <- read.csv("clases/ebanos.csv", header = T)
head(ebano)

plot(ebano$diametro, ebano$altura,
     pch=19, col= "red",
     xlab = "Diametro (cm)",
     ylab = "Altura (cm)") #pch cambia los simbolos
#Si los datos se comportan en forma ascendente es correlacion positiva

# CORRELACION DE PEARSON (r) ----------------------------------------------

cor.eb <- cor.test(ebano$diametro, ebano$altura)
cor.eb
#valor de p menor a 0.05 hay una correlacion significativa- cambio significativo si es mayor a 0.05 no hay correlacion es negativa

#elevar al cuadrado la correlacion nos dice con que tanto de confianza una variable depende de la otra en este caso un 67% la altura predice el diametro


# CANOPY ------------------------------------------------------------------

cnpy <- read.csv("Clases/canopy.csv", header = T)

plot(cnpy$Cnpy, cnpy$LAI4,
     xlab = "Apertura del dosel (%)",
     ylab = "IAF",
     pch= 19, col= "blue")

cor.test(cnpy$Cnpy, cnpy$LAI4)

plot(cnpy$Cnpy, cnpy$GLI,
     xlab = "Apertura del dosel (%)",
     ylab = "GLI",
     pch= 19, col= "red")

cor.test(cnpy$Cnpy, cnpy$GLI)

plot(cnpy$LAI4, cnpy$GLI,
     xlab = "LAI4",
     ylab = "GLI",
     pch= 19, col = "black")

cor.test(cnpy$LAI4, cnpy$GLI)

#Si los datos no vienen de una distribucion normal se usa el metodo de Kendall 
cor.test(cnpy$Cnpy, cnpy$LAI4, method = "kendall")

#para saber si los datos son normales 
shapiro.test(cnpy$Cnpy)
shapiro.test(cnpy$LAI4)
shapiro.test(cnpy$GLI)


# REGRESION ---------------------------------------------------------------


# REGRESION ENTRE CNPY VS LAI4 --------------------------------------------

#SI NO TIENE UNA ASOCIACION LINEAL NO PUEDO HACER REGRESION Y ESO SE COMPRUEBA CON LA CORRELACION 
#Funcion (lm) de lineal mode Modelo Lineal

cp.lm <- lm(cnpy$LAI4 ~ cnpy$Cnpy) #en este se pone primero la variable dependiente
cp.lm
#El intercepto es alfa  el valor que tiene (y) cuando (x) vale cero y el otro valor es para beta y tienen que cumplir lo de 0.05

cp.lm <- lm(cnpy$Cnpy ~ cnpy$LAI4) 
cp.lm #el resultado de esta esta equivocado porque se va hasta 50 por eso siempre va primero la variable dependiente

#abline para la linea de tendencia central
plot(cnpy$Cnpy, cnpy$LAI4,
      xlab= "apertura del dosel (%)",
      ylab="IAF",
      pch= 19, col= "blue")
abline(cp.lm, col = "red")
text(34, 1.5, "Y=2.67-0.04*x",pos=2)
summary(cp.lm)
#la sumatoria de todos los residuales elevados al cuadrado debe ser cero
#la regresion es significativa con el valor de p-value
#la regresion es significativa para alfa y beta se ve en los valores de intercepto y el otro si tiene 3 asteriscos es muy cercano a cero

#SUMA DE LOS RESIDUALES 
cp.lm$residuals
sum(cp.lm$residuals)

cnpy$Yprima <- cp.lm$fitted.values
cnpy$dif <- cnpy$LAI4- cnpy$Yprima
cnpy$residual <-cp.lm$residuals
sum(cnpy$residual**2)/38 #el 38 son los grados de libertad que tiene el experimento 
#los residuales elevados al cuadrado son la variabilidad y divididos entre los grados de libertad es la varianza del experimento
#suma de cuadrados del error es la variabilidad 

summary (cp.lm) #sirve 



      




