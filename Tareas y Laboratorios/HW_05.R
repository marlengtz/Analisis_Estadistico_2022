#MARLEN GUTIERREZ BARRIENTOS
#TAREA 5
#20/09/2022
#CORRELACION


# EJERCICIO 1 -------------------------------------------------------------

efimeras <- read.csv("efimeras.csv")

plot(efimeras$speed, efimeras$abundance,
     pch = 18, col = "blue",
     xlab = "Velocidad de la corriente",
     ylab = "Abundancia")

#¿Es estadisticamente significativa la correlación? R= SI

cor.efime <- cor.test(efimeras$speed, efimeras$abundance)
cor.efime

#EXPLIQUE LOS DATOS DEL GRAFICO: La abundancia de moscas es mayor conforme 
#aumenta la velocidad del arroyo 

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre la velocidad del arroyo y la abundancia de efímeras
#H1: Existe una correlacion positiva entre la velocidad del arroyo y la abundancia de efímeras


#DATOS:
#VALOR DE R:0.8441408
#GRADOS DE LIBERTAD:6
#SIGNIFICANCIA DE LA CORRELACION: Es una correlacion positiva, significativa
#VALOR DE P:0.008393
#La hipotesis aceptada es la alternativa

# EJERCICIO 2 -------------------------------------------------------------

suelo <- read.csv("suelo.csv")
suelo

plot(suelo$pH, suelo$N,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "N")

cor.sueloN <- cor.test(suelo$pH, suelo$N)
cor.sueloN

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Nitrogeno
#H1: Existe una correlacion positiva entre pH y Nitrogeno
#La hipotesis aceptada es la alternativa

plot(suelo$pH, suelo$Dens,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "Dens")

cor.sueloDens <- cor.test(suelo$pH, suelo$Dens)
cor.sueloDens

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Densidad
#H1: Existe una correlacion positiva entre pH y Densidad
#La hipotesis aceptada es la alternativa

plot(suelo$pH, suelo$P,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "P")

cor.sueloP <- cor.test(suelo$pH, suelo$P)
cor.sueloP

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Fosforo
#H1: Existe una correlacion positiva entre pH y Fosforo
#La hipotesis aceptada es la alternativa


plot(suelo$pH, suelo$Ca,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "Ca")

cor.sueloCa <- cor.test(suelo$pH, suelo$Ca)
cor.sueloCa

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Calcio
#H1: Existe una correlacion positiva entre pH y Calcio
#La hipotesis aceptada es la alternativa



plot(suelo$pH, suelo$Mg,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "Mg")

cor.sueloMg <- cor.test(suelo$pH, suelo$Mg)
cor.sueloMg

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Magnesio
#H1: Existe una correlacion positiva entre pH y Magnesio
#La hipotesis aceptada es la alternativa

plot(suelo$pH, suelo$K,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "K")

cor.sueloK <- cor.test(suelo$pH, suelo$K)
cor.sueloK

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Potasio
#H1: Existe una correlacion positiva entre pH y Potasio
#La hipotesis aceptada es la alternativa

plot(suelo$pH, suelo$Na,
     pch = 18, col = "blue",
     xlab = "pH",
     ylab = "Na")

cor.sueloNa <- cor.test(suelo$pH, suelo$Na)
cor.sueloNa

#ESTABLEZCA LA HIPOTESIS NULA Y LA HIPOTESIS ALTERNATIVA
#HO: No existe una correlacion entre pH y Sodio
#H1: Existe una correlacion positiva entre pH y Sodio
#La hipotesis aceptada es la alternativa

Dsuelo <- matrix(0,7,3)
colnames(Dsuelo) <- (c("CONJUNTO", "R", "VALOR DE P"))
row.names(Dsuelo) <- (c("1", "2", "3", "4", "5", "6", "7"))

conjunto <- c("pH - N", "pH - Dens", "pH - P", "pH - Ca", "pH - Mg", "pH - K", "pH - Na")
Dsuelo [, 1] <- conjunto

R <- c("0.636654", "-0.5890264", "0.5910303", "0.8086293", "-0.3957821", "0.5795727", "-0.693264")
Dsuelo[,2] <- R

VALORDEP <- c("0.00000149", "0.00001062", "0.00000974", "0.000000000003614", "0.005361", "0.00001585", "0.00000004724")
Dsuelo[,3] <- VALORDEP
Dsuelo 


