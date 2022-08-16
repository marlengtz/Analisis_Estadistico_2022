# Marlen Gutierrez Barientos
#16/08/2022
#Clase 2

# Ingresar Datos ----------------------------------------------------------

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7,
         11.2, 24.1, 14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5,
         7.8, 27.3, 9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5,
         14.3, 17.2, 16.8) #diametro a la altura del pecho# 

length(dbh) #longitud de dbh
# Medidas de tendencia central

media <- sum(dbh)/length(dbh)
mean(dbh) #media
median(dbh)

# Media geométrica
exp(mean(log(dbh)))

# Medidas de dispersión

sd(dbh) #Desviacion estandar
var(dbh) #Varianza

#coeficiente de variacion
coef.var <- sd(dbh)/mean(dbh)*100
coef.var

fivenum(dbh) #da 5 numeros min, max, mediana, 1 cuartil 15% y 4 cuartil 75%

quantile(dbh,0.15)
quantile(dbh, 0.30)


# Gráfica -----------------------------------------------------------------

boxplot(dbh, col= "lightgreen", ylab= "Diámetro (cm)",
        xlab= "Sitio 1", main= "Parcela BE")

# Para poner la grafica en horizontal boxplot(dbh, horizontal = T)

hist(dbh, col= "lightgreen", ylab= "Frecuencia",
     xlab= "Diámetro (cm)",
     main = "Sitio BE", ylim= c(0,10))

#Grafica de tallo y hoja
stem(dbh, scale =2)

dbh50 <- sample(dbh,10, replace = TRUE)

set.seed (45)
dbh50 <- rnorm(50, mean = 10, sd=2)
hist(dbh50)
dbh10 <- rnorm(10, mean = 10, sd=2)
hist(dbh10)
dbh100 <- rnorm(100, mean = 10, sd=2)
hist(dbh100)
dbh1000 <- rnorm(1000, mean = 10, sd=2)
hist(dbh1000)