#Marlen Gutierrez Barrientos 
#5/09/2022
#Tarea 3 MEDIDAS DE TENDENCIA CENTRAL


# PROBLEMA 1 --------------------------------------------------------------

m1 <- matrix(c(1, 6, 1, 2, 4, 3, 3, 1, 4, 4, 3, 2), 3, 4)
row.names(m1) <- c ("i", "xi", "yi")
m1

i <- c(1, 2, 3, 4)
x <- c(6, 4, 1, 3)
y <- c(1, 3, 4, 2)

xi <- sum(x)
xiyi <- sum(x)*sum(y)

promxi <- prod(x)
promxiyi <- prod(x)*prod(y)
promxiyi2 <- (prod(x)^2)*(prod(y)^0.5)


# PROBLEMA 2 --------------------------------------------------------------

GrupoA <- c(80, 90, 90, 100)
GrupoA

GrupoB <- c(60, 65, 65, 70, 70, 70, 75, 75, 80, 80, 80, 80, 80, 85, 100)
GrupoB

#a: El grupo A debido a que es un grupo chico pero con altas alturas
#b

M_A <- mean(GrupoA)
M_A

M_B <- mean(GrupoB)
M_B

#El grupo A tiene la media de altura mas grande y si coincide con mi primera impresión


# PROBLEMA 3 --------------------------------------------------------------

exam_3 <- c(87, 72, 85)
s_ex <- sum(exam_3)
pts80 <- 4*80
calif4 <- pts80 - s_ex
calif4
#Jose necesita obtener un 76 de calificacion

exam_4 <- c(87, 72, 85, 76)
mean(exam_4)


# PROBLEMA 4 --------------------------------------------------------------

#B Hay un total de 110 niños en la ciudad 


# PROBLEMA 5 --------------------------------------------------------------

m2 <- matrix(c(5, 6, 7, 8, 9, 1, 3, 5, 3, 1), 5, 2)
colnames(m2) <- c("Germinaciones", "Cajas petri")
m2

#a)¿Qué tipo de gráfico podrías usar para visualizar estos datos?

#Diagrama boxplot
boxplot(m2) 

#Histograma
hist(m2)

#b)¿Cuál es la media?
mean(m2)

#c)¿Cuál es la mediana?
median(m2)


# PROBLEMA 6 --------------------------------------------------------------

set <- c(2, 2, 3, 6, 10)

#a) Calcule la moda, la mediana y la media
#MEDIANA
median(set)

#MEDIA
mean(set)

#MODA
moda <- unique(set)
moda[which.max(tabulate(match(set, moda)))]

#b) Suma 5 a cada uno de los valores de los datos. Calcule la moda, la mediana y la media.

set5 <- set+5

#MEDIANA
median(set5)

#MEDIA
mean(set5)

#MODA
moda2 <- unique(set5)
moda2[which.max(tabulate(match(set5,moda2)))]

#c)Compare los resulatdos de las partes (a) y (b). En general, ¿cómo crees que la moda, la mediana y la media se ven afectadas cuando se agrega la misma constante a cada valor de datos en un conjunto?
#Cuando agregas una constante alos datos, se muestra un cambio equivalente en la media, mediana y moda 

#d)Multiplique cada valor de los datos por 5. Calcule la moda, la mediana y la media.
set_M5 <- set*5

#MEDIANA
median(set_M5)

#MEDIA
mean(set_M5)

#MODA
moda3 <- unique(set_M5)
moda3[which.max(tabulate(match(set_M5,moda3)))]

#e)Compare los resultados de las partes (a) y (d). En general, ¿cómo crees que la moda, la mediana y la media se ven afectadas cuando cada valor de los datos en un conjunto se multiplica por la misma constante?
#Las variables se comportan igualmente a la modificacion que se les aplico.


# PROBLEMA 7 --------------------------------------------------------------

Digitos <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

#a)Enumere cinco digitos que tengan una mediana de 7 y una media de 7 (se permiten repeticioes). Encontrar un conjunto diferente de 5 digitos que tambien funcionen.

d7 <- c(9, 4, 7, 8, 7)

#MEDIANA
median(d7)

#MEDIA
mean(d7)

d5 <- c(4,9,9,6,7)


#MEDIANA
median(d5)

#MEDIA
mean(d5)

#b)Enumere cinco digitos que tengan una mediana de 7 y una media inferior a 7 (se permiten repeticiones). Da la media de tus 5 digitos. Encuentra un conjunto diferente de 5 digitos que funcione.

D7 <- c(1,1,7,7,9)

#MEDIANA
median(D7)

#MEDIA
median(D7)

D5 <- c(1,8,9,5,7)

#MEDIANA
median(D5)

#MEDIA
mean(D5)
