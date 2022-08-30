#Marlen Gutierrez Barrientos 
#30/08/2022
#Tarea 1
 

# PROBLEMA #1 -------------------------------------------------------------

#Un investigador realiza un inventario de la superﬁcie reforestada por especie en la región centro mediante los reporte emitidos por CONAFOR. El investigador encuentra que la superﬁcie reforestada con diferentes especies son las siguientes:
  
Pinus <- 3140; Mezquite <- 145; Encinos <- 450;
Teka <- 1200; Juiperos <- 720

#crea un vector llamdado superficie

superficie <- c()
superficie <- c(Pinus, Mezquite, Encinos, Teka, Juiperos)


#Usa la funcion barplot para producir un diagrama de barras de la superficie por especie

barplot (superficie, main = "Superficies Reforestadas", ylab= "Frecuencia",
         names.arg= c("Pinus", "Mezquite", "Encinos", "Teka", "Juiperos"), 
         col = c("red","green","pink","darkcyan","blue"))


#Descubra como utilizar sort para ordenar los elementos en la variable superficie, con el fin de organizar elementos en gastos en orden creciente

s_creciente <- sort(superficie)

#barplot para grafico de barras en orden creciente

barplot (s_creciente, main = "Sort Creciente", ylab = "Frecuencia",
         names.arg= c("Pinus", "Mezquite", "Encinos", "Teka", "Juiperos"),
         col = "violet")

#sort para orden decreciente
s_decreciente <- sort (superficie, decreasing = T)

#barplot para grafico de barras en orden decreciente

barplot (s_decreciente, main = "Sort Decreciente", ylab = "Frecuencia",
         names.arg= c("Pinus", "Mezquite", "Encinos", "Teka", "Juiperos"),
         col = "turquoise1")

#determinar la media de la variable superficie

mean (superficie)


# PROBLEMA 2 --------------------------------------------------------------

#Un técnico examina 30 cajas de Petri en las que se colocaron para germinar seis semillas y después de cierto tiempo cuenta el número de semillas germinadas en cada una de ellas. Los valores de las 30 observaciones son los siguientes:

#Ingresar los datos en una variable llamada germinación
germinacion <- c(4, 1, 6, 2, 4, 2, 4, 2, 4, 6, 3, 5, 3, 2, 5, 4, 0, 5, 4, 
                 2, 4, 5, 3, 5, 3, 5, 4, 3, 6, 2)

#Determinar la media de germinación de las 30 cajas Petri
mean(germinacion)

#Averigue como determinar la desviación estándar 
help (sd)
sd(germinacion)


# PROBLEMA 3 --------------------------------------------------------------

#Una viverista mide la altura alcanzada por 25 plantas de Prosopis de un año de edad, obteniendo los siguientes valores:

#Ingresar los datos en una variable llamada altura

altura <- c(38, 14, 44, 11, 9, 21, 39, 28, 41, 4, 35, 24, 36, 12,
            20, 31, 24, 25, 10, 21, 11, 36, 37, 20, 26)

#Determinar la media de altura de las plántulas de Prosopis
mean (altura)

#Determinar la desviacíon estándar de la variable altura
sd(altura)



