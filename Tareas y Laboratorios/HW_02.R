#Marlen Gutierrez Barrientos 
#31/08/2022
#Tarea 2


# IMPORTAR BASE DE DATOS --------------------------------------------------

conjunto <- read.csv ("cuadro1.csv", header=T)


# SELECCION DE DATOS ------------------------------------------------------

#Aplicar la funcion Subset oara la variable Altura 

#Incluir datos iguales o menores a la media
H.media <- subset(conjunto$Altura,conjunto$Altura <= mean(conjunto$Altura))

#Incluir los datos menores a 16.5m
H.16 <- subset(conjunto$Altura,conjunto$Altura < 16.5)


#Aplicar la funcion subset para la variable Vecinos

#Incluir los árboles que tengan un número de vecinos iguales o menores a 3
Vecinos_3 <- subset(conjunto$Vecinos,conjunto$Vecinos <= 3)

#Incluir un número de vecinos mayores a 4
Vecinos_4 <- subset(conjunto$Vecinos,conjunto$Vecinos > 4)


#Aplicar la funcion subset para la variable Diametro

#Incluir los diámetros menores a la media

DBH_media <- subset(conjunto$Diametro,conjunto$Diametro < mean(conjunto$Diametro))

#Incluir los diámetros mayores a 16

DBH_16 <- subset(conjunto$Diametro,conjunto$Diametro > 16)


#Aplicar la funcion subset para la variable Especie

#Inlcuir la especie Cedro Rojo

Especie_C <- subset(conjunto$Especie,conjunto$Especie == "C")

#Incluir la especie Tsuga heterófila y Douglasia verde

Especie_FH <- subset(conjunto$Especie,conjunto$Especie == conjunto$Especie [c(1,4)]) 
#Determinar cuantas observaciones son menores o iguales a 16.9 cm de Diametro

DBH_16.9 <- subset(conjunto$Diametro,conjunto$Diametro <= 16.9)

#Determinar cuantas observaciones son mayores a 18.5 metros de Altura

H_18.5 <- subset(conjunto$Altura,conjunto$Altura > 18.5)


# VISUALIZACION DE DATOS --------------------------------------------------

#Con la funcion hist generar los histogramas para los objetos creados en el apartado anterior

#Altura

hist(conjunto$Altura, main = "Altura", xlab = "Datos de Altura", ylab = "Frecuencia",
     las = 1, col = "lightblue")

#H.media

hist(H.media, main = "H.media", xlab = "Datos de Altura mayor igual a media",
     ylab = "Frecuencia", las = 1, col = "#FFF0F5")

#H.16

hist(H.16, main = "H.16", xlab = "Datos de Altura menores a 16.5 metros",
     ylab = "Frecuencia", las = 1, col = "lightsteelblue1")


#Vecinos

hist(conjunto$Vecinos, main = "Vecinos", xlab = "Datos de Vecinos",
     ylab = "Frecuencia", las = 1, col = "lightsalmon1")

#Vecinos_3

hist(Vecinos_3, main = "Vecinos 3", xlab = "Vecinos iguales o menores a 3", 
     ylab = "Frecuencia", las = 1, col = "bisque1")

#Vecinos_4

hist(Vecinos_4, main = "Vecinos 4", xlab = "Vecinos mayores a 4",
     ylab = "Frecuencia", las = 1, col = "aquamarine1")


#Diametro

hist(conjunto$Diametro, main = "Diametro", xlab = "Datos de Diametro",
     ylab = "Frecuencia", las = 1, col = "pink")

#DBH_media

hist(DBH_media, main = "DBH_media", xlab = "Datos de Diametro menores a la Media",
     ylab = "Frecuencia", las = 1, col = "red")

#DBH_16

hist(DBH_16, main = "DBH_16", xlab = "Datos de Diametro mayores a 16",
     ylab = "Frecuencia", las = 1, col = "blue")



# ESTADISTICAS BASICAS ----------------------------------------------------

#Determinar la media (mean) de los objetos (variable y respectivos subsets), asi como su desviación estándar (sd).

#Altura,H.media y H.16

mean(conjunto$Altura); mean(H.media); mean(H.16)
sd(conjunto$Altura); sd(H.media); sd(H.16)

#Vecinos,Vecinos_3,Vecinos_4

mean(conjunto$Vecinos); mean(Vecinos_3); mean(Vecinos_4)
sd(conjunto$Vecinos); sd(Vecinos_3); sd(Vecinos_4)

#Diametro,DBH_media,DBH_16

mean(conjunto$Diametro); mean(DBH_media); mean(DBH_16)
sd(conjunto$Diametro); sd(DBH_media); sd(DBH_16)














