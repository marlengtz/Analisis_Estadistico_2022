#Clase 3
#Marlen Gutierrez Barrientos
#23/08/2022

# Importar datos csv ------------------------------------------------------

est <- read.csv("Clases/cumbres.csv", header = T) #se usa para decir que la primera fila tiene los nombres
head(est) #Revisar los primeros seis datos
tail(est) #Revisar los ultimos seis datos

viv <- read.csv("Clases/vivero.csv", header = T)

boxplot(viv$IE ~ viv$Tratamiento)




viv$Tratamiento <- as.factor(viv$Tratamiento)

boxplot(viv$IE ~ viv$Tratamiento)
summary(viv)