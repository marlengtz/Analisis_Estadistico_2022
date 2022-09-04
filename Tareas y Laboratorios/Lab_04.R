#Marlen Gutierrez Barrientos 
#3/09/2022
#LABORATORIO 4: RESUMIR DATOS GRAFICAMENTE- HISTOGRAMAS 

# IMPORTAR DATOS CSV ------------------------------------------------------


esp.url <- paste0("https://raw.githubusercontent.com/mgtagle/",
                  "PrincipiosEstadistica2021/main/cuadro1.csv")
inventario <- read.csv(esp.url)
head (inventario)

#Dimensiones (numero de filas y columnas)
dim(inventario)

#Nombre de las primeras cinco columnas
names(inventario[ ,1:5])

#Resumen estadistístico básico de las columnas- 3 a 5 columnas
summary(inventario[ ,3:5])

is.factor(inventario$Posicion)

inventario$Posicion <- factor(inventario$Posicion)
is.factor(inventario$Posicion)

summary(inventario[ ,3:5])


# TABLAS DE FRECUENCIA ----------------------------------------------------

freq_position <- table(inventario$Posicion)
freq_position

#Frecuencias relativas-expresar las frecuencias como proporciones

prop_position <- freq_position / sum(freq_position)
prop_position

#Expresar las proporciones como porcentajes se multiplica *100

perc_position = 100 * prop_position
perc_position


# GRAFICAS BARPLOT & PIE --------------------------------------------------

barplot(freq_position, las = 1, border = NA, cex.names = 0.7)

#Gráfico circular o pie

pie(freq_position, col=topo.colors(4)) #topo.colors es una paleta de colores pre establecidas en R

#Para mostrar frecuencias

pie(freq_position, col = topo.colors(4),
    labels = paste(levels(inventario$Posicion), round(perc_position, 2), "%"))


# AUTOESTUDIO -------------------------------------------------------------

freq_especie <- table(inventario$Especie)
freq_especie

prop_especie <- freq_especie / sum(freq_especie)
prop_especie

perc_especie = 100 * prop_especie
perc_especie

barplot(freq_especie, las = 1, border = NA, cex.names = 0.7)

pie(freq_especie, col=topo.colors(4))

pie(freq_especie, col = topo.colors(4),
    labels = paste(levels(inventario$Especie), round(perc_especie, 2), "%"))


# HISTOGRAMAS -------------------------------------------------------------

diam_hist <- hist(inventario$Diametros, las = 1, col = "#ffe0b3")
diam_hist
diam_hist$breaks

h1 <- hist(inventario$Diametros, xaxt = "n",
           breaks = c(6, 8, 10, 12, 14, 16, 18, 20, 22, 24),
           col = "#00cc99", xlab = "Diametros (cm)",
           ylab = "Frecuencias",
           main = "",
           las = 1,
           ylim = c(0,14))
axis(1,h1$mids)


# AUTOESTUDIO -------------------------------------------------------------

altura_hist <- hist(inventario$Altura, las = 1, col = "pink")
altura_hist
altura_hist$breaks

h <- hist(inventario$Altura, xaxt = "n",
          breaks = c (8, 10, 12, 14, 16, 18, 20, 22),
          col = "orchid1", xlab = "Altura (cm)",
          ylab = "Frecuencias",
          main = "",
          las = 1,
          ylim = c(0,14))
axis(1,h$mids)

