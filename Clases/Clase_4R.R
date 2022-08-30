# Clase 4 
# Marlen Gutierrez Barrientos 
# 30/08/2022

data("chickwts")#base de datos de r anteriormente guardada
head(chickwts[c(1:2,42:43, 62:64), ])

feeds <- table(chickwts$feed)#hagrupar en frecuencias es una tabla de frecuencia
barplot(feeds)
barplot(feeds[order(feeds, decreasing = TRUE)])


barplot(feeds[order(feeds, decreasing = TRUE)],
        horiz=TRUE, col= "#996600")
        
        

# Descargar datos abiertos Gob Mexico -------------------------------------

fert <- "https://www.agricultura.gob.mx/sites/default/files/sagarpa/Publicaciones/datos_abiertos/2021/fertilizantes/6to-listado-fertilizantes-2021v2.csv"

ferti <- read.csv(fert,header = T)

apoyos <- table(ferti$ESTADO)
apoyos

barplot(apoyos, col="red",
        ylim=c(0,8000),
        ylab= "Apoyos de la 4T",
        xlab="Estados")

mun <- table(ferti$MUNICIPIO)
mun

library(dplyr)

EsXMun <- ferti%>% #Sirve para agrupar se llama pipe (ctrl shift m)
  group_by(ESTADO, MUNICIPIO)%>% 
  summarise(n=n())

Gro <- ferti %>% 
  filter(ESTADO == "GUERRERO") %>% 
  group_by(MUNICIPIO) %>% 
  summarise(n=n()) %>% 
  filter(n >= 200) %>% 
  mutate(porciento =n / sum(n)*100) 
barplot(Gro$porciento)

write.csv(Gro,"Clases/Guerrero.csv", row.names = F)


