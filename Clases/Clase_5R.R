#MARLEN GUTIERREZ BARRIENTOS
#CLASE 5
#06/09/2022

viv <- read.csv("clases/vivero.csv", header = T)
summary(viv)

#MEDIA TEORETICA = 0.9375
#HIPOTESIS NULA: No existe diferencia entre el valor propuesto por Conafor (0.9375) y mis plantas

boxplot(viv$IE)

hist(viv$IE)


# NORMALIDAD DE DATOS -----------------------------------------------------

#APLICAS LA PRUEBA DE SHAPIRO PARA VER SI LOS DATOS TIENEN DISTRIBUCION NORMAL

shapiro.test(viv$IE)
#SE ACEPTA LA HIPOTESIS NUELA POR LO TANTO TIENE DISTRIBUCION NORMAL

ks.test(viv$IE, "pnorm", mean=mean(viv$IE), sd =sd(viv$IE))
#PRUEBA DE KOLMOGOROV-SMIRNOV PARA VER QUE LOS DATOS VIENEN DE UNA DISTRIBUCION NORMAL Y DE ACUERDO CON EL RESULTADO SE ACEPTA LA HIPOTESIS NULA 

t.test(viv$IE, mu = 0.9375)
#GRADOS DE LIBERTAD = n-1 el total de los datos menos 1 

t.test(viv$IE, mu = 0.89)
t.test(viv$IE, mu = 0.88)
t.test(viv$IE, mu = 0.77) #HIPOTESIS ALTERNATIVA- ESTO SE COMPARA CON EL VALOR DE P-VALUE

data("chickwts")
head(chickwts)
summary(chickwts)

#HIPOTESIS NULA NO EXISTE DIFERENCIA ENTRE EL PESO DE LOS POLLOS Y EL ALIMENTO QUE ESTA ESTABLECIDO DE 300 GRS

hist(chickwts$weight) #PRIMERO VER LA DISTRIBUCION DE LOS DATOS 

shapiro.test(chickwts$weight) #VER SI LOS DATOS PROVIENEN DE UNA DISTRIBUCION NORMAL

t.test(chickwts$weight, mu =300)
#VES LOS GRADOS DE LIBERTAD ES n-1 Y SI DA EL NUMERO CORRECTO ESTA BIEN REALIZADO LA PRUEBA Y PARA VISUALIZAR EL NUMERO DE DATOS ES CON LA FUNCION (length)

t.test(chickwts$weight, mu =250)


# PRUEBA DE T- DOS MUESTRAS INDEPENDIENTES -----------------------------------------------

boxplot(viv$IE ~ viv$Tratamiento) #VER LOS DATOS SI TIENEN VARIANZA 

shapiro.test(viv$IE) #VER SI LOS DATOS TIENEN DISTRIBUCION NORMAL

var.test(viv$IE ~ viv$Tratamiento) #COMPARAR VARIANZAS SI EL VALOR DA 0.05 SIGNIFICA QUE NO HAY DIFERENCIAS EN LAS VARIANZAS, LA TIENEN IGUAL 
#TIENEN VARIANZAS IGUALES 

t.test(viv$IE ~ viv$Tratamiento, var.equal = T) 

#LOS GRADOS D ELIBERTAD ES n-2 SON DOS VARIABLES QUE ESTAMOS COMPARANDO 

t.test(viv$IE ~ viv$Tratamiento) #CUANDO NO LE PONES QUE LAS VARIANZAS SON IGUALES CAMBIAN LOS GRADOS DE LIBERTAD Y EL VALOR DE (P) PORQUE TOMA UNOS VALORES DE REFERENCIA YA PREESTABLECIDOS 

invent <- read.csv("Clases/inventario.csv", header = T)
invent$Tratamiento <- as.factor(invent$Tratamiento)
invent$Fecha <- as.factor(invent$Fecha)

#HIPOTESIS NULA NO EXISTE DIFERENCIA SIGNIFICATIVA ENTRE LOS RODALES Y EL DIAMETRO

boxplot(invent$Diametro ~ invent$Tratamiento) #ESAN CASI IGUALES SOLO HAY DOS DATOS OUTLAYER

shapiro.test(invent$Diametro)
var.test(invent$Diametro ~ invent$Tratamiento)

t.test(invent$Diametro ~ invent$Tratamiento, var.equal = T)

boxplot(invent$Dcopa ~ invent$Tratamiento)
shapiro.test(invent$Dcopa)
var.test(invent$Dcopa ~ invent$Tratamiento)
t.test(invent$Dcopa ~ invent$Tratamiento, var.equal = T) 
#NO HAY DIFERENCIAS SIGNIFICATIVAS 


# MUESTRAS DEPENDIENTES ---------------------------------------------------

boxplot(invent$Kilogramo ~ invent$Fecha)
shapiro.test(invent$Kilogramo)
var.test(invent$Kilogramo ~ invent$Fecha)

t.test(invent$Kilogramo ~ invent$Fecha, paired = TRUE) #PAIRED PARA ESPECIFICAR QUE SON MUESTRAS DEPENDIENTES
#NO EXISTEN DIFERENCIAS SIGNIFICATIVAS ENTRE LA PRODUCCION DE SEMILLAS DEL 2012 Y 2014
#ESTA FALTA DE DIFERENCIAS PUEDE SER QUE SE MUESTREO EL MISMO BOSQUE








