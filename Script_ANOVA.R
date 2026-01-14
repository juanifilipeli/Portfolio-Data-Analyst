##################################   MODELOS LINEALES GENERALES     ########################
####################################### COMPARACIÓN DE MEDIAS       #######################

# Análisis de la Varianza (ANOVA) de 1 Factor


# En Misiones coexisten tres especies de boyeros (Aves, Icteridae). Se midió la longitud del pico (en mm) para poner a prueba la hipótesis de que las tres especies (Cacicus chrysopterus, C. haemorrhous, C. solitarius) difieren en el largo de sus picos. Los datos obtenidos se encuentran en el archivo "BD_boyeros.txt":



read.table("BD_boyeros.txt", header = T)
boyeros <- read.table("BD_boyeros.txt", header = T,stringsAsFactors = T)



##Aplicamos un poco de estadistica descriptiva
install.packages("psych")

library(psych)
describeBy(boyeros$longitud,boyeros$Especie)

#### Descripcion grafica por grupos ####

boxplot(longitud~Especie, data=boyeros, col="brown", main="longitud de pico según especie de boyero")


#Ahora hago el ANOVA para comparar  3 medias poblacionales asociadas a un factor, que en este caso es la especie.


modelo_boyas <- lm(longitud~Especie, data=boyeros) 


# Â¿CÃ³mo piensan que R se da cuenta que no es un anÃ¡lisis de regresiÃ³n?

#porque la variable explicativa es cualitativa/factor.

# Otro comando para correr anova es:  
modelo1 <- aov(longitud~Especie, data=boyeros)


# Antes de mirar el resultado del analisis, hay que evaluar si el modelo es valido. Calculen los residuos y predichos para evaluar los supuestos:

e <-resid(modelo_boyas)
pred<-fitted(modelo_boyas)



# Para evaluar la NORMALIDAD lo puedo hacer de  manera grafica y/o analitica.
install.packages("car")
library(car)
qqPlot(e)
#CONCLUYO LA NORMALIDAD GRAFICA (opcion 1)
#veo que la residuos estan dentro del rango adecuado.




#CONCLUYO LA NORMALIDAD ANALITICA(opcion 2)
# analitico: Shapiro-wilks (para usar esta, necesito plantear hipotesis)
# H0: buen ajuste de los residuos a distr. Normal
# H1: mal ajuste de los residuos a distr. Normal

shapiro.test(e)

# como p-valor > alfa (0.05), no se rechaza la H0, por lo que se sigue suponiendo la normalidad.. por ende continuo con el modelo..


# Ahora evaluo la HOMOCEDASTICIDAD de manera grafica.

plot(pred, e)
abline(h = 0)

# Como se observa amplitud similar, no presentan patrones y presentan una variabilidad constante, entonces seguimos suponiendo homocedasticidad, y continuo con el analisis.


#Ahora , Veamos una prueba de hipotesis para la homogeneidad de varianzas, conocida como:
####  Prueba de LEVENE ####  (es un anova pero trabaja con los residuos absolutos de c/tratamiento)


#Hipotesis de LEVENE
#H0: varianzas son iguales. sigma1**2 = sigma2**2= sigma3**2= sigma poblacional
#H1: alguna varianza es distinta a la poblacional.


library(car)
leveneTest(longitud~Especie, data=boyeros)

#concluyo con levene
# Como p-valor > alfa, entonces no rechazo H0 y sigo suponiendo homocedasticidad. Asumo varianzas iguales entre tratamientos


#Tambien puedo utilizar el metodo de cuadrados minimos para el ANOVA


anova(modelo_boyas)


##RTA: Como p-valor < alfa (0.05), rechazamos H0. Esto quiere decir que al menos la respuesta promedio de un tratamiento difiere del resto.
# Hay suficiente evidencia para suponer que la longitud del pico difiere según la especie de boyero por lo menos en una de las especies, con una significación del 5%.
