###################   Prueba de hipotesis para una muestra ###########
######################################################################################


#Ahora voy a demostrar una prueba de hipotesis (junto con sus hipotesis enunciadas) de un problema de ejemplo:

# Las orugas de mariposa Bandera Argentina (Morpho epistrophus argentinus), declarada protegida y patrimonio natural en el Partido de Punta Indio (Provincia de Bs. As.), acumulan glucósidos que las hacen repugnantes a los pájaros, por lo cual estos tienden a evitarlas después de un primer encuentro. En dicho distrito se recolectaron 64 de tales orugas y se les determinó la concentración de glucósidos en relación a sus pesos.
# Estudios anteriores aseguran que la población en estudio sigue una distribución aproximadamente normal con media de 0,19 g glucósidos / kg gusano y varianza 0,0036 (g glucósidos / kg gusano)^2. 


#### 1.1 ####
# En los últimos años se ha observado una disminución en la abundancia de las orugas lo cual podría sugerir una mayor depredación de las aves asociada a una menor acumulación de glucósidos por parte de las orugas. En este sentido, se busca poner a prueba la hipótesis de que en este distrito la concentración media de  glucósidos en las orugas es menor que la media previamente estimada. Calcular el p-valor de la prueba y concluir utilizando un nivel de significación del 5%.


#Planteemos las hipotesis estadisticas:

#HB= disminucion de la abundancia por disminucion de glucosidos en orugas.
# Ho: La concentración media de  glucósidos en las orugas NO es menor que la media previamente estimada. mu>=0.19
# H1: La concentración media de  glucósidos en las orugas  es menor que la media previamente estimada. mu < 0.19.


#pongamos a prueba ahora con los datos:

orugas<-read.table("BD_orugas.txt", header = T)
head(orugas)


# Calculamos la media de la concentración de glucósidos de la muestra.

x.raya <-mean(orugas$conc)
sigma <- sd(orugas$conc)

#Ahora si pongamos a prueba las hipotesis con la funcion ztest del paquete BSDA
install.packages("BSDA")
library(BSDA)


z.test(orugas$conc, alternative = "less", mu = 0.19, sigma.x = 0.06)


### Una vez realizada la prueba de hipótesis, informo:

# Valor del estadístico de prueba estandarizado (z) o no (x.raya)= -1.3

# Valor p =0.09

#El valor p es un valor de probabilidad, por lo que oscila entre 0 y 1. El valor p nos muestra la probabilidad de haber obtenido el resultado que hemos obtenido suponiendo que la hipótesis nula H0 es cierta. Es la probabilidad de que la hipotesis de que la concentración media de  glucósidos en las orugas  no sea menor que la media previamente estimada 

# Conclusión estadística:
### Valor p  < o > con respecto a alfa:	 Como pvalor>alpha  NO rechazo la H0,PERO TAMPOCO ESTOY A FAVOR DE H1. NS (no significativas) las diferencias. 

# Conclusión biológica:

#La conclusion es que no rechazo que la concentración media de  glucósidos en las orugas  NO sea menor que la media previamente estimada. (la que escribi yo)

#"No hay suficientes evidencias para suponer que la concentracion media de glucosidos de las orugas de este campo es menor que la observada anteriormente para la poblacion (mu=0.19)" 

