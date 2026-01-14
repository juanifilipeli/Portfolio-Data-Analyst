datos_aire <- read.table("calidad_aire_2025.csv", header = T, stringsAsFactors = T)

#1) Vamos a limpiar los datos, ya que las columas "palermo" (entre otras) no tienen informacion

library(tidyverse)
library(lubridate)

# Usamos na = c("", "NA", "s/d") para que R entienda que "s/d" es un valor faltante
df_aire <- read_csv("calidad_aire_2025.csv", na = c("", "NA", "s/d"))

# 2)  Convertimos las columnas a formato correcto
df_limpio <- df_aire %>%
  select(-contains("palermo")) %>% # Eliminamos Palermo porque no tiene datos
  mutate(
    fecha = ymd(fecha),
    # Convertimos a numérico lo que R haya leído como texto por error
    across(starts_with(c("co_", "no2_", "pm10_")), as.numeric)
  ) %>%
  drop_na(co_centenario) # Filtramos filas donde no haya mediciones básicas

glimpse(df_limpio)



###Ahora vamos a hacer un analisis de por ejemplo ¿A que hora del dia la ciudad esta mas contaminada?


# Promediamos el Monóxido de Carbono (CO) por hora para todas las estaciones
df_por_hora <- df_limpio %>%
  group_by(hora) %>%
  summarise(
    co_medio = mean(co_centenario, na.rm = TRUE),
    no2_medio = mean(no2_centenario, na.rm = TRUE)
  )

# Gráfico para ver el pico de contaminación
ggplot(df_por_hora, aes(x = hora, y = co_medio)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Niveles de CO promedio por hora en Parque Centenario",
       x = "Hora del día",
       y = "Concentración de CO")

# Para responder a la pregunta, en el grafico realizado se puede ver que a las 2 AM se llega a un pico de contaminacion de CO , mientras que a las 7 am aprox se llega a su menor nivel de contaminacion. La concentracion de CO vuelve a tocar su punto maximo a las 00 hs. Esto corresponde a parque centenario.


#Ahora vamos a hacer un Boxplot que compare las tres estaciones principales (Centenario, Córdoba y La Boca).

library(tidyverse)

# 1. Cargamos los datos manejando todos los tipos de "Sin Datos" que encontré
# Encontré que hay "s/d" y también "x/d" en tu archivo.
df_aire <- read_csv("calidad_aire_2025.csv", na = c("", "NA", "s/d", "x/d"))

# 2. Transformación a formato "Tidy" (Largo)
# Esto es clave: pasamos de tener 10 columnas a tener una de "Estación" y otra de "Valor"
df_tidy <- df_aire %>%
  select(-contains("palermo")) %>% # Quitamos la columna vacía
  pivot_longer(
    cols = starts_with(c("co_", "no2_", "pm10_")), 
    names_to = "variable", 
    values_to = "valor"
  ) %>%
  # Separamos el nombre (ej: "no2_la_boca") en dos columnas: "contaminante" y "estacion"
  separate(variable, into = c("contaminante", "estacion"), sep = "_", extra = "merge") %>%
  filter(!is.na(valor)) # Limpiamos los filas sin datos

# 3. Creamos el Boxplot comparativo para NO2
df_tidy %>%
  filter(contaminante == "no2") %>%
  ggplot(aes(x = estacion, y = valor, fill = estacion)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") + # Colores profesionales
  labs(
    title = "Distribución de Dióxido de Nitrógeno (NO2) por Estación",
    subtitle = "Comparativa de niveles de contaminación en CABA (2025)",
    x = "Punto de Monitoreo",
    y = "Concentración (ppb)",
    fill = "Estación"
  )


#Conclusion: La estación de La Boca tiene niveles promedio de NO2 significativamente más altos que Parque Centenario y Av. Córdoba (aprox. 26.5 ppb frente a 15.1 ppb y 18.8 ppb respectivamente).Esto tiene sentido ya que La Boca tiene una alta actividad de transporte pesado y cercanía al puerto. 




