##FACTOR CLIMA

library(readr)
library(dplyr)

# Cargar archivo CSV 
clima <- read_csv("01_datos_crudos/clima_chiapas_2022_2025.csv.csv")

# Ver primeras filas
head(clima)


# Crear el factor climático

# Definir valores mínimos biológicos sacados de literatura
Tmin <- 22 #temp mínima para desarrollo del barrenador, Spradbery 1991; Gutierrez 2019
Pmin <- 20 #precipitación mínima, NOAA/USDA Screwworm Program
Hmin <- 60 # Humedad relativa mínima,  Gutierrez 2019; Hall & Wall 1995
K    <- 612 #grados/dia 

# función del factor climático
f_clima <- function(T, P, H, Tmin, Pmin, Hmin, K){
  ((T - Tmin)/K) * ((P - Pmin)/K) * ((H - Hmin)/K)
}

#aplicar la función a todo el dataset
clima$f_clima <- with(
  clima,
  f_clima(temp_media, prec_media, humedad_media,
          Tmin, Pmin, Hmin, K)
)

#revisar los resultados
head(clima)



