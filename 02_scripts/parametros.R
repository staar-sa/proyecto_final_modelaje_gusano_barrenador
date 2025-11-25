
######Parámetros

##Introducción del ganado

##23,325 cabezas de ganado entraron a Chiapas en 2023

##231 bovinos incautados ese año

ilegal<-231/.16 ##16% de ganado ilegal
ilegal

##legal= 23 325 e ilegal=  1444



# Parámetro σ (Susceptible → Expuesto)

# Rebrote actual (2024-2025)

# 1. Población bovina en 2025

N_2022 <- 1653718  # Población bovina 2022 (INEGI)
r <- 0.0806        # Tasa de crecimiento anual
t <- 3             # Años de proyección

N_2025 <- N_2022 * (1 + r)^t
N_2025

# 2. Proporción de incidencia
casos_actuales <- 1326  # Casos confirmados en Chiapas
proporcion_actual <- casos_actuales / N_2025
proporcion_actual

# 3. Tasa de incidencia (casos por día)
duracion_brote <- 365  # días
sigma_actual <- proporcion_actual / duracion_brote
sigma_actual

# Período pre-erradicación (1972-1990)

# 1. Proporción de incidencia histórica
casos_historicos <- 286750      # Casos totales 1972-1990
poblacion_historica <- 24600000 # Población bovina 
proporcion_pre <- casos_historicos / poblacion_historica
proporcion_pre

# 2. Conversión a tasa diaria
duracion_pre <- 18 * 365  # 18 años en días
sigma_pre <- proporcion_pre / duracion_pre
sigma_pre


# Parámetro β (Expuesto → Infestado)


# Tiempo promedio en estado Expuesto
# - Huevos (pre-eclosión): ~18 horas
# - Larvas recién nacidas: ~30 horas
# - Total: ~48 horas = 2 días

tau_E <- 2  # días
beta <- 1 / tau_E
beta

# Parámetro γ (Infestado → Recuperado)

# Duración promedio de la infestación sin tratamiento: 7 días
# (rango reportado: 7-10 días hasta desenlace fatal o recuperación)

tau_I <- 7  # días
gamma <- 1 / tau_I
gamma

