
######Parámetros

library(readr)
library(dplyr)


##Natalidad 0.4 en Chiapas



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

## Parámetro de oviposición base (alpha_base)
## alpha_base = (Huevos totales puestos en su vida) / (Esperanza de vida reproductiva de la hembra (días))
promedio_huevos_por_masa <- 300   ## Promedio de huevos por masa (entre 200 y 400)
numero_masas_totales <- 4         ## Máximo de masas de huevos en su vida
vida_reproductiva_dias <- 14      ## Días que la hembra está en capacidad de oviponer
proporcion_hembras <- 0.50        ## 50% de la población Adulta (A) son hembras

Huevos_totales_por_hembra <- promedio_huevos_por_masa * numero_masas_totales
Huevos_totales_por_hembra

Tasa_Hembra <- Huevos_totales_por_hembra / vida_reproductiva_dias
Tasa_Hembra

alpha_base <- Tasa_Hembra * proporcion_hembras ##la tasa promedio por individuo en A es la mitad de la tasa de la hembra.
alpha_base



# Cargar archivo CSV 
clima <- read_csv("01_datos_crudos/clima_chiapas_2022_2025.csv.csv")

# Ver primeras filas
head(clima)


# Crear el factor climático

# Definir valores mínimos biológicos sacados de literatura
Tmin <- 22 #temp mínima para desarrollo del barrenador, Spradbery 1991; Gutierrez 2019
Pmin <- 20 #precipitación mínima, NOAA/USDA Screwworm Program
Hmin <- 60 # Humedad relativa mínima,  Gutierrez 2019; Hall & Wall 1995
K    <- 612 #grados/dia de los barrenadores en gral

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

#promedio de la influencia del clima en el periodo establecido

influencia.clima<-mean(0.00000162, 0.00000298 , 0.000000971, 0.00000379, 0.0000250,  0.0000802)

alpha_clima<- Tasa_Hembra * proporcion_hembras * influencia.clima
alpha_clima

##cuando no hay moscas estériles

promedio_huevos_por_masa1 <- 800  ## Promedio de huevos por masa (entre 200 y 400)
numero_masas_totales1 <- 8         ## Máximo de masas de huevos en su vida
vida_reproductiva_dias1<- 14      ## Días que la hembra está en capacidad de oviponer
proporcion_hembras1 <- 0.50        ## 50% de la población Adulta (A) son hembras

Huevos_totales_por_hembra1 <- promedio_huevos_por_masa1 * numero_masas_totales1
Huevos_totales_por_hembra1

Tasa_Hembra1 <- Huevos_totales_por_hembra1 / vida_reproductiva_dias1
Tasa_Hembra1
alpha_nm<-Tasa_Hembra1* proporcion_hembras1* influencia.clima
alpha_nm

## Mortalidad mosca Adulta muA
## El parámetro muA es la tasa de mortalidad natural del adulto, y se calcula como el inverso de la esperanza de vida promedio del adulto en la naturaleza.

vida_macho_promedio <- (14 + 21) / 2   ## Promedio de vida de los machos: 17.5 días
vida_hembra_promedio <- (10 + 30) / 2 ## Promedio de vida de las hembras: 20 días

Vida_Promedio_Adulto <- (vida_macho_promedio + vida_hembra_promedio) / 2 ##Vida Promedio = (Vida Machos + Vida Hembras) / 2  (Asumiendo 50% de cada sexo)
Vida_Promedio_Adulto

muA <- 1 / Vida_Promedio_Adulto
muA

## Tasa de transición (lambda)

## Tasa de Transición (lambda) = 1 / Tiempo Promedio de la Fase (tau días)

tau_huevo_a_larva <- ((8 + 12) / 2) / 24  ## 10 horas convertido a días: 0.4167 días
tau_larva_a_pupa <- (5 + 7) / 2          ## Promedio de días: 6 días
tau_pupa_a_adulto <- (7 + 10) / 2        ## Promedio de días: 8.5 días

eta_base <- 1 / tau_huevo_a_larva ## Tasa de huevo a larva
eta_base

kappa_base <- 1 / tau_larva_a_pupa ##Tasa de larva a pupa
kappa_base

eps_base <- 1 / tau_pupa_a_adulto ## Tasa de pupa a adulto 
eps_base

###Recuperación

etha<- 1/30 # 30 días de recuperación luego del tratamiento, aunque hay mejora a los 10 días
etha

##delta

delta<-1/350
delta

delta2<-1/700
delta2

#Natalidad

natalidad<-N_2025*.04
natalidad

###Variables N=

#Casos en 2025: 7474

suceptibles<-1653718-7474-469147-5231
suceptibles
infestados<-7474 #casos
Expuestos<-469147 ##cabezas de ganado en sitios fronterizos
recuperados<-5231

total<-suceptibles+Expuestos+recuperados
total

larvas<-300*infestados
larvas

huevos<-10000000*350
huevos

pupa<-huevos/3
pupa
