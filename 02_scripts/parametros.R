
######Parámetros

##Introducción del ganado

##23,325 cabezas de ganado entraron a Chiapas en 2023

##231 bovinos incautados ese año

ilegal<-231/.16 ##16% de ganado ilegal
ilegal

##legal= 23 325 e ilegal=  1444


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
