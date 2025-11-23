# MODELO SEIRS + CICLO DEL GUSANO BARRENADOR


# Basado EXACTAMENTE en el diagrama:
# A → H → L → P → A   (ciclo del parásito)
# S → E → I → R → S   (ciclo en bovinos)

library(deSolve)

###############################################################

# Función que ajusta qué tan bueno es el clima para el gusano
# Entre más grande el valor → más rápido cambia de fase
factor_clima <- function(T, Tmin, P, Pmin, H, Hmin, K){
  fc <- ((T - Tmin)/K) * ((P - Pmin)/K) * ((H - Hmin)/K)
  return(max(fc, 0))   # si es negativo, queda en 0
}

# Proporción de hembras afectadas por TIE (u)
# Si u = 0.5 → la mitad de las hembras NO producen huevos fértiles
# Esto reduce la oviposición efectiva
oviposicion_TIE <- function(alpha_base, u, fc){
  alpha_ef <- alpha_base * (1 - u) * fc
  return(alpha_ef)
}

###############################################################

#PARÁMETROS (cambiar cuando tengamos los reales de literatura, bases de datos y calculos)

param <- list(
  
  # Parámetros del clima
  T = 28, Tmin = 15,
  P = 12, Pmin = 2,
  H = 60, Hmin = 30,
  K = 50,
  
  # TIE
  u = 0.4,        # 40% de hembras quedan infértiles
  
  # Tasas base del ciclo (luego se multiplican por el clima)
  alpha_base = 120,   # oviposición por adulto
  eta_base   = 0.4,   # huevo → larva
  kappa_base = 0.3,   # larva → pupa
  eps_base   = 0.2,   # pupa → adulto
  
  # Mortalidad del parásito
  muA = 0.05,
  muH = 0.05,
  muL = 0.05,
  muP = 0.05,
  
  # Ganado
  beta = 0.0003,     # presión de adultos hacia S
  delta = 0.0001,    # efecto de larvas (flecha punteada en tu diagrama) -- no entendi, ayudaaa
  sigma = 1/7,       # E → I
  gamma = 1/14,      # I → R
  theta = 1/21,      # R → S
  muB = 1/365,       # mortalidad del bovino
  
  # Flujo de animales (entrada a suceptibles)
  v = 10,            # entrada total
  p = 0.85           # proporción ilegal
)

###############################################################
##MODELO COMO TAL

modelo_gusano <- function(t, state, param){
  
  with(as.list(c(state, param)), {
    
    #calculodelclima
    fc <- factor_clima(T, Tmin, P, Pmin, H, Hmin, K)
    
    #ajustar tasas por el clima y mosca esteril
    alpha <- oviposicion_TIE(alpha_base, u, fc)  # A → H
    eta   <- eta_base   * fc                     # H → L
    kappa <- kappa_base * fc                     # L → P
    eps   <- eps_base   * fc                     # P → A
    
    #FUERZA DE INFESTACIÓN -- no entendi por lo punteado
    # Flechas del diagrama: Adulto → Expuesto, Larva (punteado) → I
    lambda <- beta * A + delta * L
    
    #ciclo del parasito
    dA <-  eps * P - muA * A
    dH <-  alpha * A - eta * H - muH * H
    dL <-  eta * H - kappa * L - muL * L
    dP <-  kappa * L - eps * P - muP * P
    
    #animales que entrarn legal e ilegal
    entrada_S <- v * p
    salida_S  <- v * (1 - p)
    
    #modelo del ganado
    dS <- entrada_S - salida_S - lambda * S + theta * R - muB * S
    dE <- lambda * S - sigma * E - muB * E
    dI <- sigma * E - gamma * I - muB * I
    dR <- gamma * I - theta * R - muB * R
    
    return(list(c(dS, dE, dI, dR, dA, dH, dL, dP)))
  })
}


##ESTADO BASE (TODOS SE CAMBIAN A LOS ACTUALES O ESTIMADOS)

state <- c(
  S = 10000,
  E = 5,
  I = 10,
  R = 0,
  A = 500,
  H = 200,
  L = 150,
  P = 80
)

#TIEMPO DE SIMULACIÓN
tiempo <- seq(0, 200, by = 1)

#CORRER EL MODELO
out <- ode(y = state,
           times = tiempo,
           func = modelo_gusano,
           parms = param)

#GRAFICAR RESULTADOS

matplot(out[,1], out[,2:9], type="l", lty=1,
        xlab="Tiempo (días)", 
        ylab="Poblaciones",
        main="Dinámica del gusano barrenador + SEIRS bovino")

legend("topright",
       legend=c("S","E","I","R","A","H","L","P"),
       col=1:8, lty=1, cex=0.8)