
library(deSolve)

##### Modelo miasis por gusano barrenador (bases)

miasis <- function(t, state, pars){
  with(as.list(c(state, pars)), { 
    
#población bovinos
    NV <- Sv + Ev + Iv + Rv
    
# Hospedante
    
    dIv <- sigma * Sv * (M / NV) - beta * Ev - gamma * Iv + delta * Ml * Iv - miuv * Iv - miue * Iv
    dSv <- natalidad + p1 + p2 - sigma * (Sv * (M / NV)) - miuv * Sv + tetha * Rv
    dRv <- gamma * Iv - miuv * Rv + tetha * Rv
    dEv <- sigma * Sv * (M / NV) - beta * Ev - miuv * Ev   # ← corregido S → Sv
    
# Vector
    dM  <- epsilon * Mp - (alpha * M) - mium * M
    dMo <- alpha * M - (eta + miumo) * Mo
    dMl <- eta * Mo - (kappa + miuml) * Ml
    dMp <- kappa * Ml - (epsilon + miump) * Mp   # ← corregido mp → Mp
    
    list(c(dIv, dSv, dRv, dEv, dM, dMo, dMl, dMp))
  })
}

pars <- c(
  sigma = 1.740982e-06,
  beta = 0.5,
  gamma = 0.1428571,
  delta = 0.002857143,
  miue = 0.01,
  miuv = 0.048,
  tetha = 0.03333333,
  epsilon = 0.1176471,
  mium = 0.05333333,
  miumo = 0.005, 
  miuml = 0.01,
  miump = 0.03,
  kappa = 0.1666667, 
  eta = 2.4,
  alpha = 6.942857e-05, 
  natalidad = 83467.29,
  p1 = 23325,
  p2 = 1444
)

t <- seq(0, 100, by = 0.01)

state <- c(Sv = 1171866, Ev = 469147, Iv = 7474, Rv = 5231,M = 10000000, Mo = 3.5e+09, Ml =2242200, Mp = 1166666667)

# Ecuaciones diferenciales

out1 <- ode(y = state, times = t, func = miasis, parms = pars)

plot(out1, col = "purple")

matplot(out1[,1], out1[, c("Sv","Ev","Iv","Rv")], type="l",
        lwd=2, col=c("blue","purple","red","green"))

#################################################################################

##Escenario sin liberación de moscas con ganado ilegal

no_mosca<-function(t1, state1, pars1){
  with(as.list(c(state1, pars1)), { 
    
    #población bovinos
    NV <- Sv + Ev + Iv + Rv
    
    # Hospedante
    
    dIv <- sigma * Sv * (M / NV) - beta * Ev - gamma * Iv + delta * Ml * Iv - miuv * Iv - miue * Iv
    dSv <- natalidad + p1 + p2 - sigma * (Sv * (M / NV)) - miuv * Sv + tetha * Rv
    dRv <- gamma * Iv - miuv * Rv + tetha * Rv
    dEv <- sigma * Sv * (M / NV) - beta * Ev - miuv * Ev   # ← corregido S → Sv
    
    # Vector
    dM  <- epsilon * Mp - (alpha * M) - mium * M
    dMo <- alpha * M - (eta + miumo) * Mo
    dMl <- eta * Mo - (kappa + miuml) * Ml
    dMp <- kappa * Ml - (epsilon + miump) * Mp   # ← corregido mp → Mp
    
    list(c(dIv, dSv, dRv, dEv, dM, dMo, dMl, dMp))
  })
}

pars1<- c(
  sigma = 1.740982e-06,
  beta = 0.5,
  gamma = 0.1428571,
  delta = 0.01,
  miue = 0.01,
  miuv = 0.048,
  tetha = 0.03333333,
  epsilon = 0.1176471,
  mium = 0.05333333,
  miumo = 0.005, 
  miuml = 0.01,
  miump = 0.03,
  kappa = 0.1666667, 
  eta = 2.4,
  alpha = 0.0003702857, 
  natalidad = 83467.29,
  p1 = 23325,
  p2 = 1444
)

t1<- seq(0, 100, by = 0.01)

state1<- c(Sv = 1171866, Ev = 469147, Iv = 7474, Rv = 5231,M = 1000000, Mo = 3.5e+09, Ml =2242200, Mp = 1166666667)

# Ecuaciones diferenciales

out2 <- ode(y = state1, times = t1, func = no_mosca, parms = pars1)

plot(out2, col = "purple")

matplot(out2[,1], out2[, c("Sv","Ev","Iv","Rv")], type="l",
        lwd=2, col=c("blue","purple","red","green"))
###############################################################################################

##sin ganado ilegal

##### Modelo miasis por gusano barrenador (bases)

no.ilegal<- function(t2, state2, pars2){
  with(as.list(c(state2, pars2)), { 
    
    #población bovinos
    NV <- Sv + Ev + Iv + Rv
    
    # Hospedante
    
    dIv <- sigma * Sv * (M / NV) - beta * Ev - gamma * Iv + delta * Ml * Iv - miuv * Iv - miue * Iv
    dSv <- natalidad + p1 + p2 - sigma * (Sv * (M / NV)) - miuv * Sv + tetha * Rv
    dRv <- gamma * Iv - miuv * Rv + tetha * Rv
    dEv <- sigma * Sv * (M / NV) - beta * Ev - miuv * Ev   # ← corregido S → Sv
    
    # Vector
    dM  <- epsilon * Mp - (alpha * M) - mium * M
    dMo <- alpha * M - (eta + miumo) * Mo
    dMl <- eta * Mo - (kappa + miuml) * Ml
    dMp <- kappa * Ml - (epsilon + miump) * Mp   # ← corregido mp → Mp
    
    list(c(dIv, dSv, dRv, dEv, dM, dMo, dMl, dMp))
  })
}

pars2 <- c(
  sigma = 1.774202e-06,
  beta = 0.5,
  gamma = 0.1428571,
  delta = 0.002857143,
  miue = 0.01,
  miuv = 0.048,
  tetha = 0.03333333,
  epsilon = 0.1176471,
  mium = 0.05333333,
  miumo = 0.005, 
  miuml = 0.01,
  miump = 0.03,
  kappa = 0.1666667, 
  eta = 2.4,
  alpha = 6.942857e-05, 
  natalidad = 83467.29,
  p1 = 23325,
  p2 =0
)

t2 <- seq(0, 100, by = 0.01)

state2 <- c(Sv = 1171866, Ev = 469147, Iv = 7474, Rv = 5231,M = 10000000, Mo = 3.5e+09, Ml =2242200, Mp = 1166666667)

# Ecuaciones diferenciales

out3 <- ode(y = state2, times = t2, func = no.ilegal, parms = pars2)

plot(out3, col = "purple")

matplot(out3[,1], out3[, c("Sv","Ev","Iv","Rv")], type="l",
        lwd=2, col=c("blue","purple","red","green"))
