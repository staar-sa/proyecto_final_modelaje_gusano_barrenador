
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


##Guardar gráficas en pdf  
pdf(file = "Resultados_Miasis.pdf", width = 10, height = 14) # Le damos bastante altura (14) para que quepan las dos gráficas sin aplastarse.

# mfrow = c(2, 1): Crea una matriz de 2 filas y 1 columna (una gráfica arriba de la otra)
# mar = márgenes: c(abajo, izquierda, arriba, derecha). Damos espacio para los ejes.
par(mfrow = c(2, 1), mar = c(5, 5, 4, 2) + 0.1)


# gráfica 1: dinámica del ganado 

cols_ganado <- c("Sv", "Ev", "Iv", "Rv") # Seleccionamos las columnas del ganado

matplot(out1[, "time"], out1[, cols_ganado], 
        type = "l",       
        lty = 1,          
        lwd = 3,           
        col = c("blue", "orange", "red", "green"), 
        xlab = "Tiempo (días)", 
        ylab = "Población (Individuos)",
        main = "Dinámica del Ganado Bovino",
        cex.lab = 1.2,    
        cex.main = 1.5)   


legend("topright", 
       legend = c("Susceptibles (S)", "Expuestos (E)", "Infectados (I)", "Recuperados (R)"),
       col = c("blue", "orange", "red", "green"), 
       lty = 1, lwd = 3, bty = "n", cex = 1.1)

# gráfica 2: dinámica del parásito

cols_vector <- c("M", "Mo", "Ml", "Mp") # Seleccionamos las columnas del parásito

matplot(out1[, "time"], out1[, cols_vector], 
        type = "l", 
        lty = 1, 
        lwd = 3,
        col = c("black", "purple", "brown", "gray"),
        xlab = "Tiempo (días)", 
        ylab = "Población (Escala Logarítmica)",
        main = "Dinámica del Gusano Barrenador (Ciclo de Vida)",
        log = "y",        
        cex.lab = 1.2,
        cex.main = 1.5)

legend("bottomright", 
       legend = c("Adultos (M)", "Huevos (Mo)", "Larvas (Ml)", "Pupas (Mp)"),
       col = c("black", "purple", "brown", "gray"), 
       lty = 1, lwd = 3, bty = "n", cex = 1.1)


dev.off()



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

# Abrir dispositivo PDF
pdf(file = "Escenario2_SinTIE.pdf", width = 10, height = 14)

par(mfrow = c(2, 1), mar = c(5, 5, 4, 2) + 0.1)

#  Grafica 1: dinamica ganado
cols_ganado <- c("Sv", "Ev", "Iv", "Rv")

matplot(out2[, "time"], out2[, cols_ganado], 
        type = "l", lty = 1, lwd = 3,
        col = c("blue", "orange", "red", "green"),
        xlab = "Tiempo (días)", 
        ylab = "Población (Individuos)",
        main = "Escenario 2: Sin TIE - Dinámica del Ganado",
        cex.lab = 1.2, cex.main = 1.5)

legend("topright", 
       legend = c("Susceptibles", "Expuestos", "Infectados", "Recuperados"),
       col = c("blue", "orange", "red", "green"), 
       lty = 1, lwd = 3, bty = "n", cex = 1.1)

# grafica 2 dinamica del parasito 
cols_vector <- c("M", "Mo", "Ml", "Mp")

matplot(out2[, "time"], out2[, cols_vector], 
        type = "l", lty = 1, lwd = 3,
        col = c("black", "purple", "brown", "gray"),
        xlab = "Tiempo (días)", 
        ylab = "Población (Escala Log)",
        main = "Escenario 2: Sin TIE - Dinámica del Parásito",
        log = "y", # Escala logarítmica esencial aquí
        cex.lab = 1.2, cex.main = 1.5)

legend("bottomright", 
       legend = c("Adultos (M)", "Huevos (Mo)", "Larvas (Ml)", "Pupas (Mp)"),
       col = c("black", "purple", "brown", "gray"), 
       lty = 1, lwd = 3, bty = "n", cex = 1.1)

# Cerrar PDF
dev.off()
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

# abrir dispositivo PDF
pdf(file = "Escenario3_SinIlegal.pdf", width = 10, height = 14)

par(mfrow = c(2, 1), mar = c(5, 5, 4, 2) + 0.1)

# grafica 1: dinamica ganado
cols_ganado <- c("Sv", "Ev", "Iv", "Rv")

matplot(out3[, "time"], out3[, cols_ganado], 
        type = "l", lty = 1, lwd = 3,
        col = c("blue", "orange", "red", "green"),
        xlab = "Tiempo (días)", 
        ylab = "Población (Individuos)",
        main = "Escenario 3: Sin Ilegal - Dinámica del Ganado",
        cex.lab = 1.2, cex.main = 1.5)

legend("topright", 
       legend = c("Susceptibles", "Expuestos", "Infectados", "Recuperados"),
       col = c("blue", "orange", "red", "green"), 
       lty = 1, lwd = 3, bty = "n", cex = 1.1)

# grafica 2: dinamica parásito
cols_vector <- c("M", "Mo", "Ml", "Mp")

matplot(out3[, "time"], out3[, cols_vector], 
        type = "l", lty = 1, lwd = 3,
        col = c("black", "purple", "brown", "gray"),
        xlab = "Tiempo (días)", 
        ylab = "Población (Escala Log)",
        main = "Escenario 3: Sin Ilegal - Dinámica del Parásito",
        log = "y", # Escala logarítmica para ver todas las líneas
        cex.lab = 1.2, cex.main = 1.5)

legend("bottomright", 
       legend = c("Adultos (M)", "Huevos (Mo)", "Larvas (Ml)", "Pupas (Mp)"),
       col = c("black", "purple", "brown", "gray"), 
       lty = 1, lwd = 3, bty = "n", cex = 1.1)

# Cerrar PDF
dev.off()




