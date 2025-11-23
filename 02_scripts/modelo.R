
##### Modelo miasis por gusano barrenador (bases)

miasis<-function(t,state,pars){
  with(as.list(c(state, pars)), { 
    
    #Población hospedantes
    
    NV<- Sv + Ev + Iv + Rv

    
    # Hospedante
    
    dIv<- sigma*S*(M/NV) -beta*Ev -gamma*Iv + delta*ML*Iv - miuv*Iv
    dSv<- natalidad + (1-p) + p - sigma*(Sv(M/NV)) - miuv * Sv +tetha*Rv
    dR <- gamma * Iv - miuv * Rv + tetha*Rv
    dE<-  sigma*S*(M/NV)- beta*Ev- miuv*Ev
    
    ##vector
    
    dM<-epsilon * MP-(alpha* M) - mium*M
    dMo<-alpha*M -(eta + mium)*Mo
    dMl<-eta*Mo-(kappa+mium)*Ml
    dMp<-kappa*Ml-(epsilon+mium)*mp
    
    list(c(dIv,dSv,dRv,dEv,dM,dMo,dMl,dMp))
  })
}

pars<-c(sigma = ,beta= , gamma= , delta= ,miuv= ,natalidad= , p= , tetha= , epsilon= ,
        mium= ,kappa= , eta= , alpha= )

t<-seq(0,100, by=0.01)
state<-c(Iv= , Ev= , Iv= ,Rv= , M= , Mo= , Ml= , Mp= )

#ecuaciones diferenciales

out1=ode(y = state, times = t, func = miasis, parms = pars)
plot(out1,col="purple")