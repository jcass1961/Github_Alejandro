library(MASS)
library("stats4")
library(compiler)
library("cubature")
library("VGAM")
enableJIT(3)

############################################################################
#### DEFINE ESTIMADOR NUCLEO LOGNORMAL
EstimadorNucleoBS<-function(x,datos,b)
{
  n<-length(datos)
  EstNuBS<-1/n*sum(sapply(x,function(x) dbisa(datos,b^(1/2),x)))
  return(EstNuBS)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.BS<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoBS(x,datos,ancho))^2/(GI2(x,a,L)+EstimadorNucleoBS(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
DT.BS<-function(rango,a,L,datos,ancho) 
{
  rango[which.min(sapply(rango,function(a) 
    adaptIntegrate(integrand.DT.BS,lower = 0, upper = 100,a,L,datos,ancho)$integral))]
}

