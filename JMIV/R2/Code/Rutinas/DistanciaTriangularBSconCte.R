library(MASS)
library("stats4")
library(compiler)
library("cubature")
library("Conake")
enableJIT(3)

############################################################################
#### DEFINE ESTIMADOR NUCLEO LOGNORMAL
EstimadorNucleoBS<-function(x,datos,b,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dcbs(datos,  b^(1/2) , x)))
  return(EstNuGa)
}



############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.BS<-function(x,a,L,datos,ancho,Cte) (GI2(x,a,L)-EstimadorNucleoBS(x,datos,ancho,Cte))^2/(GI2(x,a,L)+EstimadorNucleoBS(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
DT.BS.Cte<-function(a,L,datos,ancho,Cte) 
adaptIntegrate(integrand.DT.BS,lower = 0, upper = 400,a,L,datos,ancho,Cte)$integral

