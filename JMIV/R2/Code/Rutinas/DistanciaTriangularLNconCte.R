library(MASS)
library("stats4")
library(compiler)
library("cubature")
library("Conake")
enableJIT(3)

############################################################################
#### DEFINE ESTIMADOR NUCLEO LOGNORMAL
EstimadorNucleoLN<-function(x,datos,b,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dlnorm(datos,log(x)+b^2,b)))
  return(EstNuGa)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.LN<-function(x,a,L,datos,ancho,Cte) (GI2(x,a,L)-EstimadorNucleoLN(x,datos,ancho,Cte))^2/(GI2(x,a,L)+EstimadorNucleoLN(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
DT.LN.Cte<-function(a,L,datos,ancho,Cte) 
adaptIntegrate(integrand.DT.LN,lower = 0, upper = 400,a,L,datos,ancho,Cte)$integral

