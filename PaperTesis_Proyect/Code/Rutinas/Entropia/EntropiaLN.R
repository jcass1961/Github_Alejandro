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
integrand.Entr.LN<-function(x,datos,ancho,Cte) (EstimadorNucleoLN(x,datos,ancho,Cte))*log(EstimadorNucleoLN(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
Ent.LN.Cte<-function(a,L,datos,ancho,Cte) 
adaptIntegrate(integrand.Entr.LN,lower = 0, upper = 400,a,L,datos,ancho,Cte)$integral

