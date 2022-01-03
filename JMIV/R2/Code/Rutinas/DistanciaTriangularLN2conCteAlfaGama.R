library(MASS)
library("stats4")
library(compiler)
library("cubature")
library("Conake")
enableJIT(3)

############################################################################
#### DEFINE ESTIMADOR NUCLEO LOGNORMAL
EstimadorNucleoLN2<-function(x,datos,b,Cte)
{
  n<-length(datos)
  EstNuLN2<-1/(n*Cte)*sum(sapply(x,function(x) dlnorm(datos,log(x),2*sqrt(log(1+b)))))
  return(EstNuLN2)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.LN2<-function(x,a,ga,L,datos,ancho,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoLN2(x,datos,ancho,Cte))^2/(GI0.alfagama(x,a,ga,L)+EstimadorNucleoLN2(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
DT.LN2.Cte2Par<-function(a,ga,L,datos,ancho,Cte) 
adaptIntegrate(integrand.DT.LN2,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral

