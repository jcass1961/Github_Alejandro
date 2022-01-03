library(MASS)
library("stats4")
library(compiler)
library("cubature")
library("Conake")
enableJIT(3)

############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}



############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DH.NG1<-function(x,a,ga,L,datos,ancho,Cte) sqrt(GI0.alfagama(x,a,ga,L)*EstimadorNucleoGama1(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
DH.NG1.Cte<-function(a,ga,L,datos,ancho,Cte) 
1-adaptIntegrate(integrand.DH.NG1,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral

