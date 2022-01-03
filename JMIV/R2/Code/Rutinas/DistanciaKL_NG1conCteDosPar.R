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
integrand.Renyi.NG1<-function(x,a,ga,L,datos,ancho,Cte,beta) (1/2*beta-1)*log(GI0.alfagama(x,a,ga,L)*log(GI0.alfagama(x,a,ga,L)/EstimadorNucleoGama1(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
Renyi.NG1.Cte<-function(a,ga,L,datos,ancho,Cte) 
1-adaptIntegrate(integrand.KL.NG1,lower = -400, upper = 400,a,ga,L,datos,ancho,Cte)$integral

