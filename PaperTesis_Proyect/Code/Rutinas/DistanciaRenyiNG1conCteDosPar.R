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
integrand.DR.NG1.1<-function(x,a,ga,L,datos,ancho,Cte,beta) 
  GI0.alfagama(x,a,ga,L)^beta*EstimadorNucleoGama1(x,datos,ancho,Cte)^(1-beta)

integrand.DR.NG1.2<-function(x,a,ga,L,datos,ancho,Cte,beta) 
  GI0.alfagama(x,a,ga,L)^(1-beta)*EstimadorNucleoGama1(x,datos,ancho,Cte)^beta

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON LOG NORMAL
DR.NG1.Cte.1<-function(a,ga,L,datos,ancho,Cte,beta) 
adaptIntegrate(integrand.DH.NG1.1,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte,beta)$integral

DR.NG1.Cte.2<-function(a,ga,L,datos,ancho,Cte,beta) 
  adaptIntegrate(integrand.DH.NG1.2,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte,beta)$integral

DR.NG1.Cte<-function(a,ga,L,datos,ancho,Cte,beta)
            1/(2*(beta-1))*log(DR.NG1.Cte.1+DR.NG1.Cte.2)