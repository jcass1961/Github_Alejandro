############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}


############################################################################
log.estim<-function(x,datos,ancho,Cte) EstimadorNucleoGama1(x,datos,ancho,Cte)


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.1<-function(x,datos,ancho,Cte) EstimadorNucleoGama1(x,datos,ancho,Cte)*log.estim(x,datos,ancho,Cte)

integrand.2<-function(x,datos,ancho,Cte) EstimadorNucleoGama1(x,datos,ancho,Cte)*log.estim(x,datos,ancho,Cte)^2


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
I1<-function(datos,ancho,Cte)
  adaptIntegrate(integrand.1,lower = 0.001, upper = 400,datos,ancho,Cte)$integral

I2<-function(datos,ancho,Cte)
  adaptIntegrate(integrand.2,lower = 0.001, upper = 400,datos,ancho,Cte)$integral