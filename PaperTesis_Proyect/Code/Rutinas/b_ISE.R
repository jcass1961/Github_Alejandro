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
integrand.ISE.NG1<-function(x,ancho,a,ga,L,datos,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^2

############################################################################
#### DEFINO ISE en funcion de b
ISE<-function(ancho,a,ga,L,datos,Cte)
  adaptIntegrate(integrand.ISE.NG1,lower = 0, upper = 400,ancho,a,ga,L,datos,Cte)$integral


