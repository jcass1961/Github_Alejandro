
############################################################################
#### DEFINE PARAMETRO DE FORMA PARA NUCLEO GAMMA SEGUNDA VERSION
ro.b <- function(x,ancho) 
{
  y <- ifelse(x >=2*ancho, x/ancho,1/4*(x/ancho)^2+1)
  return(y) 
}


############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA OPCION 2
EstimadorNucleoGama2<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=ro.b(x,ancho),scale=ancho)))
  return(EstNuGa)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 2
integrand.ISE.GA2<-function(x,a,ga,L,datos,ancho,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoGama2(x,datos,ancho,Cte))^2

############################################################################
#### DEFINE EL ISE
ISE.GA2<-function(a,ga,L,datos,ancho,Cte) 
  adaptIntegrate(integrand.ISE.GA2,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral


