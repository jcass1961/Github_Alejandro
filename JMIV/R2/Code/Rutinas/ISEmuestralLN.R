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
integrand.LN<-function(x,a,ga,L,datos,ancho,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoLN(x,datos,ancho,Cte))^2

############################################################################


############################################################################
#### DEFINE EL ISE
ISE.LN<-function(a,ga,L,datos,ancho,Cte) 
  adaptIntegrate(integrand.LN,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral
