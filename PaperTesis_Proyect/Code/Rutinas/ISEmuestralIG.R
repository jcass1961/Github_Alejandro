############################################################################
#### DEFINE ESTIMADOR NUCLEO INVERSO GAUSSIANO
EstimadorNucleoIG<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuIG<-1/(n*Cte)*sum(sapply(x,function(x) dinvgauss(datos,mean=x,shape=1/ancho)))
  return(EstNuIG)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.IG<-function(x,a,ga,L,datos,ancho,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoIG(x,datos,ancho,Cte))^2

############################################################################


############################################################################
#### DEFINE EL ISE
ISE.IG<-function(a,ga,L,datos,ancho,Cte) 
  adaptIntegrate(integrand.IG,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral
