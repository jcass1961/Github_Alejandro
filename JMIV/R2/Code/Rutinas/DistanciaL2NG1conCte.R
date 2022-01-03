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
integrand.L2.NG1<-function(x,a,L,datos,ancho,Cte) (GI2(x,a,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^2

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
L2.NG1.Cte<-function(a,L,datos,ancho,Cte)
  adaptIntegrate(integrand.L2.NG1,lower = 0, upper = 400,a,L,datos,ancho,Cte)$integral