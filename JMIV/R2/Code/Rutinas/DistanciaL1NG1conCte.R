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
integrand.L1.NG1<-function(x,a,L,datos,ancho,Cte) abs(GI2(x,a,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
L1.NG1.Cte<-function(a,L,datos,ancho,Cte)
  adaptIntegrate(integrand.L1.NG1,lower = 0, upper = 400,a,L,datos,ancho,Cte)$integral