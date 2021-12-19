############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuGa<-1/n*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.NG1<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoGama1(x,datos,ancho))^2/(GI2(x,a,L)+EstimadorNucleoGama1(x,datos,ancho))

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
DT.NG1<-function(a,L,datos,ancho)
  adaptIntegrate(integrand.DT.NG1,lower = 0, upper = 400,a,L,datos,ancho)$integral