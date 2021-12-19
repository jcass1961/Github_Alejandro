############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1Jeon<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuGa<-1/n*sum(sapply(x,function(x) dgamma(x,shape=datos/ancho+1,scale=ancho)))
  return(EstNuGa)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.NG1Jeon<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoGama1Jeon(x,datos,ancho))^2/(GI2(x,a,L)+EstimadorNucleoGama1Jeon(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO GAMMA VERSION 1 y OPTIM
DT.NG1Jeon<-function(a,L,datos,ancho) 
  adaptIntegrate(integrand.DT.NG1Jeon,lower = 0, upper = 400,a,L,datos,ancho)$integral

