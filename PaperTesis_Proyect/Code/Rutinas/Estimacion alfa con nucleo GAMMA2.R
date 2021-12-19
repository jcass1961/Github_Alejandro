############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA2
EstimadorNucleoGama2<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuGa2<-1/n*sum(sapply(x,function(x) dgamma2(datos,x,ancho)))
  return(EstNuGa2)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA2
integrand.DT.NG2<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoGama2(x,datos,ancho))^2/
                  (GI2(x,a,L)+EstimadorNucleoGama2(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO GAMMA2
DT.NG2<-function(rango,a,L,datos,ancho) 
{
  rango[which.min(sapply(rango,function(a) 
    adaptIntegrate(integrand.DT.NG2,lower = 0, upper = 100,a,L,datos,ancho)$integral))]
}