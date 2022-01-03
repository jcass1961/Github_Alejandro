############################################################################
#### DEFINE ESTIMADOR NUCLEO BOUNDARY KERNEL
EstimadorNucleoBK<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuBK<-dbckden(x, datos, lambda = ancho, bcmethod = "simple")
  return(EstNuBK)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BKpeso<-function(x,a,L,datos,ancho,k) (GI2(x,a,L)-EstimadorNucleoBK(x,datos,ancho))^2*exp(-x/k)/
                  (GI2(x,a,L)+EstimadorNucleoBK(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BKpeso<-function(rango,a,L,datos,ancho,k) 
{
  rango[which.min(sapply(rango,function(a) 
        integrate(integrand.DT.BKpeso,lower = 0, upper = 400,a,L,datos,ancho,k)$integral))]
}