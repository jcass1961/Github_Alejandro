###### REQUIERE LIBRERIA EVMIX


############################################################################
#### DEFINE ESTIMADOR NUCLEO BOUNDARY KERNEL
EstimadorNucleoBK.EPA<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuBK<-dbckden(x, datos, lambda = ancho, bcmethod = "simple",kernel="epanechnikov")
  return(EstNuBK)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BK.EPA<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoBK.EPA(x,datos,ancho))^2/
                  (GI2(x,a,L)+EstimadorNucleoBK.EPA(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BK.EPA<-function(rango,a,L,datos,ancho) 
{
  rango[which.min(sapply(rango,function(a) 
        integrate(integrand.DT.BK.EPA,lower = 0, upper = 400,a,L,datos,ancho)$integral))]
}