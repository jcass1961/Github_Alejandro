############################################################################
#### DEFINE ESTIMADOR NUCLEO BOUNDARY KERNEL
EstimadorNucleoBK<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuBK<-dbckden(x, datos, lambda = ancho, bcmethod = "simple")
  return(EstNuBK)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR
integrand.DH0<-function(x,a,L,datos,ancho) sqrt((GI2(x,a,L)*EstimadorNucleoBK(x,datos,ancho)))

