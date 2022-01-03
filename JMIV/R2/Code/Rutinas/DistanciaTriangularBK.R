###### REQUIERE LIBRERIA EVMIX


############################################################################
#### DEFINE ESTIMADOR NUCLEO BOUNDARY KERNEL
EstimadorNucleoBK<-function(x,datos,ancho,nucleo)
{
  n<-length(datos)
  EstNuBK<-dbckden(x, datos, lambda = ancho, bcmethod = "simple",kernel=nucleo)
  return(EstNuBK)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BK<-function(x,a,L,datos,ancho,nucleo) (GI2(x,a,L)-EstimadorNucleoBK(x,datos,ancho,nucleo))^2/
                  (GI2(x,a,L)+EstimadorNucleoBK(x,datos,ancho,nucleo))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BK<-function(a,L,datos,ancho,nucleo) 
  adaptIntegrate(integrand.DT.BK,lower = 0, upper = 400,a,L,datos,ancho,nucleo)$integral