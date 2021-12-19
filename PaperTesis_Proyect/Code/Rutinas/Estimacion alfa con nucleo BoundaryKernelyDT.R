############################################################################
#### DEFINE ESTIMADOR NUCLEO BOUNDARY KERNEL
EstimadorNucleoBK<-function(x,datos,ancho,ker)
{
  n<-length(datos)
  EstNuBK<-dbckden(x, datos, lambda = ancho, bcmethod = "simple",kernel=ker)
  return(EstNuBK)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BK<-function(x,a,L,datos,ancho,ker) (GI2(x,a,L)-EstimadorNucleoBK(x,datos,ancho,ker))^2/
                  (GI2(x,a,L)+EstimadorNucleoBK(x,datos,ancho,ker))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BK<-function(rango,a,L,datos,ancho,ker) 
{
  rango[which.min(sapply(rango,function(a) 
        integrate(integrand.DT.BK,lower = 0, upper = 400,a,L,datos,ancho,ker)$integral))]
}