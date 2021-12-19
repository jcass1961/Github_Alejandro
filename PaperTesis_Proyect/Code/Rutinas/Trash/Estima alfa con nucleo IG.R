############################################################################
### CALCULA LA DISTANCIA TRIANGULAR ENTRE LA GI0 Y LA ESTIMACION NO      ###
### PARAMETRICA CON NUCLEO INVERSO GAUSSIANO Y ANCHO DE BANDA DADO POR   ###
### JSTAR b<-1/(5*sqrt(n))                                               ###
###                                                                      ###
### REQUIERE library("statmod")                                          ###
############################################################################

############################################################################
#### DEFINE ESTIMADOR NUCLEO INVERSO GAUSSIANO
EstimadorNucleoIG<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuIG<-1/n*sum(sapply(x,function(x) dinvgauss(datos,mean=x,shape=1/ancho)))
  return(EstNuIG)
}

############################################################################
#### DEFINO LA FUNCION A INTEGRAR
integrand.DT0<-function(x,a,L,datos,ancho) 
  (GI2(x,a,L)-EstimadorNucleoIG(x,datos,ancho))^2/(GI2(x,a,L)+EstimadorNucleoIG(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR
DT.IG<-function(rango,a,L,datos,ancho) 
{
  rango[which.min(sapply(rango,function(a) 
    adaptIntegrate(integrand.DT0,lower = 0.1, upper = 100,a,L,datos,ancho)$integral))]
}
