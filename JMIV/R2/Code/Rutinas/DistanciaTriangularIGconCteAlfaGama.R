############################################################################
### CALCULA LA DISTANCIA TRIANGULAR ENTRE LA GI0 Y LA ESTIMACION NO      ###
### PARAMETRICA CON NUCLEO INVERSO GAUSSIANO Y ANCHO DE BANDA DADO POR   ###
### JSTAR b<-1/(5*sqrt(n))                                               ###
###                                                                      ###
### REQUIERE library("statmod")    límite inferior=0                     ###
############################################################################

############################################################################
#### DEFINE ESTIMADOR NUCLEO INVERSO GAUSSIANO
EstimadorNucleoIG<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuIG<-1/(n*Cte)*sum(sapply(x,function(x) dinvgauss(datos,mean=x,shape=1/ancho)))
  return(EstNuIG)
}

############################################################################
#### DEFINO LA FUNCION A INTEGRAR
integrand.DT.IG<-function(x,a,ga,L,datos,ancho,Cte) 
  (GI0.alfagama(x,a,ga,L)-EstimadorNucleoIG(x,datos,ancho,Cte))^2/(GI0.alfagama(x,a,ga,L)+EstimadorNucleoIG(x,datos,ancho,Cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR
DT.IG.Cte2Par<-function(a,ga,L,datos,ancho,Cte)
  adaptIntegrate(integrand.DT.IG,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral