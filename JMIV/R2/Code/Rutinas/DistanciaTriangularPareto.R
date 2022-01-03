############################################################################
#### DEFINE ESTIMADOR NUCLEO Pareto VERSION 1
EstimadorNucleoPAR<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuPar<-1/n*sum(sapply(x,function(x) dpareto(datos,location=x,shape=1/ancho)))
  return(EstNuPar)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO Pareto VERSION 1
integrand.DT.PAR<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoPAR(x,datos,ancho))^2/(GI2(x,a,L)+EstimadorNucleoPAR(x,datos,ancho))

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO Pareto VERSION 1
DT.PAR<-function(a,L,datos,ancho)
  adaptIntegrate(integrand.DT.PAR,lower = 0, upper = 400,a,L,datos,ancho)$integral