#### REQUIERE LIBRERIA 

library("EnvStats")

############################################################################
#### DEFINE ESTIMADOR NUCLEO Pareto VERSION 1
EstimadorNucleoPAR<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuPar<-1/(n*Cte)*sum(sapply(x,function(x) dpareto(datos,location=x,shape=1/ancho)))
  return(EstNuPAR)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO PAR VERSION 1
integrand.DT.PAR<-function(x,a,ga,L,datos,ancho,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoPAR(x,datos,ancho,Cte))^2/(GI0.alfagama(x,a,ga,L)+EstimadorNucleoPAR(x,datos,ancho,Cte))

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO PAR VERSION 1
DT.PAR.Cte<-function(a,ga,L,datos,ancho,Cte)
  adaptIntegrate(integrand.DT.PAR,lower = 0, upper = 400,a,ga,L,datos,ancho,Cte)$integral