############################################################################
#### DEFINE ESTIMADOR NUCLEO Pareto 
EstimadorNucleoPAR<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  #EstNuPar<-1/(n*Cte)*sum(sapply(x,function(x) dpareto(datos,location=x,shape=1/ancho)))
  EstNuPar<-1/(n*Cte)*sum(sapply(x,function(x) dpareto(datos,location=x,shape=1/ancho)))
  return(EstNuPar)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO Pareto 
integrand.DT.PAR<-function(x,a,L,datos,ancho,Cte) (GI2(x,a,L)-EstimadorNucleoPAR(x,datos,ancho,Cte))^2/(GI2(x,a,L)+EstimadorNucleoPAR(x,datos,ancho,Cte))

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO Pareto
DT.PAR.Cte<-function(a,L,datos,ancho,Cte)
  adaptIntegrate(integrand.DT.PAR,lower = 0, upper = 400,a,L,datos,ancho,Cte)$integral