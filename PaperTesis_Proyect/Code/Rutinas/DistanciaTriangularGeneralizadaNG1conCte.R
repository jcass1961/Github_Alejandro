############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.DT.NG1<-function(x,s,L,datos,ancho,Cte,alfa) abs(GI2(x,alfa,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^s/(GI2(x,alfa,L)+EstimadorNucleoGama1(x,datos,ancho,Cte))^(s-1)

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
DT.NG1.Cte.Gen<-function(s,L,datos,ancho,Cte,alfa)
  adaptIntegrate(integrand.DT.NG1,lower = 0, upper = 400,s,L,datos,ancho,Cte,alfa)$integral