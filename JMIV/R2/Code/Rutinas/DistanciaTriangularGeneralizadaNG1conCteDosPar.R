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
integrand.DT.NG1<-function(x,alfa,ga,s,L,datos,ancho,Cte) 
  abs(GI0.alfagama(x,alfa,ga,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^s/(GI0.alfagama(x,alfa,ga,L)+EstimadorNucleoGama1(x,datos,ancho,Cte))^(s-1)

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
DT.NG1.Cte.Gen<-function(alfa,ga,s,L,datos,ancho,Cte)
  adaptIntegrate(integrand.DT.NG1,lower = 0, upper = 400,alfa,ga,s,L,datos,ancho,Cte)$integral