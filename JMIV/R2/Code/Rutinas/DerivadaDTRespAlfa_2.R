############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}

############################################################################
#### Derivada DT respecto de alfa

derivGIalfa<-function(z,alfa,L) 
{
  (L^L*z^(-1 + L)*(-1 - alfa)^(-alfa)*(-1 + L*z - alfa)^(-L + alfa)*
     gamma(L - alfa)*(-(alfa/(1 + alfa)) - (L - alfa)/(1 - L*z + alfa) - log(-1 - alfa) + 
                        log(-1 + L*z - alfa) - digamma(L - alfa) + digamma(-alfa)))/(gamma(L)*gamma(-alfa))
}

derivDTalfa<-function(x,alfa,L,datos,ancho,Cte) 
{
  derivGIalfa(x,alfa,L)*
    (2*(GI2(x,alfa,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))/(GI2(x,alfa,L)+EstimadorNucleoGama1(x,datos,ancho,Cte))-
    (GI2(x,alfa,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^2/(GI2(x,alfa,L)+EstimadorNucleoGama1(x,datos,ancho,Cte))^2)
}

############################################################################
#### DEFINO LA FUNCION A INTEGRAL DE LA DERIVADA DE LA DISTANCIA TRIANGULAR RESPECTO
#### DE ALFA


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
Int.Deriv.DT.ALFA<-function(alfa,L,datos,ancho,Cte)
  adaptIntegrate(derivDTalfa,lower = 0, upper = 400,alfa,L,datos,ancho,Cte)$integral




