
#####################################
#### Define UNA DISTRIBUCION GI #####
#### PARAMETROS: alfa, gamma y L ####
#####################################
#### Define UNA DISTRIBUCION GI #####
#### PARAMETROS: alfa, gamma y L ####
#####################################

kerGI0<-function(z,xi,b,L)
{
  alfap<--b/xi-1
  gamap<-b
  L^L*gamma(L - alfap)/((gamap)^alfap*gamma(L)*gamma(-alfap))*z^( L - 1)/((gamap) + L*z)^(L - alfap)}


############################################################################
#### DEFINE ESTIMADOR NUCLEO GI0
EstimadorNucleoGI0<-function(z,datos,ancho,L)
{
  n<-length(datos)
  EstNuGI0<-1/n*sum(sapply(z,function(z) kerGI0(z,datos,ancho,L)))
  return(EstNuGI0)
}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GI0
integrand.DT.GI0<-function(z,alfat,datos,ancho,L) (GI2(z,alfat,L)-EstimadorNucleoGI0(z,datos,ancho,L))^2/
                  (GI2(z,alfat,L)+EstimadorNucleoGI0(z,datos,ancho,L))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO GI0
DT.GI0<-function(rango,alfat,L,datos,ancho) 
{
  rango[which.min(sapply(rango,function(a) 
    adaptIntegrate(integrand.DT.GI0,lower = 0, upper = 400,alfat,L,datos,ancho)$integral))]
}