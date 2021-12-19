
############################################################################
#### GENERA CONTAMINADOS
### GENERACIÓN DE BERNOULLI

ber<-function(epsilon,n)
{rber<-rbinom(n,1,epsilon)}

generoGI<-function(alfa1,gama,n,L)
{
  XM<-rgamma(n,L,L)
  YM<-rgamma(n,-alfa1,gama)
  datos<-XM/YM
}


##########################################
#Funciones para la contaminacion

genera_gi0_cont2<-function(alfa1,gama1,n,L,epsilon, corner)
{
  Gl<-generoGI(alfa1,gama1,n,L)
  
  e1<- rbinom(n,1,epsilon)
  datosc <- (1 - e1)* Gl + e1*corner
  return(datosc)
}

