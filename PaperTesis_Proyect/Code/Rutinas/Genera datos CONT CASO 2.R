
############################################################################
#### GENERA CONTAMINADOS
generoGI<-function(alfa,n,L,k)
{
  #Ga<-(L*gamma(-alfa)*gamma(L))/(gamma(-alfa-1)*gamma(L+1))### esto es para que tenga esperanza 1
  Ga<--alfa-1
  XM<-rgamma(n,L,L)
  YM<-rgamma(n,-alfa,10^k*Ga)
  datos<-XM/YM
}


genera_gi0_caso2<-function(alfa1, n, L, epsilon, corner)
{
  Gl<-generoGI(alfa1,n,L,0)
  
  e1<- rbinom(n,1,epsilon)
  datosc <- (1 - e1)* Gl + e1*corner
  return(datosc)
}
  

