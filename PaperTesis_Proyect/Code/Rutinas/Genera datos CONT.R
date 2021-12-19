
############################################################################
#### GENERA CONTAMINADOS
### GENERACIÓN DE BERNOULLI

ber<-function(epsilon,n)
{rber<-rbinom(n,1,epsilon)}

genero.cont<-function(alfa,n,L,k,epsilon)
{
  datos.ber<-ber(epsilon,n)
  datosGI<-generoGI(alfa,n,L,0)
  datosGI.cont<-generoGI(alfa,n,L,k) 
  datos.cont<-(1-datos.ber)*datosGI+datos.ber*datosGI.cont
}

generoGI<-function(alfa,n,L,k)
{
  #Ga<-(L*gamma(-alfa)*gamma(L))/(gamma(-alfa-1)*gamma(L+1))### esto es para que tenga esperanza 1
  Ga<--alfa-1
  XM<-rgamma(n,L,L)
  YM<-rgamma(n,-alfa,10^k*Ga)
  datos<-XM/YM
}


##########################################
#Funciones para la contaminacion
uno<-function(alfa1,alfa2, n, L, epsilon)
{
  GIl<-generoGI(alfa1,n,L,0)
  GIc<-generoGI(alfa2, n, L,0)
  
  e1<- rbinom(n,1,epsilon)
  
  datosc<- (1 - e1)* GIl + e1*GIc
  return(datosc)
}

dos=function(alfa1,n,L,epsilon, corner)
{
  Gl<-generoGI(alfa1,n,L,0)
  
  e1<- rbinom(n,1,epsilon)
  datosc <- (1 - e1)* Gl + e1*corner
  return(datosc)
}

tres<-function(alfa1,n,L,k,epsilon)
{datosc<-genero.cont(alfa1,n,L,k,epsilon)
return(datosc)
}


genera_gi0_cont2<-function(caso, alfa1,alfa2, n, L, epsilon, corner,k)
  # caso = "uno","dos", "tres" caso de contaminaci\'on.
  # caso =1  contaminacion de tipo (1-epsilon)* GI0(alfa1, gamma tal que esperanza es 1, L) + epsilon *GI0(alfa2, gamma_tal_que_esperanza = 1, L)
  # caso = 2 una proporci\'on epsilon de valores arbitrariamente grandes, con valor corner
  # caso = 3 contaminación del parámetro gamma con probabilidad epsilon
  
{
  switch(caso,
         uno=uno(alfa1,alfa2,n,L,epsilon),
         dos=dos(alfa1,n,L,epsilon, corner),
         tres=tres(alfa1,n,L,k,epsilon))
}


