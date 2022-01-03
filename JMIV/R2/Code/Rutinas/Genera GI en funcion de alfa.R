####################################
#### GENERA UNA DISTRIBUCION GI ####
####################################

generoGI<-function(alfa,n,L)
{
  Ga<-(L*gamma(-alfa)*gamma(L))/(gamma(-alfa-1)*gamma(L+1))
  XM<-rgamma(n,L,L)
  YM<-rgamma(n,-alfa,Ga)
  datos<-XM/YM
}