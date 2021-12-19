####################################
#### GENERA UNA DISTRIBUCION GI ####
####################################

generoGI.alfagama<-function(alfa,gama,n,L)
{
  XM<-rgamma(n,L,L)
  YM<-rgamma(n,-alfa,gama)
  datos<-XM/YM
}