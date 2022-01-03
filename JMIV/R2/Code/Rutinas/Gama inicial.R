#########################################
####      DEFINE GAMMA INICIAL       ####
#########################################

gama.inic<-function(a,datos){
  n=length(datos) 
  L=1
  alfa= MV(a,datos,n,L)@coef[[1]]  
  gama= -1-alfa
  return(gama)
}