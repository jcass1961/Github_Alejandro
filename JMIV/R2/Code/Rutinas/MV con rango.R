#####################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD    ####
#### buscando el máximo en un rango              ####
#####################################################


MV.rango<-function(rango0,datos,n,L) 
{
  argLL0<-function(a,datos,n,L)
    (n*(log(gamma(L-a))-a*log(-a-1)-log(gamma(-a)))+(L-1)*sum(log(datos))-(L-a)*sum(log(-a-1+L*datos)))
  
  argLL<-function(a) argLL0(a,datos,n,L)
  alfa.MV.conrango<-rango0[which.max(sapply(rango0,argLL))]
}
