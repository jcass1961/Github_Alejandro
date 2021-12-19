#####################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD    ####
#### CARGAR LIBRERIA stats4                      ####
#####################################################

MV<- function(a,datos,n,L)
{ argLL0<-function(a,datos,n,L)
  -(n*(log(gamma(L-a))-a*log(-a-1)-log(gamma(-a)))+(L-1)*sum(log(datos))-(L-a)*sum(log(-a-1+L*datos)))
  
  argLL<-function(a) argLL0(a,datos,n,L)
  alfa.MV<-mle(argLL, start = list(a=-1), method = "L-BFGS-B",lower =-20,upper=-1.01,
               nobs=NROW(datos))
  return(alfa.MV)
}
