#####################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD    ####
#### CARGAR LIBRERIA stats4                      ####
#####################################################

MV.congama<- function(a,datos,gama,n,L)
{ argLL0<-function(a,datos,gama,n,L)
  -(n*(log(gamma(L-a))-a*log(gama)-log(gamma(-a)))+(L-1)*sum(log(datos))-(L-a)*sum(log(gama+L*datos)))
  
  argLL<-function(a) argLL0(a,datos,gama,n,L)
  alfa.MV<-mle(argLL, start = list(a=-1), method = "L-BFGS-B",lower =-20,upper=-1.01,
               fixed = list(gama,n,L), nobs=NROW(datos))
  return(alfa.MV)
}
