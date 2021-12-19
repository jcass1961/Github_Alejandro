#############################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD            ####
#### para el parámetro alfa de una distribución GI0      ####
#### bajo la condición de E(Z)=1, donde Z es el retorno, #### 
#### gamma=-alfa-1                                       ####
#### CARGAR LIBRERIA stats4                              ####
#############################################################

### Parametro xo punto inicial
library("stats4")

MV.x0<- function(a,datos,n,L,x0)
{ argLL0<-function(a,datos,n,L)
  -(n*(log(gamma(L-a))-a*log(-a-1)-log(gamma(-a)))+(L-1)*sum(log(datos))-(L-a)*sum(log(-a-1+L*datos)))
  
  argLL<-function(a) argLL0(a,datos,n,L)
  alfa.MV<-mle(argLL, start = list(a=x0), method = "L-BFGS-B",lower =-20,upper=-1.01,
               nobs=NROW(datos))
  return(alfa.MV)
}
