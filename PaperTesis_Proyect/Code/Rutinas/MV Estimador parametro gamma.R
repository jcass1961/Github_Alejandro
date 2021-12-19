#####################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD    ####
#### DEL PARAMETRO GAMMA                         ####
#### CARGAR LIBRERIA stats4                      ####
#####################################################

gama.MV<- function(ga,datos,n,L)
{ 
  argLL<-argLL<-function(ga)
    -n*(log(gamma(L-(-ga-1)))-(-ga-1)*log(ga)-log(gamma(-(-ga-1))))+(L-1)*sum(log(datosGI))-(L-(-ga-1))*sum(log(ga+L*datosGI))
  MV<-mle(argLL, start = list(ga=1), method = "L-BFGS-B",lower =0.1,upper=7,
          nobs=NROW(datosGI))
  
  alfa.MV<-mle(argLL, start = list(a=-1), method = "L-BFGS-B",lower =-20,upper=-1.01,
               nobs=NROW(datos))
  return(alfa.MV)
}
