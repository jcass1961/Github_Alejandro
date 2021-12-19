#############################################################
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD            ####
#### para los parámetro alfa y gamma                     ####
#### de una distribución GI0                             #### 
#### CARGAR LIBRERIA stats4                              ####
#############################################################


library("stats4")

MVAlfaGama<- function(datos,n,L)
{
  alfa1<-function(ga) L*sum(1/(ga+datos*L))/(-n/ga+sum(1/(ga+datos*L)))
  
  f<-function(ga) -n*digamma(L-alfa1(ga))-n*log(ga)+n*digamma(-alfa1(ga))+sum(log(ga+datos*L))
  if (f(10^(-7))*f(100)<0)
  {gama.MV <- uniroot(f, c(10^(-7),100), tol = 0.0001)$root }
  else {gama.MV<-0}
  
  alfa.MV<-alfa1(gama.MV)
  salidaDosPar<-c(alfa.MV,gama.MV)
  return(salidaDosPar)
}


