############################################################################
##### PARA GENERAR LA BASE DE DATOS

######################################################################################################
############################################################################
############################################################################
##### PROGRAMA PRINCIPAL
estimadores.tiempoMV<-function(muestra,L,ker)
{
  #browser()
  
  datosGI<-muestra
  n<-length(muestra)
  
  if (alfa.mom0(datosGI,L)[1]!=0) 
  {x0<-alfa.mom0(datosGI,L)
  alfa.ini<-x0[1]
  gama.ini<-x0[2]*(-x0[1]-1)
  } else 
  {alfa.ini<--1.5
  gama.ini<-mean(datosGI)*(-(-1.5)-1)
  }
  #print(gama.ini)
  
  # if(ker=="IGJstar") b.IGJstar<-1/(5*sqrt(n))
  # else  b<-cv(datosGI,ker=ker)$hcv
  LogLike0 <- function(alfa,gama) LogLike(alfa,gama, n,L,datosGI)
  Loglike1<-function(x) LogLike0(x[1],x[2])
  
  MV<-optim(c(alfa.ini,gama.ini),Loglike1, 
      method = "L-BFGS-B",lower = c(-20,1/gama.ini), upper = c(-10^(-3),10*gama.ini))
  
  salida.alfas<-data.frame(L=L,alfa.MV=MV[1],gama.MV=MV[2])
  #DT=salida.alfas<-c(L,alfa,n,alfa.DPDT.GA,tiempo.DT,b))
  
  
  
  return(salida.alfas)
  #return(base1)
}