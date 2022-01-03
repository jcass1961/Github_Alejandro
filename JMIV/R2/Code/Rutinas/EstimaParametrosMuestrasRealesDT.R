############################################################################
##### PARA GENERAR LA BASE DE DATOS

######################################################################################################
############################################################################
############################################################################
##### PROGRAMA PRINCIPAL
estimadores.tiempoDT<-function(muestra,L,ker)
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
  b<-boptim(alfa.ini,gama.ini,L,n)
  
  const<-Conakereport2(datosGI,ker=ker,h=b,nx=100,a = 0, b = 1)$C_n
  DosPar.DTNG1<-function(a,ga) DT.NG1.Cte2Par(a,ga,L,datosGI,b,const)
  
  #gama<--alfa-1
  
  f2.DPDT<-function(x) DosPar.DTNG1(x[1],x[2])
  
  DosP<-optim(c(alfa.ini,gama.ini),f2.DPDT, 
        method = "L-BFGS-B",lower = c(-20,1/gama.ini), upper = c(-10^(-3),10*gama.ini))
  
  salida.alfas<-data.frame(L=L,alfa.DT=DosP[1],gama.DT=DosP[2])
  #DT=salida.alfas<-c(L,alfa,n,alfa.DPDT.GA,tiempo.DT,b))
  
  
  
  return(salida.alfas)
  #return(base1)
}