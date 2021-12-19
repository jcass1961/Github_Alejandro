######################################################################################################
############################################################################
############################################################################
##### PROGRAMA PRINCIPAL
estima.unpar<-function(muestra,L)
{
  #browser()
  
  datosGI<-muestra
  
  if (mom_1_2(datosGI,L)!=0) x0<-mom_1_2(datosGI,L)
  else x0<--1.5
  
  b.GA<-cv(datosGI,ker="GA")$hcv
  b.LN<-cv(datosGI,ker="LN")$hcv
  
  const.GA<-Conakereport2(datosGI,ker="GA",nx=100)$C_n
  const.LN<-Conakereport2(datosGI,ker="LN",nx=100)$C_n

 f2.GA<-function(a) DT.NG1.Cte(a,L,datosGI,b.GA,const.GA)
 f2.LN<-function(a) DT.LN.Cte(a,L,datosGI,b.LN,const.LN)
 
 n<-length(muestra)
 alfa.MV<-MV(a,datosGI,n,L)@coef[[1]]
  
 alfa.DT.GA<-optim(x0,f2.GA, 
                              method = "L-BFGS-B",lower = -20, upper = -1.00000001)$par
 alfa.DT.LN<-optim(x0,f2.LN, 
                              method = "L-BFGS-B",lower = -20, upper = -1.00000001)$par
 alfa.LC<-logcum.orden1(datosGI,L)

salida.alfas<-data.frame(L=L,alfa.MV=alfa.MV,alfa.GA=alfa.DT.GA,alfa.LN=alfa.DT.LN,alfa.LC=alfa.LC)
  return(salida.alfas)
  #return(base1)
}
