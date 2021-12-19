### REQUIERE PROGRAMA source("C:../../../../../Code/R/Rutinas/MV_Gama.R")

library("ADGofTest")

ADtest<-function(muestra)
{
  dimension<-dim(muestra)[2] 
  pvalue.ad<-rep(0,dimension)
  for (i in 1:dimension)
  {
    muest<-muestra[,i]
    m = mean(muest)
    emvalpha = emvalphaGamma(muest)
    emvbeta = m/emvalpha
    pvalue.ad[i]<-ad.test(muest, pgamma,emvalpha,1/emvbeta)$p.value
  }
  return("pvalue.ad"=pvalue.ad)
}

