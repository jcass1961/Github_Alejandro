KS_ADtest<-function(muestra)
{
  m = mean(muestra)
  emvalpha = emvalphaGamma(muestra)
  emvbeta = m/emvalpha
  emvalpha
  emvbeta
  
  pvalue.smirnov<-ks.test(muestra, "pgamma",emvalpha,1/emvbeta,alternative = "two.sided")$p.value
  pvalue.ad<-ad.test(muestra, pgamma,emvalpha,1/emvbeta)$p.value
  
  return(list("pvalue.ad"=pvalue.ad,"pvalue.smirnov"=pvalue.smirnov))
}


