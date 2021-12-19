############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 1

logcum.orden1 = function(muestra,gama,L){
  
  tm <- length(muestra)
  
  logcum.est.1 <- sum(log(muestra))/tm
  
  f <- function(x,est.1,gama,L)
  {est.1 + log(L) - log(gama) - digamma(L) + digamma(-x)}
  
  if (f(-20,logcum.est.1,gama,L)*f(-1.01,logcum.est.1,gama,L)<0)
    alfa.LogCum.1 <- uniroot(f, c(-20,-1.01), tol = 0.0001,logcum.est.1,gama,L)$root
  else
    alfa.LogCum.1 <- 0
  
  return(alfa.LogCum.1)
}

