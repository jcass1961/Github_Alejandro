############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 1

logcum.orden1 = function(muestra,L){
  
  tm <- length(muestra)
  
  logcum.est.1 <- sum(log(muestra))/tm
  
  f <- function(x,est.1,L)
  {est.1 + log(L) - log(-x-1) - digamma(L) + digamma(-x)}
  
  if (f(-20,logcum.est.1,L)*f(-1.01,logcum.est.1,L)<0)
    alfa.LogCum.1 <- uniroot(f, c(-20,-1.01), tol = 0.0001,logcum.est.1,L)$root
  else
    alfa.LogCum.1 <- 0
  
  return(alfa.LogCum.1)
}

