############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 1

logcum.orden1.uniroot.all = function(muestra,L){
  
  tm <- length(muestra)
  
  k_hat_1 <- sum(log(muestra))/tm 
  
  f <- function(x)
  {k_hat_1  + log(L) - log(-x-1) - digamma(L) + digamma(-x)}
  
  alfa.LogCum.1 <- uniroot.all(f, c(-20,-1.01), tol = 0.0001)
  return(alfa.LogCum.1)
}

