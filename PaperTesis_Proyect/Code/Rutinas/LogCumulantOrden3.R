############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 2

logcum.orden3 = function(muestra, L){
  
  logcum.est.1 <- mean(log(muestra))
  logcum.est.3 <- mean((log(muestra)-logcum.est.1)^3)
  
  f <- function(x,est.3,L)
    {est.3 - psigamma(L, deriv = 2) - psigamma(-x, deriv = 2)}
  
  if (f(-20,logcum.est.3,L)*f(-1.01,logcum.est.3,L)<0)
    alfa.LogCum.3 <- uniroot(f, c(-20,-1.01), tol = 0.0001,logcum.est.3,L)$root
  else
    alfa.LogCum.3 <- 0
  
  return(alfa.LogCum.3)
}

