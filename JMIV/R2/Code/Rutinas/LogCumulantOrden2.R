############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 2

logcum.orden2 = function(muestra, L){
  
  logcum.est.1 <- mean(log(muestra))
  logcum.est.2 <- mean((log(muestra)-logcum.est.1)^2)
  
  f <- function(x,est.2,L)
    {est.2 - psigamma(L, deriv = 1) - psigamma(-x, deriv = 1)}
  
  if (f(-20,logcum.est.2,L)*f(-1.01,logcum.est.2,L)<0)
    alfa.LogCum.2 <- uniroot(f, c(-20,-1.01), tol = 0.0001,logcum.est.2,L)$root
  else
    alfa.LogCum.2 <- 0
  
  return(alfa.LogCum.2)
}

