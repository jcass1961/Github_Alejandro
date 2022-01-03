############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT de orden 2

alfa.gama.LCestim = function(muestra, L){
  
  logcum.est.1 <- mean(log(muestra))
  logcum.est.2 <- mean((log(muestra)-logcum.est.1)^2)
  
  f1 <- function(alfa,est.2,L)
  {est.2 - psigamma(L, deriv = 1) - psigamma(-alfa, deriv = 1)}
  
  f2 <- function(alfa,ga,est.1,L)
  {est.1+log(L/ga)+psigamma(L, deriv = 0) + psigamma(alfa, deriv = 0)}
  
  if (f1(-20,logcum.est.2,L)*f1(-1.01,logcum.est.2,L)<0)
    alfa.LC <- uniroot(f1, c(-20,-1.01), tol = 0.0001,logcum.est.2,L)$root
  else
    alfa.LC <- 0
  
  if (f2(alfa.LC,0,logcum.est.1,L)*f2(alfa.LC,100,logcum.est.1,L)<0)
    gama.LC <- uniroot(f2, c(-20,-1.01), tol = 0.0001,alfa.LC,logcum.est.1,L)$root
  else
    gama.LC <- 0
  return(list=c(alfa.LC,gama.LC))
}

