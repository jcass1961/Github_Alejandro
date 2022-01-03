### Derivada GI respecto de alfa

derivGIalfa<-function(z,alfa,L) 
{
  (L^L*z^(-1 + L)*(-1 - alfa)^(-alfa)*(-1 + L*z - alfa)^(-L + alfa)*
  gamma(L - alfa)*(-(alfa/(1 + alfa)) - (L - alfa)/(1 - L*z + alfa) - log(-1 - alfa) + 
      log(-1 + L*z - alfa) - digamma(L - alfa) + digamma(-alfa)))/(gamma(L)*gamma(-alfa))
}



