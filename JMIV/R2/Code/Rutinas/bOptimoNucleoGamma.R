#### DEFINE ANCHO DE BANDA OPTIMO NUCLEO GAMMA

Int1<-function(alfa,ga,L)
{
  (sqrt(L/(ga))*gamma(-(1/2) + L)*gamma(1/2 - alfa))/( gamma(L)*gamma(-alfa))
}

Int2<-function(alfa,ga,L)
{
  -(9*L*(L-alfa)*(-1+alfa)*gamma(1+2*L)*gamma(1-2*alfa)*gamma(L-alfa)^2)/
    (4*(-1+2*L)*ga*gamma(L)^2*gamma(2+2*L-2*alfa)*gamma(-alfa)^2)
}

b1<-function(alfa,ga,L,n)
{
  (1/(2*sqrt(pi))*Int1(alfa,ga,L)/(4*n*Int2(alfa,ga,L)))^(2/5)
}

