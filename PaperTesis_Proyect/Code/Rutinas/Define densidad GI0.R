############################################################################
#### DEFINE DENSIDAD GI0 CONSIDERANDO ESPERANZA 1
#### alfa=-gamma-1
GI2<-function(z,alfap,L)
{L^L*gamma(L - alfap)/((-alfap - 1)^alfap*gamma(L)*gamma(-alfap))*z^( L - 1)/((-alfap - 1) + L*z)^(L - alfap)}
