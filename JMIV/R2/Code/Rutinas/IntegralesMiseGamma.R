
################### ESTUDIO MISE #############################################
######################################################################################################

############## INTEGRALES NUCLEOS GAMMA #################################

## NUCLEOS GAMMA 
Int5<-function(alfa,L)
{
  (sqrt(L/(-alfa-1))*gamma(-(1/2) + L)*gamma(1/2 - alfa))/( gamma(L)*gamma(-alfa))
}

Int6<-function(alfa,L)
{
  L^2*(L - alfa)*(1 +L - alfa)*(-1 + alfa)*
    (L*(-2 + alfa)*(-1 + 2*alfa)*(6 - 4*alfa + L*(-5 + 3*alfa))- 2*(-3 + 2*L)*
       (3 + 2*L - 2*alfa)*(-1 + 2*alfa)*(-alfa-1) - 4*(-3 + 2*L)*(3 + 2*L - 2*alfa)*(-alfa-1)^2)*
    gamma(-1 + 2*L)*gamma(1 - 2*alfa)* gamma(L - alfa)^2/
    ((-3 + 2*L)*(-alfa-1)^3*gamma(L)^2*gamma(4 + 2*L - 2*alfa)*gamma(-alfa)^2)
}

Int7<-function(alfa,L)
{
  (2*L^3*(L - alfa)*(1 + L - alfa)*(-2 + alfa)* 
     (6 - 4*alfa + L*(-5 + 3*alfa))*gamma(-1 + 2*L)*
     gamma(3 - 2*alfa)*gamma(L -alfa)^2)/((-3 + 2* L)*(-alfa-1)^3* gamma(L)^2*gamma(4 + 2*L - 2*alfa)*gamma(-alfa)^2)
}
