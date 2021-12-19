############################################################################
##### DEFINE ESTIMADOR DE MOMENTO - USA MOMENTO DE ORDEN 2
MOM2<- function(muestra)
{
  mom1 <- mean(muestra)
  mom2 <- mean(muestra^2)
  #browser()
  alfa.mom.2<--1+mom2/(2*mom1^2-mom2) 
  return(alfa.mom.2)
}