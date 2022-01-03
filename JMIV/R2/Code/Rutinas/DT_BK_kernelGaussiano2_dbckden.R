
EstimadorNucleoBK2<-function(x,datos,ancho,metodo){
  #dok =(1/cte)*sapply(x, bckdenxrenorm,datos,ancho)
  dok = dbckden(x, datos, lambda = ancho, bcmethod = metodo)
  return(dok)

}

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BK<-function(x,a,L,datos,ancho,metodo) (GI2(x,a,L)-EstimadorNucleoBK2(x,datos,ancho,metodo))^2/
                  (GI2(x,a,L)+EstimadorNucleoBK2(x,datos,ancho,metodo))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BK<-function(a,L,datos,ancho,metodo) 
  adaptIntegrate(integrand.DT.BK,lower = 0, upper = 400,a,L,datos,ancho,metodo)$integral

