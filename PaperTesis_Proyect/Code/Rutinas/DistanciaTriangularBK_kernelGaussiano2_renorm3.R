
EstimadorNucleoBK2<-function(x,datos,ancho){
  #dok =(1/cte)*sapply(x, bckdenxrenorm,datos,ancho)
  dok =sapply(x, bckdenxrenorm,datos,ancho)
  return(dok)

}

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BK<-function(x,a,L,datos,ancho) (GI2(x,a,L)-EstimadorNucleoBK2(x,datos,ancho))^2/
                  (GI2(x,a,L)+EstimadorNucleoBK2(x,datos,ancho))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BK<-function(a,L,datos,ancho) 
  adaptIntegrate(integrand.DT.BK,lower = 0, upper = 400,a,L,datos,ancho)$integral

