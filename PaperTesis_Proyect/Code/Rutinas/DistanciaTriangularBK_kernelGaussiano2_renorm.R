
EstimadorNucleoBK2<-function(x,datos,ancho,cte){
  #dok =(1/cte)*sapply(x, bckdenxrenorm,datos,ancho)
  dok =sapply(x, bckdenxrenorm,datos,ancho)
  return(dok)

}

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO BOUNDARY KERNEL
integrand.DT.BK<-function(x,a,L,datos,ancho,cte) (GI2(x,a,L)-EstimadorNucleoBK2(x,datos,ancho,cte))^2/
                  (GI2(x,a,L)+EstimadorNucleoBK2(x,datos,ancho,cte))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR CON NUCLEO BOUNDARY KERNEL
DT.BK<-function(a,L,datos,ancho,cte) 
  adaptIntegrate(integrand.DT.BK,lower = 0, upper = 400,a,L,datos,ancho,cte)$integral

