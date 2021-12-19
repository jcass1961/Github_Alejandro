### GENERA UN MAPA DE ALFA CON NUCLEO LOGNORMAL Y 
### ANCHO DE BANDA ENCONTRADO POR CROSS VALIDATION
### REQUIERE LIBRERIA CONAKE

mapa.MV<-function(tam,imagen)
{
  dimension<-dim(imagen)
  fin.r<-dimension[1]-2*tam
  fin.k<-dimension[2]-2*tam
  
  mapa.alfa.MV<-matrix(0,fin.r,fin.k)
  
  for(r in 1:fin.r)
  {
    print(r)
    for(k in 1:fin.k)
    {
      #browser()
      print(k)
      
      fin.im1<-r+2*tam
      fin.im2<-k+2*tam
      imag<-imagen[r:fin.im1,k:fin.im2]
      datosGI0<-as.vector(imag)
      
      #### NORMALIZO LOS DATOS PARA QUE TENGAN ESPERANZA 1
      datosGI<-datosGI0/mean(datosGI0)
      
      #### Defino distancias estoc?sticas y minimizo
      
      if (mom_1_2(datosGI,L)!=0) x0<-mom_1_2(datosGI,L)
      else x0<--1.5
      n<-length(datosGI0)
      alfa.MV<-MV(a,datosGI,n,L)@coef[[1]]
      
      
      mapa.alfa.MV[r,k]<-alfa.MV
      
    }
  }
  return(mapa.alfa.MV)
}