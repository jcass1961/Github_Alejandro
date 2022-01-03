### GENERA UN MAPA DE ALFA CON NUCLEO LOGNORMAL Y 
### ANCHO DE BANDA ENCONTRADO POR CROSS VALIDATION
### REQUIERE LIBRERIA CONAKE

mapa.DT.LN<-function(tam,imagen)
{
  dimension<-dim(imagen)
  fin.r<-dimension[1]-2*tam
  fin.k<-dimension[2]-2*tam
  
  mapa.alfa.DT<-matrix(0,fin.r,fin.k)
  
  for(r in 1:fin.r)
  {
    print(r)
    for(k in 1:fin.k)
    {
      #browser()
      
      fin.im1<-r+2*tam
      fin.im2<-k+2*tam
      imag<-imagen[r:fin.im1,k:fin.im2]
      datosGI<-as.vector(imag)
      
      #### NORMALIZO LOS DATOS PARA QUE TENGAN ESPERANZA 1
      datosGI.norm<-datosGI/mean(datosGI)
      
      #### Defino distancias estocásticas y minimizo
      
      b.LN.crossV<-cvbw(datosGI.norm,ker="LN")$hcv
      
      alfa.DT.LN.crossV<-DT.LN(rango.alfa,a,L,datosGI.norm,b.LN.crossV)
      
      mapa.alfa.DT[r,k]<-alfa.DT.LN.crossV
      print(k)
    }
  }
  return(mapa.alfa.DT)
}