### GENERA UN MAPA DE ALFA CON NUCLEO GAMMA Y 
### ANCHO DE BANDA ENCONTRADO POR CROSS VALIDATION
### REQUIERE LIBRERIA CONAKE
### PARAMETRO DE ENTRADA:
# tam: PARTE ENTERA DEL (TAMAÑO DE LA VENTANA/2)
# imagen: imagen
# L: número de looks

mapa.DT.GA<-function(tam,imagen,L)
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
      
      #### Defino ancho de banda y aplico DT

      b.NG1.crossV<-cvbw(datosGI.norm,ker="GA")$hcv
      
      alfa.DT.NG1.crossV<-DT.NG1(rango.alfa,a,L,datosGI.norm,b.NG1.crossV)
      
      mapa.alfa.DT[r,k]<-alfa.DT.NG1.crossV
      print(k)
    }
  }
  return(mapa.alfa.DT)
}