### GENERA UN MAPA DE ALFA CON NUCLEO GAMMA Y 
### ANCHO DE BANDA ENCONTRADO POR CROSS VALIDATION
### REQUIERE LIBRERIA CONAKE
### PARAMETRO DE ENTRADA:
# tam: PARTE ENTERA DEL (TAMAÑO DE LA VENTANA/2)
# imagen: imagen
# L: número de looks

mapa.PL<-function(tam,imagen,matriz.alfa,L)
{
  dimension<-dim(imagen)
  fin.r<-dimension[1]-2*tam
  fin.k<-dimension[2]-2*tam
  
  #mapa.alfa.MV<-matrix(0,fin.r,fin.k)
  mapa.alfa.PL<-matrix(0,fin.r,fin.k)
  
  for(r in 1:fin.r)
  {
    print(r)
    for(k in 1:fin.k)
    {
      #browser()
      
      fin.im1<-r+2*tam
      fin.im2<-k+2*tam
      imag<-imagen[r:fin.im1,k:fin.im2]
      mat.alfas<-matriz[r:fin.im1,k:fin.im2]
      datosGI<-as.vector(imag)
      
      #### NORMALIZO LOS DATOS PARA QUE TENGAN ESPERANZA 1
      datosGI.norm<-datosGI/mean(datosGI)
      
      #### Defino tamanio de muestra y aplico MV
      n<-dim(imag)[1]*dim(imag)[2]
          
      #alfa.MV<-MV(a,datosGI.norm,n,L)@coef
      gama<--mean(mat.alfas)-1
      
      alfa.est<-PL.quantile1(datosGI.norm,gama) 
      mapa.alfa.PL[r,k]<-max(-20,alfa.est)
      
      #mapa.alfa.MV[r,k]<-alfa.MV
      print(k)
    }
  }
  #return(list("mapa.alfa.MV"=mapa.alfa.MV,"mapa.alfa.PL"=mapa.alfa.PL))
  return(list("mapa.alfa.PL"=mapa.alfa.PL))
}
