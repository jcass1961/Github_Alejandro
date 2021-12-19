#### FUNCION FILTRO.MEDIA 
### ARGUMENTOs; la imagen y LA PARTE ENTERA DEL (TAMAÑO DE LA VENTANA/2)
#### CAMBIAR EL NOMBRE DE LOS ARCHIVOS Y EL ARGUMENTO DE LA FUNCION

filtro.media<-function(imagen,tam.vent)
{
  dimension<-dim(as.matrix(imagen))
  tam<-floor(tam.vent/2)
  fin.r<-dimension[1]-2*tam
  fin.k<-dimension[2]-2*tam
  
  mapa.media<-matrix(0,fin.r,fin.k)
  
  for(r in 1:fin.r)
  {
    print(r)
    for(k in 1:fin.k)
    {
      #browser()
      
      fin.im1<-r+2*tam
      fin.im2<-k+2*tam
      imag<-imagen[r:fin.im1,k:fin.im2]
      
      mapa.media[r,k]<-mean(imag)
    }
  }
  return(mapa.media)
}