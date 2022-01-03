### GENERA UN PROCESAMIENTO PIRAMIDAL CON tam VECINOS
### INGRESAR EL TAMAÑO DE LA VENTANA=TAM/2

## IMAGEN ORIGINAL DE DIMENSION M*N
## IMAGEN PROCESADA DE DIMENSION (M/2)*(N/2)
## i RECORRE LAS FILAS DE LA IMAGEN ORIGINAL
## j RECORRE LAS COLUMNAS DE LA IMAGEN ORIGINAL
## m RECORRE LAS FILAS DE LA IMAGEN PROCESADA
## n RECORRE LAS COLUMNAS DE LA IMAGEN PROCESADA

proc.piram<-function(tam.vent,imagen)
{
  dimension<-dim(imagen)
  dim.imagen.fila<-floor(dimension[1]/tam.vent)
  dim.imagen.col<-floor(dimension[2]/tam.vent)
  
  imagen.1<-matrix(0,dim.imagen.fila,dim.imagen.col)
  
  fin.r<-dim.imagen.fila-1
  fin.k<-dim.imagen.col-1
  
  for(r in 0:fin.r)
  {
    print(r)
    i<-tam.vent*r+1
    m<-r+1
    for(k in 0:fin.k)
    {
      j<-tam.vent*k+1
      n<-k+1
      #x<-c(imagen[i,j],imagen[i,j+1],imagen[i+1,j],imagen[i+1,j+1])
      i.fin<-i+tam.vent-1
      j.fin<-j+tam.vent-1
      x<-c(imagen[i:i.fin,j:j.fin])
      imagen.1[m,n]<-mean(x)
      
      print(k)
    }
  }
  return(imagen.1)
}
