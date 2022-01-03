### PROGRAMA QUE ESTIMA EL NUMERO DE LOOKS - NO CONSIDERA ESTIMACION ROBUSTA
### PARAMETROS DE ENTRADA: IMAGEN Y CANTIDAD DE MUESTRAS
x1<-c()
x2<-c()
y1<-c()
y2<-c()


elige.muestra<-function(imagen,color)
{
  pos<-locator(2,type="l",col="blue")
  pos
  
  lty=1
  lwd=1
  
  x1<-pos$x[1]
  x2<-pos$x[2]
  y1<-pos$y[1]
  y2<-pos$y[2]
 
  segments(x1,y1,x1,y2,col=(color),lty=lty,lwd=lwd)
  segments(x1,y2,x2,y2,col=(color),lty=lty,lwd=lwd)
  segments(x2,y2,x2,y1,col=(color),lty=lty,lwd=lwd)
  segments(x2,y1,x1,y1,col=(color),lty=lty,lwd=lwd)
  
  x1.p<-dim(imagen)[1]-floor(y1)
  x2.p<-dim(imagen)[1]-floor(y2)
  y1.p<-floor(x1)
  y2.p<-floor(x2)
  
  muestra<-as.vector(imagen[x1.p:x2.p,y1.p:y2.p])
  muestra.imagen<-imagen[x1.p:x2.p,y1.p:y2.p]
  coord<-data.frame("x1"=x1.p,"x2"=x2.p,"y1"=y1.p,"y2"=y2.p)
  
  n1<-length(muestra)
  
  return(list("muestra"=muestra,"muestra.imagen"=muestra.imagen,"Coord"=coord,"tamanio"=n1))
}
