### PROGRAMA QUE ESTIMA EL NUMERO DE LOOKS
### PARAMETROS DE ENTRADA: IMAGEN Y CANTIDAD DE MUESTRAS


estim.num.look<-function(imagen,cant.muestras)
{
  look.muestras<-rep(0,cant.muestras)
  tams.muestras<-rep(0,cant.muestras)
  media.muestras<-rep(0,cant.muestras)
  desvio.muestras<-rep(0,cant.muestras)
  media.muestras.cuad<-rep(0,cant.muestras)
  desvio.muestras.cuad<-rep(0,cant.muestras)
  
  x1.tot<-rep(0,cant.muestras)
  x2.tot<-rep(0,cant.muestras)
  y1.tot<-rep(0,cant.muestras)
  y2.tot<-rep(0,cant.muestras)
  
  for (i in 1:cant.muestras)
  {
    #col=c("magenta","green","blue","yellow","red","white","forestgreen","purple",
    #      "orangered","palegreen")
    fin<-cant.muestras+1
    col=2:cant.muestras
    lty=1
    lwd=2
    print("Ingrese 2 puntos en la imagen utilizando el mouse")
    pos.1<-locator(n=2,type="n")
    
    
    #dibuja los rectángulos
    segments(pos.1$x[1],pos.1$y[1],pos.1$x[2],pos.1$y[1],col=col[i],lty=lty,lwd=lwd)
    segments(pos.1$x[1],pos.1$y[2],pos.1$x[2],pos.1$y[2],col=col[i],lty=lty,lwd=lwd)
    segments(pos.1$x[1],pos.1$y[1],pos.1$x[1],pos.1$y[2],col=col[i],lty=lty,lwd=lwd)
    segments(pos.1$x[2],pos.1$y[1],pos.1$x[2],pos.1$y[2],col=col[i],lty=lty,lwd=lwd)
    
    x1<-floor(pos.1$x[1])
    y1<-floor(pos.1$y[1])
    x2<-floor(pos.1$x[2])
    y2<-floor(pos.1$y[2])
    
    x1.tot[i]<-x1
    x2.tot[i]<-x2
    y1.tot[i]<-y1
    y2.tot[i]<-y2
    
    #muestra para estimar los parámetros
    muestra.1<-as.vector(imagen[x1:x2,y1:y2])
    
    n1<-length(muestra.1)
    
    look.muestras[i]<-(mean(muestra.1)/sd(muestra.1))^2
    tams.muestras[i]<-n1
    media.muestras[i]<-mean(muestra.1)
    desvio.muestras[i]<-sd(muestra.1)
    media.muestras.cuad[i]<-media.muestras[i]^2
    desvio.muestras.cuad[i]<-desvio.muestras[i]^2
  }
  n<-sum(tams.muestras)
  salida.lm<-lm(media.muestras.cuad~desvio.muestras.cuad-1)
  salida.lm.rob<-lmRob(media.muestras.cuad~desvio.muestras.cuad-1)
  L.est.reg<-summary(salida.lm)$coefficients[1]
  L.est.rob<-summary(salida.lm.rob)$coefficients[1]
  L.est.CV<-(sum(look.muestras*tams.muestras))/n
  
  #return(list("x1.tot"=x1.tot,"x2.tot"=x2.tot,"y1.tot"=y1.tot,"y2.tot"=y2.tot))
  return(list("salida.lm"=salida.lm,"tams.muestras"=tams.muestras,
              "L.est.reg"=L.est.reg,"L.est.rob"=L.est.rob,"L.est.CV"=L.est.CV,"medias"=media.muestras,"desvios"=desvio.muestras,"x1"=x1,"x2"=x2,"y1"=y1,"y2"=y2))
}
