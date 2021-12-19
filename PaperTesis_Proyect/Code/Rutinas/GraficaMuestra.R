### PROGRAMA QUE ESTIMA EL NUMERO DE LOOKS - NO CONSIDERA ESTIMACION ROBUSTA
### PARAMETROS DE ENTRADA: IMAGEN Y CANTIDAD DE MUESTRAS
### windows(width=20, height=20, rescale="fit")
### plot(imagematrix(normalize(matrix(ecdf(piramidal)(piramidal), nrow=dim(piramidal)[1], ncol=dim(piramidal)[2]))))

#dim(piramidal)
grafica.muestra.imagenRecortada<-function(imagen,x1,x2,y1,y2,color,grosor)
{
  dimension<-dim(imagen)
  y1.fin<-dim(imagen)[1]-y1
  y2.fin<-dim(imagen)[1]-y2
  lty=1
  lwd=grosor
  #dibuja los rect?ngulos
  segments(x1,y1.fin,x1,y2.fin,col=color,lty=lty,lwd=lwd)
  segments(x1,y2.fin,x2,y2.fin,col=color,lty=lty,lwd=lwd)
  segments(x2,y2.fin,x2,y1.fin,col=color,lty=lty,lwd=lwd)
  segments(x2,y1.fin,x1,y1.fin,col=color,lty=lty,lwd=lwd)
  }

# grafica.muestra.imagenRecortada(piramidal,243,249,184,190,"magenta")
# grafica.muestra.imagenRecortada(piramidal,246,252,203,197,"red")
# grafica.muestra.imagenRecortada(piramidal,137,143,313,307,"yellow")
# grafica.muestra.imagenRecortada(piramidal,265,271,80,74,"green")
# grafica.muestra.imagenRecortada(piramidal,182,197,198,205,"purple")
# grafica.muestra.imagenRecortada(piramidal,211,237,344,325,"white")
# grafica.muestra.imagenRecortada(piramidal,163,169,130,124,"lightcoral")
# grafica.muestra.imagenRecortada(piramidal,246,252,343,337,"darkgoldenrod1")
# 
# ##################################################
# ## EN LA IMAGEN GRANDE
# windows(width=20, height=20, rescale="fit")
# plot(imagematrix(normalize(matrix(ecdf(piramidal0)(piramidal0), 
#                                   nrow=dim(piramidal0)[1], ncol=dim(piramidal0)[2]))))
# 
# grafica.muestra.imagenGrande<-function(imagen,x1,x2,y1,y2,color)
# {
#   dimension<-dim(imagen)
#   #y1.fin<-dim(imagen)[1]-(y1+130)
#   #y2.fin<-dim(imagen)[1]-(y2+130)
#   #x1.fin<-x1+13
#   #x2.fin<-x2+13
#   y1.fin<-dim(imagen)[1]-y1
#   y2.fin<-dim(imagen)[1]-y2
#   x1.fin<-x1
#   x2.fin<-x2
#   lty=1
#   lwd=2
#   #dibuja los rect?ngulos
#   segments(x1.fin,y1.fin,x1.fin,y2.fin,col=color,lty=lty,lwd=lwd)
#   segments(x1.fin,y2.fin,x2.fin,y2.fin,col=color,lty=lty,lwd=lwd)
#   segments(x2.fin,y2.fin,x2.fin,y1.fin,col=color,lty=lty,lwd=lwd)
#   segments(x2.fin,y1.fin,x1.fin,y1.fin,col=color,lty=lty,lwd=lwd)
# }
# 
# 
# x<-13
# y<-130
# grafica.muestra.imagenGrande(piramidal0,243+x,249+x,184+y,190+y,"magenta")
# grafica.muestra.imagenGrande(piramidal0,246+x,252+x,203+y,197+y,"red")
# grafica.muestra.imagenGrande(piramidal0,137+x,143+x,313+y,307+y,"yellow")
# grafica.muestra.imagenGrande(piramidal0,265+x,271+x,80+y,74+y,"green")
# grafica.muestra.imagenGrande(piramidal0,182+x,197+x,198+y,205+y,"purple")
# grafica.muestra.imagenGrande(piramidal0,211+x,237+x,344+y,325+y,"white")
# grafica.muestra.imagenGrande(piramidal0,163+x,169+x,130+y,124+y,"lightcoral")
# grafica.muestra.imagenGrande(piramidal0,246+x,252+x,343+y,337+y,"darkgoldenrod1")
# grafica.muestra.imagenGrande(piramidal0,403,409,74,68,"orangered")
# grafica.muestra.imagenGrande(piramidal0,468,474,230,224,"lightblue1")
# grafica.muestra.imagenGrande(piramidal0,377,383,425,419,"lightyellow")
# 
# 
# 
# 
# 
# 
# 
# 
