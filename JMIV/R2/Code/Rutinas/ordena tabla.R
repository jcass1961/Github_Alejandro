library("plyr")

############## FUNCION QUE ORDENA - REQUIERE LIBRERIA PLYR
ordena.tabla<-function(datos)
{
  #tabla.nom00<-table(datos$n,datos$alfa)
  tabla0<-as.data.frame(table(datos$n,datos$alfa))
  names(tabla0)<-c("n","alfa","frec")
  tabla<-data.frame(alfa=tabla0$alfa,
                    n=tabla0$n,
                    frec=tabla0$frec)
  
  orden<-arrange(tabla,desc(tabla$alfa),tabla$n)
  return(orden)
}
