############################################
####    DEFINE ESTIMADOR DE HILL        ####
####    REQUIERE PROGRAMA GAMA INICIAL  ####
####      O UN VALOR DE GAMA INICIAL    ####
############################################

hill.est1<-function(datos,gama){
  data<-datos + gama
  datosord<-sort(data)
  long<-length(data)
  k<-floor(3*long/5)
  cola<-datosord[k:long]
  umbral<-log(datosord[k])
  media<-mean(log(cola))
  alfa.hill<--(media-umbral)^(-1)
  return(max(-20,alfa.hill))
}