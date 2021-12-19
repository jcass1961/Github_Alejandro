############################################
####       DEFINE ESTIMADOR DE PL       ####
####       CARGAR LIBRERIA poweRlaw     ####
####    REQUIERE PROGRAMA GAMA INICIAL  ####
####      O UN VALOR DE GAMA INICIAL    ####
############################################

PL.quantile1<-function(datos,gama){
  data<-datos + gama
  
  PL.alfa <- conpl$new(data)
  PL.alfa.xmin<-estimate_xmin(PL.alfa)
  PL.alfa.xmin$Xmin<-quantile(data,0.75)
  alfa.PL.quantile<-1-estimate_pars(PL.alfa)$pars
  return(alfa.PL.quantile)
}