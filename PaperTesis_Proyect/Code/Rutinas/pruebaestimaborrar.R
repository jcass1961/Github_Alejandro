############################################################################
##### PARA GENERAR LA BASE DE DATOS

######################################################################################################
############################################################################
############################################################################
##### PROGRAMA PRINCIPAL
setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/")

library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("Conake")

source("Code/Rutinas/Alfa0Gama0PorMomentos.R")
source("Code/Rutinas/LogCumulantAlfayGamma.R")
source("Code/Rutinas/DefineDensidadGI_alfaygamma.R")
source("Code/Rutinas/LogLikelihoodGI0.R")
source("Code/Rutinas/DistanciaTriangularNG1conCteAlfaGama.R")


source("Code/Rutinas/nucleos.R")                ###############
#source("Code/Rutinas/CrossValidationdeR.R")     ###############
source("Code/Rutinas/MOM_1medio.R")
#source("Code/Rutinas/Conakereport2conIntegrate.R")                ###############
source("Code/Rutinas/Conakereport2.R") 
source("Code/Rutinas/boptimoNucleoGamma_alfaygama.R")
source("Code/Rutinas/EstimaParametrosMuestrasRealesDT.R")
source("Code/Rutinas/EstimaParametrosMuestrasRealesMV.R")

source("Code/Rutinas/Genera GI en funcion de alfa y gamma.R")



alfa1<--8
gama1<--alfa1-1


L=3
ker="GA"


datos2<-generoGI.alfagama(alfa1,gama1,49,L)

a.DT<-estimadores.tiempoDT(datos2,L,ker)
a.DT

a.MV<-estimadores.tiempoMV(datos2,L,ker)
a.MV
#datosGI<-muestraSF[,2]



######################################################################################
muestraSF <- read.csv("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Images/DTterico/DatosImagen/muestraSF.csv")

L=3
ker="GA"


datos<-muestraSF$x
head(datos)
a.DT<-estimadores.tiempoDT(datos,L,ker)
a.DT

a.MV<-estimadores.tiempoMV(datos,L,ker)
a.MV
