library("wesanderson")
require(tidyverse)
require(ggthemes)
require(rstudioapi)
library("cubature")
library("Conake")
library("caTools")

library(MASS)
library("stats4")


set.seed(1234)


### DIRECTORIO DONDE ESTAN LAS BASES
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


### CARGO RUTINAS
source("../Rutinas/imagematrix.R")
source("../Rutinas/myread.ENVI.R")
source("../Rutinas/estima numero de looks muestras de tamanio gral_V2.R")
source("../Rutinas/elige una muestra_V4.R")
source("../Rutinas/ModeloGammaKS_qqplot.R")
source("../Rutinas/MV_Gama.R")
source("../Rutinas/Define densidad GI0.R")

############################################
## PARA ESTIMAR PARAMETROS 
source("../Rutinas/DistanciaTriangularNG1conCte.R")
source("../Rutinas/DistanciaTriangularLNconCte.R")
source("../Rutinas/LogcumulantOrden1.R")
source("../Rutinas/MV Estimador.R")

source("../Rutinas/EstimaUnPar_MuestrasReales.R")




source("../Rutinas/nucleos.R")                ###############
source("../Rutinas/CrossValidationdeR.R")     ###############
source("../Rutinas/MOM_1medio.R")
source("../Rutinas/Conakereport2conIntegrate.R")                ###############
source("../Rutinas/Conakereport2.R") 
source("../Rutinas/procesamiento piramidal_V2.R")
source("../Rutinas/DensitiesGI0Alejandro.R")

source("../Rutinas/GraficaMuestra.R")


setwd("../../Data/PaperTesis")

a0<-read.ENVI("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/Images/tesis/dlr_munich_4s.flt")

b0<-a0^2

imagen.recortada<-b0[180:400,20:250]


##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

piramidal.MUNICH<-proc.piram(2,imagen.recortada)

L=3.21


##################### GRABA GRAFICOS

windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(normalize(matrix(ecdf(piramidal.MUNICH)(piramidal.MUNICH), nrow=dim(piramidal.MUNICH)[1], ncol=dim(piramidal.MUNICH)[2]))))
#View(piramidal.MUNICH)
grafica.muestra.imagenRecortada(piramidal.MUNICH,19,22,76,79,"yellow",2)#muestra1##########
grafica.muestra.imagenRecortada(piramidal.MUNICH,20,21,77,78,"red",2)#muestra2
grafica.muestra.imagenRecortada(piramidal.MUNICH,18,23,75,80,"blue",2)#muestra3
grafica.muestra.imagenRecortada(piramidal.MUNICH,17,24,74,81,"green",2)#muestra3
grafica.muestra.imagenRecortada(piramidal.MUNICH,16,25,73,82,"magenta",2)#muestra3


##################### GRABA GRAFICOS
setwd("../../figures/ConModificacionAlejandro")
getwd()
ggsave("CincoMuestras.pdf", plot = last_plot(), device = "pdf",scale=1.2)


