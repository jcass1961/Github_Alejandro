### DIRECTORIO DONDE ESTAN LAS BASES
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("Conake")

library("caTools")

library(grDevices)
require(ggplot2)

### CARGO RUTINAS
source("./Rutinas/imagematrix.R")
source("./Rutinas/myread.ENVI.R")
source("./Rutinas/estima numero de looks muestras de tamanio gral_V2.R")

source("./Rutinas/ModeloGammaKS_qqplot.R")
source("./Rutinas/MV_Gama.R")
source("./Rutinas/Define densidad GI0.R")

############################################
## PARA ESTIMAR PARAMETROS 
source("./Rutinas/DistanciaTriangularNG1conCte.R")
source("./Rutinas/DistanciaTriangularLNconCte.R")
source("./Rutinas/LogcumulantOrden1.R")
source("./Rutinas/MV Estimador.R")

source("./Rutinas/EstimaUnPar_MuestrasReales.R")
source("./Rutinas/elige una muestra_V4.R")

source("./Rutinas/nucleos.R")                ###############
source("./Rutinas/CrossValidationdeR.R")     ###############
source("./Rutinas/MOM_1medio.R")
source("./Rutinas/Conakereport2conIntegrate.R")                ###############
source("./Rutinas/Conakereport2.R") 
source("./Rutinas/procesamiento piramidal_V2.R")
source("./Rutinas/DensitiesGI0Alejandro.R")

source("./Rutinas/GraficaMuestra.R")

setwd("../Data/PaperTesis")
a0<-read.ENVI("C:/Users/julia/Dropbox/Procesamiento de imagenes/KerEst/Images/tesis/dlr_munich_4s.flt")

b0<-a0^2

imagen.recortada<-b0[29:360,3:350]


##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

piramidal.MUNICH<-proc.piram(2,imagen.recortada)


L=3.21

############################################################
## ESTIMO

#recortada1<-piramidal.MUNICH[4:47,4:62]
recortada2<-piramidal.MUNICH[9: 94,26:115]
#recortada1<-piramidal.MUNICH[26:82,7:65]
getwd()
setwd("../../Figures")
#windows(width=6.5, height=6.5, rescale="fit")

cairo_ps(file = "CornerJulia_Roja2.eps",onefile = FALSE,
         fallback_resolution=600,family="serif",width = 10, height = 10, pointsize = 12)

plot(imagematrix(normalize(matrix(ecdf(recortada2)(recortada2), 
                                  nrow=dim(recortada2)[1], ncol=dim(recortada2)[2]))))

grafica.muestra.imagenRecortada(recortada2,60,62,43,45,"magenta",3)
grafica.muestra.imagenRecortada(recortada2,59,63,42,47,"green",3)
grafica.muestra.imagenRecortada(recortada2,58,66,41,49,"yellow",3)
grafica.muestra.imagenRecortada(recortada2,57,68,40,51,"red",3)
dev.off() 




