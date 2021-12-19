### DIRECTORIO DONDE ESTAN LAS BASES
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("Conake")

library("caTools")

require(ggplot2)

### CARGO RUTINAS
source("../Rutinas/imagematrix.R")
source("../Rutinas/myread.ENVI.R")
source("../Rutinas/estima numero de looks muestras de tamanio gral_V2.R")

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
source("../Rutinas/elige una muestra_V4.R")

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
windows(width=6.5, height=6.5, rescale="fit")
plot(imagematrix(normalize(matrix(ecdf(recortada2)(recortada2), 
                                  nrow=dim(recortada2)[1], ncol=dim(recortada2)[2]))))

grafica.muestra.imagenRecortada(recortada2,60,62,43,45,"magenta",2)
grafica.muestra.imagenRecortada(recortada2,59,63,42,47,"green",2)
grafica.muestra.imagenRecortada(recortada2,58,66,41,49,"yellow",2)
grafica.muestra.imagenRecortada(recortada2,57,68,40,51,"red",2)

ggsave("../../figures/ConModificacionAlejandro/CornerJulia_Roja.pdf", 
       plot = last_plot(), device = "pdf",scale=2)


# muestra1.r2<-elige.muestra(recortada2,"red")
# muestra1.m.r2<-muestra1.r2$muestra/mean(muestra1.r2$muestra)
# 
# muestra2.r2<-elige.muestra(recortada2,"green")
# muestra2.m.r2<-muestra2.r2$muestra/mean(muestra2.r2$muestra)
# 
# muestra3.r2<-elige.muestra(recortada2,"yellow")
# muestra3.m.r2<-muestra3.r2$muestra/mean(muestra3.r2$muestra)
# 
# muestra4.r2<-elige.muestra(recortada2,"yellow")
# muestra4.m.r2<-muestra4.r2$muestra/mean(muestra4.r2$muestra)
# 
# muestra1.r2
# muestra2.r2
# muestra3.r2
# muestra4.r2
# 
# a.estim1.r2<-estima.unpar(as.vector(muestra1.m.r2),L)
# a.estim1.r2
# 
# a.estim2.r2<-estima.unpar(as.vector(muestra2.m.r2),L)
# a.estim2.r2
# 
# a.estim3.r2<-estima.unpar(as.vector(muestra3.m.r2),L)
# a.estim3.r2
# 
# a.estim4.r2<-estima.unpar(as.vector(muestra4.m.r2),L)
# a.estim4.r2
# 
# 
# 
# m1<-recortada2[43:45,60:62]
# m2<-recortada2[42:47,59:63]
# m3<-recortada2[41:49,58:66]
# m4<-recortada2[40:51,57:68]
# m5<-recortada2[43:45,60:63]
# 
# 
# a.estim1.r2<-estima.unpar(as.vector(m1)/mean(as.vector(m1)),L)
# a.estim1.r2
# 
# a.estim2.r2<-estima.unpar(as.vector(m2)/mean(as.vector(m2)),L)
# a.estim2.r2
# 
# a.estim3.r2<-estima.unpar(as.vector(m3)/mean(as.vector(m3)),L)
# a.estim3.r2
# 
# a.estim4.r2<-estima.unpar(as.vector(m4)/mean(as.vector(m4)),L)
# a.estim4.r2
# 
# a.estim5.r2<-estima.unpar(as.vector(m5)/mean(as.vector(m5)),L)
# a.estim5.r2
