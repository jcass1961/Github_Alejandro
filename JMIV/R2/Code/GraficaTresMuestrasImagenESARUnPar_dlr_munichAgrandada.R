library("wesanderson")
require(tidyverse)
require(ggthemes)
require(rstudioapi)

library("cubature")
library("Conake")

library("caTools")

library(grDevices)

library(MASS)
library("stats4")


set.seed(1234)


### DIRECTORIO DONDE ESTAN LAS BASES
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

### CARGO RUTINAS
source("./Rutinas/imagematrix.R")
source("./Rutinas/myread.ENVI.R")
source("./Rutinas/estima numero de looks muestras de tamanio gral_V2.R")
source("./Rutinas/elige una muestra_V4.R")
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




source("./Rutinas/nucleos.R")                ###############
source("./Rutinas/CrossValidationdeR.R")     ###############
source("./Rutinas/MOM_1medio.R")
source("./Rutinas/Conakereport2conIntegrate.R")                ###############
source("./Rutinas/Conakereport2.R") 
source("./Rutinas/procesamiento piramidal_V2.R")
source("./Rutinas/DensitiesGI0Alejandro.R")

source("./Rutinas/GraficaMuestra.R")

setwd("../Data/PaperTesis")

a0<-read.ENVI("dlr_munich_4s.flt")

b0<-a0^2

############ Recorto imagen

imagen.recortada<-b0[180:450,20:250]

### GRAFICO LA IMAGEN NORMALIZADA Y ECUALIZADA
# windows(width=6.5, height=6.5, rescale="fit")
# plot(imagematrix(matrix(ecdf(imagen.recortada)(imagen.recortada), nrow=nrow(imagen.recortada))))
# 

##########################################################
## APLICO PROCESAMIENTO PIRAMIDAL

piramidal.MUNICH<-proc.piram(2,imagen.recortada)

#View(mapa.media)
dim(piramidal.MUNICH)

### GRAFICO IMAGEN PIRAMIDAL NORMALIZADA Y ECUALIZADA

# windows(width=6.5, height=6.5, rescale="fit")
# plot(imagematrix(normalize(matrix(ecdf(piramidal.MUNICH)(piramidal.MUNICH), nrow=dim(piramidal.MUNICH)[1], ncol=dim(piramidal.MUNICH)[2]))))



#looks.piramidal<-elige.muestra(piramidal.MUNICH)


#L=mean(looks.piramidal$muestra)^2/sd(looks.piramidal$muestra)^2
L=3.21

##################################################

muestra1.mat<-piramidal.MUNICH[76:79,19:22] #"YELLOW"
muestra1.mat


muestra2.mat<-piramidal.MUNICH[77:80,19:22] #"RED"
muestra2.mat

muestra3.mat<-piramidal.MUNICH[77:79,19:22] #"BLUE"
muestra3.mat

# muestra4.mat<-piramidal.MUNICH[75:80,19:22]
# muestra4.mat



muestra1<-as.vector(muestra1.mat)/mean(as.vector(muestra1.mat))
muestra2<-as.vector(muestra2.mat)/mean(as.vector(muestra2.mat))
muestra3<-as.vector(muestra3.mat)/mean(as.vector(muestra3.mat))


length(muestra1)
length(muestra2)
length(muestra3)



a.estim1<-estima.unpar(muestra1,L)
a.estim1

a.estim2<-estima.unpar(muestra2,L)
a.estim2

a.estim3<-estima.unpar(muestra3,L)
a.estim3


piramidal.munich.recortada<-piramidal.MUNICH[63:90,0:42]


#View(piramidal.MUNICH)

y1R.nuevo<-77-63
y2R.nuevo<-80-63

y1B.nuevo<-77-63
y2B.nuevo<-79-63

y1A.nuevo<-76-63
y2A.nuevo<-79-63

lty=1
lwd=4

getwd()
setwd("../../Figures")
cairo_ps(file = "TresMuestrasAgrandada.eps",onefile = FALSE,
         fallback_resolution=600,family="serif", pointsize = 12)
plot(imagematrix(normalize(matrix(ecdf(piramidal.munich.recortada)(piramidal.munich.recortada), 
                                  nrow=dim(piramidal.munich.recortada)[1], ncol=dim(piramidal.munich.recortada)[2]))))
grafica.muestra.imagenRecortada(piramidal.munich.recortada,19,22,y1R.nuevo,y2R.nuevo,"red",lwd)#muestra1##########
grafica.muestra.imagenRecortada(piramidal.munich.recortada,19,22,y1B.nuevo,y2B.nuevo,"blue",lwd)#muestra2##########
grafica.muestra.imagenRecortada(piramidal.munich.recortada,19,22,y1A.nuevo,y2A.nuevo,"yellow",lwd)#muestra1##########

dev.off()
