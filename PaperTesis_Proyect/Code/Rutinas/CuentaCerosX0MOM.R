
######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/data/OtroPaper/Mom1medio")


getwd()

### VALOR DE L
L=3


### COMIENZO DEL PROGRAMA
base1 <- read_excel("X0MOM_L_3TodoAlfa.xlsx")


base<-subset(base1,base1$X0Mom==0)

tabla.ceros<-table(base$n,base$alfa)
por.tabla.ceros<-tabla.ceros/500
tabla.ceros
por.tabla.ceros

