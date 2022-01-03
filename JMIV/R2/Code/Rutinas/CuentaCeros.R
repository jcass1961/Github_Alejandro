
######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/data/OtroPaper")


getwd()

### VALOR DE L
L=3

######################################################################################################
######################################################################################################

### NOMBRE BASE A GRAFICAR
nombre.base1<-paste("base500_NOCONT_MVyLNyNG1_L",L, sep = "")
nombre.base2<-paste("base_NOCONT_MVyLNyNG1_L",L,"_TODOalfa_n=1000v2.csv", sep = "")
nombre.base3<-paste("../../Data/OtroPaper/MVLogCum1y2_500_L",L, "_n=1000",sep = "")

### NOMBRE DE LOS GRAFICOS
graf.alfa.pdf<-paste("alfa_500_sinmenos20_jstar_L",L,"_MV_LN_NG1ybarrasdeerror",".pdf",sep = "")

##########################################################################################
###########################################################################################

### COMIENZO DEL PROGRAMA
base1 <- read.csv(nombre.base1)[,2:7]
base2 <- read.csv(nombre.base2)
base3.0<-read.csv(nombre.base3)

head(base3.0)
#View(base2)
base0<-rbind(base1,base2)
base<-subset(base0,base0$MV!=-20 & base0$alfa.DT.NG1.crossV!=-20 & 
               base0$alfa.DT.LN.crossV!=-20)

base3<-subset(base3.0,base3.0$alfa.logcum1!=0 & base3.0$alfa.logcum1!=-20)



ceros.LogCum<-subset(base3.0,base3.0$alfa.logcum1==0)
tabla.ceros.LogCum<-table(ceros.LogCum$n,ceros.LogCum$alfa)
por.tabla.ceros.LogCum<-tabla.ceros.LogCum/500
tabla.ceros.LogCum
por.tabla.ceros.LogCum

