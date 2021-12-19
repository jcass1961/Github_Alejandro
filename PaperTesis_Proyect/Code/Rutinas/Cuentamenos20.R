
######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/data/DTTeorico/L2L1Hell")

getwd()



### VALOR DE L
L=3

######################################################################################################
######################################################################################################

### NOMBRE BASE A GRAFICAR
# nombre.base1<-paste("base500_NOCONT_MVyLNyNG1_L",L, sep = "")
# nombre.base2<-paste("base_NOCONT_MVyLNyNG1_L",L,"_TODOalfa_n=1000v2.csv", sep = "")

nombre.base1<-paste("base500_NoCont_L3L2GAhasta121TODOALFAygamaV210conIntegrate.csv",sep = "")


##########################################################################################
###########################################################################################

### COMIENZO DEL PROGRAMA
# base1 <- read.csv(nombre.base1)[,2:7]
# base2 <- read.csv(nombre.base2)
base3.0<-read.csv(nombre.base1)

head(base3.0)
#View(base2)
# base0<-rbind(base1,base2)
# base<-subset(base0,base0$MV!=-20 & base0$alfa.DT.NG1.crossV!=-20 & 
#                base0$alfa.DT.LN.crossV!=-20)

#base3<-subset(base3.0,base3.0$alfa.logcum1!=0 & base3.0$alfa.logcum1!=-20)



ceros<-subset(base3.0,base3.0$alfa.DT.GA.BFGS==-20)
tabla.ceros<-table(ceros$n,ceros$alfa)
por.tabla.ceros<-tabla.ceros/500
tabla.ceros
por.tabla.ceros

ceros<-subset(base3.0,base3.0$alfa.L2.GA==-20)
tabla.ceros<-table(ceros$n,ceros$alfa)
por.tabla.ceros<-tabla.ceros/500
tabla.ceros
por.tabla.ceros