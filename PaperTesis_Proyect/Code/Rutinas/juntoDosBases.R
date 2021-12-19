
setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/otropaper")

L=3
nombre1<-paste("base500_NoCont_v2_L",L,"MVyLN_crossV_n=1000",sep="")
nombre2<-paste("base500_NoCont_L",L,"MVyLN_crossV_n=1000",sep="")

base1<-read.csv(nombre1)
base2<-read.csv(nombre2)

junto<-rbind(base1,base2)
head(junto)

basefinal<-junto[order(junto$n,-junto$alfa),]
head(basefinal)

nombrefinal<-paste("base500_NoCont_L",L,"MVyLN_crossV_n=1000final",sep ="")
write.csv(basefinal, file = nombrefinal)