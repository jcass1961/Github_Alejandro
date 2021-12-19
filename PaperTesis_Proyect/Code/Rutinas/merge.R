setwd("C:/Users/Julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/Contaminacion")
#setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper")
L=8
eps<-"05"



nombre1<-paste("base500_ContCasouno_L",L,"MVyLCyIG_alfa2_15_eps",eps,sep ="")
nombre2<-paste("base500_ContCasouno_L",L,"MVyLCyIG_alfa2_15_eps",eps,sep ="")
nombrefinal<-paste("base500_ContCasouno_L",L,"MVyLCyIG_alfa2_15_COMPLETAFINAL",sep ="")

base1<-read.csv(nombre1)[,-1]
base2<-read.csv(nombre2)[,-1]
View(base1)

base1<-base1[order(base1$L,-base1$alfa,base1$n),]
base2<-base2[order(base2$L,-base2$alfa,base2$n),]

View(base2)
View(base1)

basefinal<-merge(base1, base2,by.x=c("L","n","alfa"),by.y=c("L","n","alfa"))
basefinal<-basefinal[order(basefinal$L,-basefinal$alfa,basefinal$n),]
View(basefinal)
dim(basefinal)

write.csv(basefinal, file = nombrefinal)
