
#setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper")
#setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/Contaminacion/Bases Parciales")
setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/Contaminacion")

getwd()
L=8

alfa1<-"menos1punto5"
alfa2<-"-3"
alfa3<-"-5"
alfa4<-"-8"
eps<-"05"

nombre1<-paste("base500_ContCasouno_L",L,"NGyMV_n1000_alfa_",alfa1,"_alfa2-15_eps",eps,sep ="")
nombre2<-paste("base500_ContCasouno_L",L,"NGyMV_n1000_alfa",alfa2,"_alfa2-15_eps",eps,sep ="")
nombre3<-paste("base500_ContCasouno_L",L,"NGyMV_n1000_alfa",alfa3,"_alfa2-15_eps",eps,sep ="")
nombre4<-paste("base500_ContCasouno_L",L,"NGyMV_n1000_alfa",alfa4,"_alfa2-15_eps",eps,sep ="")

nombrefinal<-paste("base500_ContCasouno_L",L,"NGyMV_n1000_alfa2-15_COMPLETAFINAL_eps",eps,sep ="")

base1<-read.csv(nombre1,sep=",")[,-1]
head(base1)
base2<-read.csv(nombre2,sep=",")[,-1]
base3<-read.csv(nombre3,sep=",")[,-1]
base4<-read.csv(nombre4,sep=",")[,-1]
dim(base1)
dim(base2)
dim(base3)
dim(base4)

junto12<-rbind(base1,base2)
junto123<-rbind(junto12,base3)
junto1234<-rbind(junto123,base4)

View(junto1234)
dim(junto1234)

basefinal<-junto1234[order(junto1234$n,-junto1234$alfa),]
View(basefinal)

write.csv(basefinal, file = nombrefinal)
