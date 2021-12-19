
#setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/contaminacion")
setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/contaminacion/Bases Parciales")
#setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/JSTAR_2016")


getwd()
L=3
eps<-"01"

alfa1<-"menos1punto5"
alfa2<-"menos3"
alfa3<-"menos5"
alfa4<-"-8"

n1<-9
n2<-25
n3<-49
n4<-81
n5<-121
n6<-500
n7<-1000

nombre1<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n1,"_eps",eps,sep ="")
nombre2<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n2,"_eps",eps,sep ="")
nombre3<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n3,"_eps",eps,sep ="")
nombre4<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n4,"_eps",eps,sep ="")
nombre5<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n5,"_eps",eps,sep ="")
nombre6<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n6,"_eps",eps,sep ="")
nombre7<-paste("base500_ContCasouno_L",L,"MVyLN_alfa",alfa4,"_alfa2-15_n",n7,"_eps",eps,sep ="")

nombrefinal<-paste("base500_NoCont_L",L,"MVyLN_COMPLETAFINAL","_eps",eps,sep ="")

base1<-read.csv(nombre1,sep=",")[,-1]
head(base1)
base2<-read.csv(nombre2,sep=",")[,-1]
base3<-read.csv(nombre3,sep=",")[,-1]
base4<-read.csv(nombre4,sep=",")[,-1]
base5<-read.csv(nombre5,sep=",")[,-1]
base6<-read.csv(nombre6,sep=",")[,-1]
base7<-read.csv(nombre7,sep=",")[,-1]

junto12<-rbind(base1,base2)
junto123<-rbind(junto12,base3)
junto1234<-rbind(junto123,base4)
junto12345<-rbind(junto1234,base5)
junto123456<-rbind(junto1234,base6)
junto1234567<-rbind(junto1234,base7)

View(junto1234567)

basefinal<-junto1234567[order(junto1234567$n,-junto1234567$alfa),]
View(basefinal)

setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/contaminacion/Bases Finales")
write.csv(basefinal, file = nombrefinal)
