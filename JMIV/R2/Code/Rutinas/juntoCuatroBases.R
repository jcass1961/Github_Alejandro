
setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/Kernel Estimation 20112018/Data/OtroPaper/ConConst/")
#setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/JSTAR_2016")

getwd()
#L=8

nombre1<-paste("base500_NoCONT_L8MVyLC_alfa-1punto5hasta121",sep ="")
nombre2<-paste("base500_NoCONT_L8MVyLC_alfa-3hasta121",sep = "")
nombre3<-paste("base500_NoCONT_L8MVyLC_alfa-5hasta121",sep = "")
nombre4<-paste("base500_NoCONT_L8MVyLC_alfa-8hasta121",sep = "")

nombrefinal<-paste("base500_NoCont_L8MVyLC_OPTIM_hasta121_Final",sep ="")

base1<-read.csv(nombre1,sep=",")
base2<-read.csv(nombre2,sep=",")
base3<-read.csv(nombre3,sep=",")
base4<-read.csv(nombre4,sep=",")
junto12<-rbind(base1,base2)
junto123<-rbind(junto12,base3)
junto1234<-rbind(junto123,base4)

View(junto1234)

basefinal<-junto1234[order(junto1234$n,-junto1234$alfa),]
View(basefinal)

write.csv(basefinal, file = nombrefinal)
dim(basefinal)
