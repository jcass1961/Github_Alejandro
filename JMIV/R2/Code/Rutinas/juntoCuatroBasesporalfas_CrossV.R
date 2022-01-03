
#setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper/Contaminacion/Bases Parciales")
setwd("C:/Users/usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Data/OtroPaper")

getwd()
L=1


alfa1<-"-1punto5"
alfa2<-"-3"
alfa3<-"-5"
alfa4<-"-8"

n=9

nombre1<-paste("base500_NoCont_L",L,"MVyNG1yNG2yIGconCV_alfa",alfa1,"n",n,sep ="")
nombre2<-paste("base500_NoCont_L",L,"MVyNG1yNG2yIGconCV_alfa",alfa2,"n",n,sep ="")
nombre3<-paste("base500_NoCont_L",L,"MVyNG1yNG2yIGconCV_alfa",alfa3,"n",n,sep ="")
nombre4<-paste("base500_NoCont_L",L,"MVyNG1yNG2yIGconCV_alfa",alfa4,"n",n,sep ="")

nombrefinal<-paste("base500_NoCont_L",L,"MVyNG1yNG2yIGconCV","_n",n,sep ="")

base1<-read.csv(nombre1,sep=",")[,-1]
head(base1)
base2<-read.csv(nombre2,sep=",")[,-1]
base3<-read.csv(nombre3,sep=",")[,-1]
base4<-read.csv(nombre4,sep=",")[,-1]
# dim(base1)
# dim(base2)
# dim(base3)
# dim(base4)

base1.df<-data.frame(L=base1[,1],n=base1[,3], alfa=base1[,2],MV=base1[,4],
                         alfa.DT.NG1.CV=base1[,5],alfa.DT.NG2.CV=base1[,6],
                         alfa.DT.IG.CV=base1[,7],alfa.DT.IGjstar=base1[,8],
                         b.NG1.CV=base1[,9],b.NG2.CV=base1[,10],b.IG.CV=base1[,11])

junto12<-rbind(base1,base2)
junto123<-rbind(junto12,base3)
junto1234<-rbind(junto123,base4)

junto1234.df<-data.frame(L=junto1234[,1],n=junto1234[,3], alfa=junto1234[,2],
                         MV=junto1234[,4],alfa.DT.NG1.CV=junto1234[,5],
                         alfa.DT.NG2.CV=junto1234[,6],alfa.DT.IG.CV=junto1234[,7],
                         alfa.DT.IGjstar=junto1234[,8],
                         b.NG1.CV=junto1234[,9],b.NG2.CV=junto1234[,10],
                         b.IG.CV=junto1234[,11])

View(junto1234.df)
dim(junto1234)

basefinal<-junto1234.df[order(junto1234.df$n,-junto1234.df$alfa),]
View(basefinal)



write.csv(basefinal, file = nombrefinal)
