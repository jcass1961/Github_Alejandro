library("wesanderson")
require(tidyverse)
require(ggthemes)
require(rstudioapi)


library(grDevices)


library(MASS)
library("stats4")


set.seed(1234)


### DIRECTORIO DONDE ESTAN LAS BASES
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

setwd("../Data/PaperTesis")




L=3
alfa=-8
alfa.nomb="-8"
nucleo1<-"MVyGAyLNyLC"
n=25

nombre1<-paste("INFL_",nucleo1,"_alfa",alfa.nomb,"L",L,"n=",n,"_FINAL.csv",sep = "")
nombre.cuantil<-paste("Cuantil_L",L,"_n_",n,".csv",sep="")


Cuantil<-read.csv(nombre.cuantil)
datos1 <- read.csv(nombre1)
Cuantil.alfa<-t(round(Cuantil$alfa8,2))

head(datos1)

legenda.nomb<-c("Set1"=expression(widehat(alpha)[Gamma]),
                "Set2"=expression(widehat(alpha)[LC]),
                "Set3"=expression(widehat(alpha)[LN]),
                "Set4"=expression(widehat(alpha)[ML]))

ticks<-c(Cuantil.alfa,seq(8,20,2))


nombre.ticks<-c(min(Cuantil.alfa),max(Cuantil.alfa),3,5,7,9,11)
cant.black<-length(nombre.ticks)-2

nombre.x<-expression(italic(z))

graf.alfa<-paste("../../figures/CurvaInfluenciaAlfa",alfa.nomb,"L",L,"n",n,".eps",sep="")

cairo_ps(file = graf.alfa,onefile = FALSE,
         fallback_resolution=600,family="serif",width = 10, height = 10)


p1<-ggplot(datos1, aes(x=grilla)) +
  geom_line(aes(y=alfa.GA, colour="Set1",linetype="Set1"),size=2) +
  geom_line(aes(y=alfa.LC, colour="Set2",linetype="Set2"),size=2) +
  geom_line(aes(y=alfa.LN, colour="Set3",linetype="Set3"),size=2) +
  geom_line(aes(y=MV, colour="Set4",linetype="Set4"),size=2) +
  labs(x=nombre.x, y = expression(paste(widehat(alpha))))+
  scale_x_continuous(breaks = nombre.ticks)+
  scale_colour_manual(name = " ", 
                      #values=wes_palette("Darjeeling", n = 4),
                      #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = " ", 
                        values = c("Set1" ="dashed", "Set2"="twodash" ,"Set3"= "dotted","Set4"="longdash"),
                        labels = legenda.nomb)+
  theme_few()+
  theme(text=element_text(size=40, family="serif"),
        legend.position="top",
        legend.text = element_text( size=40),
        legend.title = element_text( size=40),
        axis.text.y = element_text( size = 40 ),
        axis.text.x = element_text(colour = c("red","red",rep("black",cant.black)),
                                   angle=70,hjust = 1, size = 40),
        axis.title.y = element_text( size = 40 ),
        axis.title.x = element_text( size = 40 ),
        strip.text = element_text(size = 40))+
  geom_line(data=datos1, aes(x = grilla,y=alfa),size=2, color="blue") 
print(p1)
dev.off()

