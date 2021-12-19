require(ggplot2)
require(ggthemes)

source("../Code/Rutinas/DefineDensidadGI_alfaygamma.R")


# Densidades de la GI0(a, g*, L)
  
alfa1<--20
gama1<--alfa1-1
alfa2<--30
gama2<--alfa2-1

L=3

rango.x<-seq(0,4,0.005)

GI0.1<-function(z) GI0.alfagama(z,alfa1,gama1,L)
GI0.2<-function(z) GI0.alfagama(z,alfa2,gama2,L)


y1<-sapply(rango.x,GI0.1)
y2<-sapply(rango.x,GI0.2)


data1<-data.frame(alfa=factor(alfa1),rango.x,y=y1)
data2<-data.frame(alfa=factor(alfa2),rango.x,y=y2)


resultados<-as.data.frame(rbind(data1,data2))
#View(resultados)
#head(resultados)

#########################################################

LegendTitle = expression(paste(alpha ))

ggplot(resultados, aes(x=rango.x, y=y, group=alfa)) +
  geom_line(aes(linetype=alfa,color=alfa,alpha=I(0.7)),size=2)+
  labs(x=expression(italic(z)), y = "Densities")+
  scale_linetype_manual(name = LegendTitle,values=c("twodash", "dashed"))+
  scale_color_manual(name = LegendTitle,values=c("blue","magenta"))+
  theme_few()+
  theme(text=element_text(size=20, family="serif"),
        legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20),
        axis.text.x = element_text(hjust = 1, size = 20, angle=45),
        axis.title.y = element_text( size = 20),
        axis.title.x = element_text( size = 20),
        #axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 20))
ggsave(file="../../Figures/DensidadGI0L3.pdf")
system("pdfcrop ../../Figures/DensidadGI0L3.pdf ../../Figures/DensidadGI0L3.pdf")

### 

L=8

rango.x<-seq(0,4,0.005)

GI0.1<-function(z) GI0.alfagama(z,alfa1,gama1,L)
GI0.2<-function(z) GI0.alfagama(z,alfa2,gama2,L)


y1<-sapply(rango.x,GI0.1)
y2<-sapply(rango.x,GI0.2)


data1<-data.frame(alfa=factor(alfa1),rango.x,y=y1)
data2<-data.frame(alfa=factor(alfa2),rango.x,y=y2)


resultados<-as.data.frame(rbind(data1,data2))
#View(resultados)
#head(resultados)

#########################################################

LegendTitle = expression(paste(alpha ))

ggplot(resultados, aes(x=rango.x, y=y, group=alfa)) +
  geom_line(aes(linetype=alfa,color=alfa,alpha=I(0.7)),size=2)+
  labs(x=expression(italic(z)), y = "Densities")+
  scale_linetype_manual(name = LegendTitle,values=c("twodash", "dashed"))+
  scale_color_manual(name = LegendTitle,values=c("blue","magenta"))+
  theme_few()+
  theme(text=element_text(size=20, family="serif"),
        legend.position="top",
        legend.text = element_text( size=20),
        legend.title = element_text( size=20),
        axis.text.y = element_text( size = 20),
        axis.text.x = element_text(hjust = 1, size = 20, angle=45),
        axis.title.y = element_text( size = 20),
        axis.title.x = element_text( size = 20),
        #axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 20))
ggsave(file="../../Figures/DensidadGI0L8.pdf")
system("pdfcrop ../../Figures/DensidadGI0L8.pdf ../../Figures/DensidadGI0L8.pdf")

