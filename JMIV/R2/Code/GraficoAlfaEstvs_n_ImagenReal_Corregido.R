library("wesanderson")
require(tidyverse)
require(ggthemes)
require(rstudioapi)


library(MASS)
library("stats4")

library(grDevices)

set.seed(1234)


### DIRECTORIO DONDE ESTAN LAS BASES
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

setwd("../Data/PaperTesis")


## Estimaciones de alfa para cada una de las muestras elegidas

muestra1<-read.csv("muestra1")[,-1]
muestra2<-read.csv("muestra2")[,-1]
muestra3<-read.csv("muestra3")[,-1]
muestra4<-read.csv("muestra4")[,-1]
muestra5<-read.csv("muestra5")[,-1]


muestra1
muestra2
muestra3
muestra4
muestra5

muestra.est<-rbind(muestra1,muestra2,muestra3,muestra4,muestra5)%>%
  relocate(L,n,alfa.MV,alfa.GA,alfa.LN,alfa.LC)

muestra.f<-muestra.est%>% gather(.,key=metodo,value=estimador,3:6) 

########################################
### Leo base muestras bootstrap
base0 <- read.csv("G:/Mi unidad/Procesamiento de imagenes/KerEst/Data/Tesis/Bootstrap/Bootstrap2000_FinalCon121.csv", sep=";")
head(base0)

# base<-subset(base0,alfa.MV!=-20 & alfa.GA!=-20
#              & alfa.LN!=-20 & alfa.LC!=-20)
base<-base0%>%filter(alfa.MV!=-20 , alfa.GA!=-20,
             alfa.LN!=-20, alfa.LC!=-20)
#View(base)

base.f<-base%>% gather(.,key=metodo,value=estimador,3:6) 
head(base.f)
data.class(base.f)

## Calculo percentiles muestrales

per.LI0<-base.f%>%group_by(L,n,metodo)
per.LI<-per.LI0%>%summarise(per=quantile(estimador,0.025))
per.LS<-base.f%>%group_by(L,n,metodo)%>%summarise(per=quantile(estimador,0.975))
per.LI



percentiles<-left_join(per.LI,per.LS,by=c('L','n','metodo'))


datos<-left_join(muestra.f,percentiles,by=c('L','n','metodo'))%>%
  rename(alfa.est=estimador,li=per.x,ls=per.y)

## Asignoo -20 a los l?mites IC para ML y LC cuando no convergieron
datos[1,5]=-20
datos[1,6]=-20
datos[16,5]=-20
datos[16,6]=-20

head(datos)

############################
### Calculo longitud de intervalos

IC.long<-datos%>%mutate(long=ls-li)%>%select(n,metodo,long)%>%
  pivot_wider(names_from = metodo, values_from = long)
#write.csv(datos.long,file("G:/Mi unidad/Procesamiento de imagenes/KerEst/Data/PaperTesis/IC.long"))

#########################################################
## Genero gr?fico

legenda.nomb<-c("alfa.MV"="ML","alfa.GA"=expression(paste("  ",Gamma)),
                "alfa.LN"="LN","alfa.LC"="LC")

ticks<-n
nombre.ticks<-c(9,25,49,81,121)

nombre.x<-expression(italic(n))

getwd()

cairo_ps(file = "../../figures/AlfaVsTamCincoMuestrasCorregido_v2.eps",onefile = FALSE,
         fallback_resolution=600,family="serif",width = 10, height = 10, pointsize = 12)

p3<-ggplot()+geom_line(data = datos, aes(x = n, y = alfa.est, color=metodo,linetype=metodo),size=2) +
  geom_point(data = datos, aes(x = n, y = alfa.est, color=metodo,shape=metodo),size=3.5) +
  geom_errorbar(data = datos,aes(x = n,  ymin=li, ymax=ls,color=metodo), width=.1,#'#0072B2'
                position=position_dodge(.03))+ 
  labs(x = nombre.x, y = expression(paste(widehat(alpha)))) +  
  scale_x_continuous(trans="log10",breaks=c(9,25 ,49,81,121,500))+
  scale_colour_manual(name = "Metodo", 
                      #values=wes_palette("Darjeeling", n = 4),
                      #values = c("#01AFBB","#DC4E07", "#668cff","magenta"),
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = "Metodo", 
                        values = c("dashed", "twodash" ,"dotted","longdash"),
                        labels = legenda.nomb)+
  scale_shape_manual(name = "Metodo", 
                     values = c(17, 19, 18,15),
                     labels = legenda.nomb)+
  theme_few()+
  theme(text=element_text(size=30, family="serif"),
        legend.position="top",
        legend.text = element_text( size=30),
        legend.title = element_text( size=30),
        axis.text.y = element_text( size = 30 ),
        axis.text.x = element_text(angle=70,hjust = 1, size = 30),
        axis.title.y = element_text( size = 30 ),
        axis.title.x = element_text( size = 30 ),
        strip.text = element_text(size = 30))+
  theme(legend.title=element_blank())
print(p3)
dev.off()

 # ggsave("../../figures/ConModificacionAlejandro/AlfaVsTamCincoMuestrasCorregido_v2.pdf", 
 #        plot = last_plot(), device = "pdf",scale=2)
