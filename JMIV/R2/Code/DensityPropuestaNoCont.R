library(MASS)
library(stats4)
library(caTools)
require(ggplot2)
require(ggthemes)
require(tidyverse)
require(rstudioapi)

library(grDevices)

######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

setwd("../Data/PaperTesis")
L=8

######################################################################################################
######################################################################################################

### NOMBRE BASE A GRAFICAR
nombre.base<-paste("base500_NoCont_L",L,"MVyGAyLNyLC_OPTIM_MOM1medioCONST_VERSION2_FINAL.csv", sep = "")
nombre.base


##########################################################################################
###########################################################################################

### COMIENZO DEL PROGRAMA
base00 <- read.csv(nombre.base)
head(base00)

base0<-subset(base00,alfa.MV!=-20 & alfa.DT.GA.BFGS!=-20
              & alfa.DT.LN.BFGS!=-20 & alfa.LC!=-0)[,1:7]
head(base0)
colnames(base0)<-c("L","alfa","n","alfa.MV","alfa.GA","alfa.LN","alfa.LC")

head(base0)
#View(base)

base.df<-base0%>% gather(.,key=metodo,value=alfa.est,4:7) %>% filter(alfa==-3)
base.df

LegendTitle = "Method"
legenda.nomb<-c("alfa.MV"=expression(widehat(alpha)[ML]), 
                "alfa.GA"=expression(widehat(alpha)[Gamma]), 
                "alfa.LN"=expression(widehat(alpha)[LN]),
                "alfa.LC"=expression(widehat(alpha)[LC]))

#legenda.nomb<-c("alfa.MV"="ML","alfa.GA"=expression(Gamma),"alfa.LN"="LN","alfa.LC"="LC")

n.labs = c("9"="n=9","25"="n=25","49"="n=49","81"="n=81","121"="n=121","500"="n=500")
#nombre.ticks<-c(-5,-3,-1)

nombre.ticks<-factor(base.df$alfa.est, levels = c("-1","-3","-5"),
       ordered = TRUE,
       labels=c(expression(alpha==-1),
                expression(alpha==-3),
                expression(alpha==-5))
)


getwd()

setwd("../../Figures")
cairo_ps(file = "../../figures/DensidadEstimadorNoCont.eps",onefile = FALSE,
         fallback_resolution=600,family="serif",width = 10, height = 10)

p3 <- ggplot(base.df, aes(x=alfa.est,color=metodo)) + 
  #facet_wrap(~n,labeller = labeller(n=n.labs),ncol=3) + 
  facet_wrap(~n, labeller = label_bquote(cols=italic("n="*.(n))),ncol=3) + 
  geom_line(stat='density', aes(linetype = metodo), size = 2) +
  scale_x_continuous(breaks=c(-3),limits = c(-6,-1))+
  #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
  scale_colour_manual(name = "",
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = "",
                        values = c("solid", "solid" ,"solid","solid"),
                        labels = legenda.nomb)+
  # scale_shape_manual(name = " ",
  #                    values = c(17, 19, 18,15),
  #                    labels = legenda.nomb)+
  theme_few()+
  theme(text=element_text(size=30, family="serif"),
        legend.position="top",
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        axis.text.y = element_text(size = 30),
        axis.text.x = element_text(hjust = 1, size = 30, angle=45),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30),
        #axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 30))+
  labs(x="",y = "Density") + 
  geom_vline(aes(xintercept=-3), color="blue", linetype="dashed", size=1) + 
  geom_hline(aes(yintercept=0), color="black",  size=1) 

print(p3)
dev.off()
