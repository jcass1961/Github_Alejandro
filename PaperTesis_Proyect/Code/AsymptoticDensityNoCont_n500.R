library(MASS)
library("stats4")
library(caTools)
require(tidyverse)
require(ggthemes)

######################################################################################################
######################################################################################################
##### SETEO DE DIRECTORIOS Y DEFINICION DE NOMBRES DE ARCHIVOS

### DIRECTORIO DONDE ESTAN LAS BASES

setwd("../Data/PaperTesis")

L=3

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

base.df<-base0%>% gather(.,key=metodo,value=alfa.est,4:7) %>% filter(n==500)%>%
  mutate(alfa2=alfa,alfa.fact=as.factor(alfa2))
base.df

LegendTitle = "Method"
#legenda.nomb<-c("alfa.MV"="ML","alfa.GA"=expression(Gamma),"alfa.LN"="LN","alfa.LC"="LC")
legenda.nomb<-c("alfa.MV"=expression(paste("  ",widehat(alpha)[ML])), 
                "alfa.GA"=expression(paste("  ",widehat(alpha)[Gamma])), 
                "alfa.LN"=expression(paste("  ",widehat(alpha)[LN])),
                "alfa.LC"=expression(paste("  ",widehat(alpha)[LC])))


base.df$alfa <- factor(base.df$alfa, levels = c("-1.5","-3","-5","-8"),
                  ordered = TRUE,
                  labels=c(expression(alpha==-1.5),
                           expression(alpha==-3),
                           expression(alpha==-5),
                           expression(alpha==-8)))
#nombre.ticks<-c(-5,-3,-1)

head(base.df)

p3<-ggplot(base.df, aes(x=alfa.est,color=metodo,group =metodo)) + 
  facet_wrap(~alfa,labeller = label_parsed,ncol=2, scales = 'free_y') +
  geom_line(stat='density', aes(linetype = metodo), size = 2) +
  geom_vline(aes(xintercept=alfa2),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(breaks=c(-1.5,-3,-5,-8),limits = c(-13,-1))+
  #scale_color_discrete(name = LegendTitle,labels = legenda.nomb)+
  scale_colour_manual(name = " ",
                      values = c("#56B4E9","coral", "magenta","#009E73"),
                      labels = legenda.nomb)+
  scale_linetype_manual(name = " ",
                        values = c("solid", "solid" ,"solid","solid"),
                        labels = legenda.nomb)+
  scale_shape_manual(name = " ",
                     values = c(17, 19, 18,15),
                     labels = legenda.nomb)+
  theme_few()+
  theme(legend.position="top",
        legend.text = element_text( size=28),
        legend.title = element_text( size=28),
        axis.text.y = element_text( size = 28 ),
        axis.text.x = element_text(hjust = 1, size = 28,angle=45),
        axis.title.y = element_text( size = 28 ),
        axis.title.x = element_text( size = 28 ),
        #axis.ticks.length=unit(0.5,"cm"),
        strip.text = element_text(size = 28))+
  labs(x=" ", y = "Density")
  #labs(x=expression(paste(widehat(alpha))), y = "Density")
  p3

######################################## 
getwd()
ggsave("../../figures/Asymptotic_n500_TodoAlfa.eps", plot = last_plot(), device = "eps",scale=2)


