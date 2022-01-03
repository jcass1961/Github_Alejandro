library(MASS)
library("stats4")
library(lattice)
library(compiler)
library(latticeExtra)
library(cubature)
library("statmod")
enableJIT(3)


set.seed(1234567890)

#### GENERA UNA DISTRIBUCION GI
generoGI<-function(alfa,n,L)
{
  Ga<-(L*gamma(-alfa)*gamma(L))/(gamma(-alfa-1)*gamma(L+1))
  XM<-rgamma(n,L,L)
  YM<-rgamma(n,-alfa,Ga)
  datos<-XM/YM
}

############################################################################
#### DEFINE DENSIDAD GI
GI2<-function(z,alfap,L)
{L^L*gamma(L - alfap)/((-alfap - 1)^alfap*gamma(L)*gamma(-alfap))*z^( L - 1)/((-alfap - 1) + L*z)^(L - alfap)}


############################################################################
#### DEFINO LA FUNCION A INTEGRAR
integrand.DT0<-function(x,a,L,datos,b) (GI2(x,a,L)-EstimadorNucleoGama(x,datos,b))^2/(GI2(x,a,L)+EstimadorNucleoGama(x,datos,b))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR
DT<-function(rango,a,L,datos) 
{
  rango[which.min(sapply(rango,function(a) 
    adaptIntegrate(integrand.DT0,lower = 0.1, upper = 10,a,L,datos)$integral))]
}

#############################################################################################
# NUCLEO GAMMA - b es el ancho de banda
EstimadorNucleoGama<-function(x,datos,b)
{
  n<-length(datos)
  EstNuGa<-1/n*sum(sapply(x,function(x) dgamma(datos,shape=x/b+1,scale=b)))
  return(EstNuGa)
}
#############################################################################################
# NUCLEO IGAMMA
nucleoIGama<-function(x,t,b)
{
  nuIGama<-1/sqrt(2*pi*b*t^3)*exp(-1/(2*b*x)*(t/x-2+x/t))
  return(nuIGama)
}
#-----------------------------------------
#Estimación con el Núcleo Inverso Gaussiano
#b es el ancho de banda
EstimadorNucleoIGama<-function(x,datos,b)
{
  n<-length(datos)
  EstimadorNucleo<-1/n*sum(sapply(x,function(x) nucleoIGama(x,datos,b)))
}

EstimadorNucleoIG<-function(x,datos,ancho)
{
  n<-length(datos)
  EstNuIG<-1/n*sum(sapply(x,function(x) dinvgauss(datos,mean=x,shape=1/ancho)))
  return(EstNuIG)
}
#############################################################################################
#genero los datos
n<-100
alfa<--1.5
L<-1
datosGI<-generoGI(alfa,n,L)
z<-seq(0,10,0.1)
b<-0.007
#rango.alfa<-seq(-10,-1.01,0.1)

est.ga<-sapply(z,function(x) EstimadorNucleoGama(x,datosGI,b))
est.Iga<-sapply(z,function(x) EstimadorNucleoIGama(x,datosGI,b))
est.Ig2<-sapply(z,function(x) EstimadorNucleoIG(x,datosGI,b))

cant<-length(z)
metodo<-c(rep("Truth",cant),rep("Gamma Kernel",cant),rep("IG Kernel",cant),
          rep("IG Kernel2",cant))
datos1<-cbind(z,GI2(z,alfa,L))
datos2<-cbind(z,est.ga)
datos3<-cbind(z,est.Iga)
datos4<-cbind(z,est.Ig2)
datos0<-rbind(datos1,datos2,datos3,datos4)
datos<-data.frame(metodo=metodo,w=datos0[,1],estim=datos0[,2])
dim(datos0)


##########################
## HISTOGRAMA CON METODO FREEDMAN DIACONIS - SACO LOS BREAKS
h<-hist(datosGI, breaks = "freedman-diaconis", probability = TRUE,xlim=c(0,6))

##############################################################################
#### CON HISTOGRAMA
windows(width=6, height=4, rescale="fit")
d1<-histogram(datosGI,type = "density",breaks=h$breaks, xlim=c(0,6),ylim=c(0,1),lty=1,xlab="x",ylab="Densities",
              panel=function(x,params,...){
                panel.histogram(x,...,col=NULL)})
d2<-xyplot(estim~ w,group=metodo,lty = 1,lwd=2,type = "l",data=datos,xlim=c(0,6),
           xlab="x",ylab="Densities",
           auto.key = list(space = "top",columns=nlevels(datos$metodo),points = FALSE, lines = TRUE))


as.layer(d1)
d2+d1+d2

rango<-seq(0,10,0.1)
plot(z,est.Ig2)
plot(rango,fhatIG,col="blue",type="l")
##############################################################################

length(rango.alfa)
a<-rango.alfa[which.min(sapply(rango.alfa,function(a) adaptIntegrate(integrand.DT0,lower = 0.1, upper = 10,a,L,datosGI,b)$integral))]
a

##############################################################################
### OTRO GRAFICO
#h2<-2.08*sd(datos1)*100^(-1/5)

y1<-sapply(z,function(x) EstimadorNucleoGama(x,datos1,b))
#y2<-sapply(z,function(x) EstimadorNucleoGama(x,datos1,h2))
distGI<-sapply(z,function(x) GI2(x,alfa,L))
plot(z,y1,col="red",xlim=c(0,10),ylim=c(0,1))
#par(new=TRUE)
#plot(z,y2,col="blue",xlim=c(0,2),ylim=c(0,1))
par(new=TRUE)
plot(z,distGI,xlim=c(0,10),ylim=c(0,1))

datosGI<-datos1/mean(datos1)