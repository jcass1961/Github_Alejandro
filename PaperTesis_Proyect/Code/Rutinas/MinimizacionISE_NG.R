library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("Conake")
library("statmod") ## nucleo IG

enableJIT(3)

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/OtroPaper")
#setwd("C:/Users/julia/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/OtroPaper")

source("C:../Rutinas/Genera GI en funcion de alfa y gamma.R")
source("C:../Rutinas/DefineDensidadGI_alfaygamma.R")
source("C:../Rutinas/Alfa0Gama0PorMomentos.R")
source("C:../Rutinas/LogCumulantAlfayGamma.R")

source("C:../Rutinas/LogLikelihoodGI0.R")
source("C:../Rutinas/DistanciaTriangularNG1conCteAlfaGama.R")


source("C:../Rutinas/nucleos.R")                ###############
source("../Rutinas/CrossValidationdeR.R")     ###############
source("../Rutinas/MOM_1medio.R")
#source("C:../Rutinas/Conakereport2conIntegrate.R")                ###############
source("C:../Rutinas/Conakereport2.R") 
source("C:../Rutinas/boptimoNucleoGamma_alfaygama.R")

getwd()
set.seed(1775)

############################################################################
#### DEFINE ESTIMADOR NUCLEO GAMA VERSION 1
EstimadorNucleoGama1<-function(x,datos,ancho,Cte)
{
  n<-length(datos)
  EstNuGa<-1/(n*Cte)*sum(sapply(x,function(x) dgamma(datos,shape=x/ancho+1,scale=ancho)))
  return(EstNuGa)
}

############################################################################
#### DEFINO LA FUNCION A INTEGRAR CON NUCLEO GAMMA VERSION 1
integrand.ISE.NG1<-function(x,ancho,a,ga,L,datos,Cte) (GI0.alfagama(x,a,ga,L)-EstimadorNucleoGama1(x,datos,ancho,Cte))^2

############################################################################
#### DEFINO ISE en funcion de b
ISE<-function(ancho,a,ga,L,datos,Cte)
  adaptIntegrate(integrand.ISE.NG1,lower = 0, upper = 400,ancho,a,ga,L,datos,Cte)$integral


############################################################################
############################################################################

r<-500
L<-3
alfa<--8
gama<--alfa-1

n<-9

alfa.nom<-"-1punto5"

 ker<-"GA"
# nucleo.nomb<-"GAyMVyLC"
# dist0="DT"
# 
# repli.L.alfa<-data.frame()
# 
# nombre<-paste("base500NoContDOSPAR","_L",L,nucleo.nomb,"n121y500",alfa.nom,"ygama",gama1,"NuevoLSalfayGaEstre",sep = "")
# #random1<-replicate(285,generoGI.alfagama(alfa,gama0,9,L))


######################################################################################################
############################################################################
############################################################################
##### PROGRAMA PRINCIPAL

  
datosGI<-generoGI.alfagama(alfa,gama,n,L)

############################################################################
#### Minimizo ISE en funcion de b

if (alfa.mom0(datosGI,L)[1]!=0) 
{x0<-alfa.mom0(datosGI,L)
alfa.ini<-x0[1]
gama.ini<-x0[2]*(-x0[1]-1)
} else 
{alfa.ini<--1.5
gama.ini<-mean(datosGI)*(-(-1.5)-1)
}

bopt<-boptim(alfa.ini,gama.ini,L,n)
b<-cv(datosGI,ker=ker)$hcv
const<-Conakereport2(datosGI,ker=ker,h=bopt,nx=100,a = 0, b = 1)$C_n
  
ISE.b<-function(ancho) ISE(ancho,alfa,gama,L,datosGI,const)
ISE.b.alfaMOM<-function(ancho) ISE(ancho,alfa.ini,gama.ini,L,datosGI,const)
ISE.b(2)

b.ISE<-optim(bopt,ISE.b, method = "L-BFGS-B",lower = 10^(-3), upper = 5)$par
b.ISE.alfaMOM<-optim(bopt,ISE.b.alfaMOM, method = "L-BFGS-B",lower = 10^(-3), upper = 5)$par

# x.b<-seq(0.001,8,0.01)
# y.b<-sapply(x.b,ISE.b)

b.ISE
b.ISE.alfaMOM
bopt
b

plot(x.b,y.b,xlim=c(0,2))
  