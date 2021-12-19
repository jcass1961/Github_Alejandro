library(MASS)
library(RColorBrewer)
library("stats4", lib.loc="C:/Program Files/R/R-2.15.3/library")
library(lattice)
library(compiler)
library(latticeExtra)
library(cubature)
enableJIT(3)


set.seed(1234567890)

rango.alfa<-seq(-10,-1.01,0.1)
len<-length(rango.alfa)
vector.dt<-rep(0,len)

#### GENERA UNA DISTRIBUCION GI
generoGI<-function(alfa,n,L,k)
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
#### DEFINE ESTIMADOR DE MAXIMA VEROSIMILITUD

MV<- function(a,datos,n,L)
{ argLL0<-function(a,datos,n,L)
  -(n*(log(gamma(L-a))-a*log(-a-1)-log(gamma(-a)))+(L-1)*sum(log(datos))-(L-a)*sum(log(-a-1+L*datos)))
  
  argLL<-function(a) argLL0(a,datos,n,L)
  alfa.MV<-mle(argLL, start = list(a=-1), method = "L-BFGS-B",lower =-10,upper=-1.01,
      nobs=NROW(datos))
  return(alfa.MV)
}
  
############################################################################
#### DEFINO LA FUNCION A INTEGRAR
integrand.DT0<-function(x,a,L,datos) (GI2(x,a,L)-EstimadorNucleoIgama(x,datos))^2/(GI2(x,a,L)+EstimadorNucleoIgama(x,datos))

############################################################################
#### DEFINE EL ESTIMADOR DE DISTANCIA TRIANGULAR
DT<-function(rango,a,L,datos) 
{
  rango[which.min(sapply(rango,function(a) 
  adaptIntegrate(integrand.DT0,lower = 0.1, upper = 10,a,L,datos)$integral))]
}

############################################################################
#### DEFINE NUCLEO IGAMA

nucleoIGama<-function(x,t,b)
{
  nuIGama<-1/sqrt(2*pi*b*t^3)*exp(-1/(2*b*x)*(t/x-2+x/t))
  return(nuIGama)
}

############################################################################
#### DEFINE ESTIMADOR NUCLEO IGAMA
EstimadorNucleoIgama<-function(x,datos)
{
  n<-length(datos)
  b<-1/(5*sqrt(n))
  EstimadorNuc<-1/n*sum(nucleoIGama(x,datos,b))
  return(EstimadorNuc)
}

############################################################################
##### DEFINE ESTIMADOR DE MOMENTO 1/2
mom_1_2 = function(muestra, L)
{
  mom_sample_1_2 = sum(muestra^(0.5))/length(muestra)
  #browser()
  
  f <- function(x,mom_sample_1_2,L){-mom_sample_1_2*gamma(L)/gamma(L+0.5)*(L/(-x-1))^(0.5) + gamma(-x-0.5)/gamma(-x)}
  if (f(-15,mom_sample_1_2,L)*f(-1.01,mom_sample_1_2,L)<0)
  {alpha_mom_1_2 <- uniroot(f, c(-15,-1.01), tol = 0.0001,mom_sample_1_2,L)
   alpha_mom_1_2 <-alpha_mom_1_2$root
  }
  else
    alpha_mom_1_2 <-0
  return(alpha_mom_1_2)
  
}

############################################################################
##### DEFINE ESTIMADOR LOGCUMULANT

logcumulant_fo_uniroot = function(muestra, L){
  
  tm <- length(muestra)
  k_hat_1 <- sum(log(muestra))/tm
  f <- function(x,k_hat_1,L){k_hat_1 + log(L) - log(-x-1) - digamma(L) + digamma(-x)}
  if (f(-15,k_hat_1,L)*f(-1.01,k_hat_1,L)<0)
    alpha_log <- uniroot(f, c(-15,-1.01), tol = 0.0001,k_hat_1,L)$root
  else
    alpha_log <- 0
  
  return(alpha_log)
}

#### GENERA CONTAMINADOS

### GENERACIÓN DE BERNOULLI
ber<-function(epsilon,n)
{rber<-rbinom(n,1,epsilon)}

genero.cont<-function(alfa,n,L,k,epsilon)
{
  datos.ber<-ber(epsilon,n)
  datosGI<-generoGI(alfa,n,L,0)
  datosGI.cont<-generoGI(alfa,n,L,k) 
  datos.cont<-(1-datos.ber)*datosGI+datos.ber*datosGI.cont
}


##########################################
#Funciones para la contaminacion
uno<-function(alfa1,alfa2, n, L, epsilon)
{
  GIl<-generoGI(alfa1,n,L,0)
  GIc<-generoGI(alfa2, n, L,0)
  
  e1<- rbinom(n,1,epsilon)
  
  datosc<- (1 - e1)* GIl + e1*GIc
  return(datosc)
}

dos=function(alfa1,n,L,epsilon, corner)
{
  Gl<-generoGI(alfa1,n,L,0)
  
  e1<- rbinom(n,1,epsilon)
  datosc <- (1 - e1)* Gl + e1*corner
  return(datosc)
}

tres<-function(alfa1,n,L,k,epsilon)
{datosc<-genero.cont(alfa1,n,L,k,epsilon)
 return(datosc)
}


genera_gi0_cont2<-function(caso, alfa1,alfa2, n, L, epsilon, corner,k)
  # caso = "uno","dos", "tres" caso de contaminaci\'on.
  # caso =1  contaminacion de tipo (1-epsilon)* GI0(alfa1, gamma tal que esperanza es 1, L) + epsilon *GI0(alfa2, gamma_tal_que_esperanza = 1, L)
  # caso = 2 una proporci\'on epsilon de valores arbitrariamente grandes, con valor corner
  # caso = 3 contaminación del parámetro gamma con probabilidad epsilon
  
{
  switch(caso,
         uno=uno(alfa1,alfa2,n,L,epsilon),
         dos=dos(alfa1,n,L,epsilon, corner),
         tres=tres(alfa1,n,L,k,epsilon))
}


############################################################################
##### PROGRAMA PRINCIPAL
estimadores.tiempo<-function(alfa,alfa2,n,L,k,epsilon,caso,corner)
{
  #browser()
  datosGI.cont0<-genera_gi0_cont2(caso,alfa,alfa2, n, L, epsilon, corner,k)
  datosGI<-datosGI.cont0/mean(datosGI.cont0)

  
  tiempo.MV<-system.time(MV<-MV(a,datosGI,n,L))[[3]]
  
 # tiempo.DT1<-system.time(alfa.DT1<-DT(rango.alfa,a,L,datosGI))[[3]]
  
# tiempo.DT0<-system.time(vector.dt<-sapply(rango.alfa,function(a) adaptIntegrate(integrand.DT0, 
#                                  lower = 0.1, upper = 10,a,L,datosGI)$integral))[[3]]  
  
  
  
  tiempo.DT<-system.time(alfa.DT<-rango.alfa[which.min(sapply(rango.alfa,
              function(a) adaptIntegrate(integrand.DT0,lower = 0.1, upper = 10,a,L,datosGI)$integral))])[[3]]
  
  tiempo.MOM12<-system.time(alfa.MOM12<-mom_1_2(datosGI,L))[[3]]
  
  tiempo.LOGCUM<-system.time(alfa.LOGCUM<-logcumulant_fo_uniroot(datosGI,L))[[3]]
  


    #salida.alfas<-c(L,alfa,n,MV@coef[[1]],alfa.DT,alfa.MOM12,alfa.LOGCUM,
    #tiempo.MV,tiempo.DT,tiempo.DT1,tiempo.MOM12,tiempo.LOGCUM)

salida.alfas<-c(L,alfa,n,MV@coef[[1]],alfa.DT,alfa.MOM12,alfa.LOGCUM,
                tiempo.MV,tiempo.DT,tiempo.MOM12,tiempo.LOGCUM)
  return(salida.alfas)
}

############################################################################
##### PARA GENERAR LAS REPLICACIONES

estimaciones.tiempo1<-function(r,alfa,alfa2,L,k,epsilon,caso,corner)
{
  repli.L.alfa<-t(replicate(r, estimadores.tiempo(alfa,alfa2,9,L,k,epsilon,caso,corner)))
  
  for(n in c(25,49,81,121,1000))
    #for(n in c(25,49,81,121))
  {
    #browser()
    print(n)
    repli.L.alfa0<-t(replicate(r, estimadores.tiempo(alfa,alfa2,n,L,k,epsilon,caso,corner)))
    repli.L.alfa<-rbind(repli.L.alfa,repli.L.alfa0)
  }
  
  return(repli.L.alfa)
}

estimaciones.tiempo2<-function(r,alfa2,L,k,epsilon,caso,corner)
{
  repli.L<-estimaciones.tiempo1(r,-1.5,alfa2,L,k,epsilon,caso,corner)
 
  for(alfa in c(-3,-5,-8))
  {
    repli.L0<-estimaciones.tiempo1(r,alfa,alfa2,L,k,epsilon,caso,corner)
    repli.L<-rbind(repli.L,repli.L0)
  }
  #print(repli.L)
  return(repli.L)
}

############################################################################
##### PARA GENERAR LA BASE DE DATOS

r<-200
L<-3
alfa2<--15
k<-2
epsilon<-0.01
caso<-"dos"
corner<-100

nombre<-"base200JSTAR_ContCasoDos_L3_a215_eps01"


setwd("C:/Users/usuario/Dropbox/Estimaciones alfa y gama/ProgramasparaJSTAR")
getwd()

repli<-estimaciones.tiempo2(r,alfa2,L,k,epsilon,caso,corner)
base1<-data.frame(L=repli[,1],n=repli[,3], alfa=repli[,2],MV=repli[,4],
                  DT=repli[,5],MOM12=repli[,6],LogCum=repli[,7],
                  TMV=repli[,8],TDT=repli[,9],TMom=repli[,10],TLCum=repli[,11])


write.csv(base1, file = nombre)


