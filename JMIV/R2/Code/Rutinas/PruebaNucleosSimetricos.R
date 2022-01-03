library(MASS)
library("stats4")
library("compiler")
library("cubature")
library("evmix")
enableJIT(3)

setwd("C:/Users/Usuario/Dropbox/Procesamiento de imagenes/KernelEstimation/Code/OtroPaper")

source("C:../Rutinas/Genera GI en funcion de alfa.R")
source("C:../Rutinas/Define densidad GI0.R")
source("C:../Rutinas/MV Estimador.R")

set.seed(1234567890)

alfa=-1.5
L=1
n=9
grilla=seq(0,100,0.1)
datosGI<-generoGI(alfa,n,L)

# cut and normalize is very quick
fit = fbckden(datosGI, linit = 0.2, bcmethod = "cutnorm")
hist(datosGI, breaks="FD", freq = FALSE)

# but cut and normalize does not always work well for boundary correction
lines(grilla, dbckden(grilla, datosGI, lambda = fit$lambda, bcmethod = "cutnorm"), lwd = 2, col = "red")
lines(grilla, dbckden(grilla, datosGI, lambda = fit$lambda, bcmethod = "simple", proper = T), lwd = 2, col = "blue")
lines(grilla, dbckden(grilla, datosGI, lambda = fit$lambda, bcmethod = "simple"), lwd = 2, col = "green")




legend("topright", c("True Density", "BC KDE using cutnorm",
                     "BC KDE using simple", "KDE Using density"),
       lty = c(1, 1, 1, 2), lwd = c(1, 2, 2, 2), col = c("black", "red", "blue", "green"))



band<-fbckden(grilla, linit = NULL, bwinit = 0.1, kernel = "gaussian",
              extracentres = NULL, bcmethod = "simple", proper = TRUE, nn = "jf96",
              offset = NULL, xmax = NULL, add.jitter = FALSE, factor = 0.1,
              amount = NULL, std.err = TRUE, method = "BFGS", 
              control = list(maxit =10000), finitelik = TRUE)
fhat<-dbckden(grilla, datosGI,  kernel = "epanechnikov",
              bcmethod = "simple", proper = TRUE, nn = "jf96", offset = NULL,
              xmax = NULL, log = FALSE)
