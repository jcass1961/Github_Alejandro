#######################################################################################
#######################################################################################
#### GENERA UN REPORTE: ENCUENTRA EL BANDWITH, LA CONSTANTE DE NORMALIZACI?N
#### Y LOS VALORES DE LA FUNCI?N DE DENSIDAD EN UNA GRILLA
#### 
#### REQUIERE: LIBRERIA CONAKE, Y FUNCION kef2
#######################################################################################
#######################################################################################

dke2conIntegrate<-function (vec_data, ker, h, x = NULL,nx, a = 0, b = 1) 
{
  n <- length(vec_data)
  if (is.null(x)) {
    x = seq(min(vec_data), max(vec_data), length.out = 100)
  }
  aux <- matrix(data = vec_data, nrow = length(vec_data), ncol = length(vec_data), 
                byrow = TRUE)
  aux2 <- kef2(x, aux, h, ker, a, b)
  res <- apply(aux2, 1, mean)
  #C <- simp_int(x, res)
  fres<-function(x,t,h,ker){
    auxiliar<-mean(kef2(x,t,h,ker=ker,a=0,b=1))
    
    return(auxiliar)
  }
  
  C<-integrate(Vectorize(fres,"x"),0,Inf, t=vec_data,h=b,ker=ker)$value
 # result <- res/C
  result <- res/C
  return(list(C_n = C, f_n = result))
}

Conakereport2conIntegrate<-function (Vec, ker, h = NULL, nx,a = 0, b = 1) 
{
  if (missing(h)) {
    h1 = cv(Vec, NULL, ker, a, b)
    h = h1$hcv
    bilan = dke2conIntegrate(Vec, ker, h, x = NULL,nx, a = 0, b = 1)
    message("f_n is the estimated p.d.f. obtained")
    if (ker == "GA") {
      message("using gamma kernel and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "BE") {
      message("using extended beta kernel and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "LN") {
      message("using lognormal kernel and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "LN2") {
      message("using lognormal kernel_2 and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "BS") {
      message("using BS and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "IG") {
      message(paste("using inverse Gaussian and h_n by cross validation technique."))
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "IGJstar") {
      message(paste("using inverse Gaussian Jstar and h_n by cross validation technique."))
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
  }
  else {
    bilan = dke2conIntegrate(Vec, ker, h, x = NULL,nx, a = 0, b = 1)
    message("f_n is the estimated p.d.f. obtained")
    if (ker == "GA") {
      message("with a gamma kernel and a given bandwidth h=",h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "BE") {
      message("with an extended beta kernel and a given bandwidth h=",h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "LN") {
      message("with a lognormal kernel and a given bandwidth h=",h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "LN2") {
      message("with a lognormal kernel_2 and a given bandwidth h=", h)
      return(list(h = h, C_n = bilan$C_n))
    }
    else if (ker == "BS") {
      message("using BS and h_n by cross validation technique.")
      return(list(hn_cv = h, C_n = bilan$C_n))
    }
    else if (ker == "IGJstar") {
      message("with a reciprocal Gaussian Jstar kernel and a given bandwidth h=", h)
      return(list(h = h, C_n = bilan$C_n))
    }
  }
}