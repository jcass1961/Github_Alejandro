###############################################
#####    Núcleo Gamma 2                    #####
##### t es la variable, va la muestra      #####
##### x es la grilla, va en el parámetro   #####
###############################################

dgamma2<-function (t,x,h) 
{
  {
    result <- t
    Logic0 <- (0 <= t & t < 2*h)
    Logic1 <- (2*h <= t)
    Logic2 <- (t < 0)
    tval <- result[Logic0]
    tval1 <- result[Logic1]
    result[Logic2] = 0
    result[Logic0] <- dgamma(tval, 1/4*(x/h)^2 + 1, 1/h)
    result[Logic1] <- dgamma(tval1, x/h, 1/h)
    return(result)
  }
} 


