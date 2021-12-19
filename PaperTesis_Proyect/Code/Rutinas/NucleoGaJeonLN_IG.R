kef3<-function (x, t, h, ker, a = 0, b = 1) 
{
if (ker == "GAJeon") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dgamma(x, (tval/h) + 1, 1/h)
    return(result)
}
  if (ker == "GA") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dgamma(tval, (x/h) + 1, 1/h)
    return(result)
  }
  else if (ker == "LN") {
    result <- t
    Logic0 <- (0 <= t)
    Logic1 <- (t < 0)
    tval <- result[Logic0]
    result[Logic1] = 0
    result[Logic0] <- dlnorm(tval, meanlog = log(x) + h^2,sdlog = h)
    return(result)
  }
  else if (ker == "IG") {
    result <- t
    Logic0 <- (0 < t)
    Logic1 <- (t <= 0)
    tval <- result[Logic0]
    result[Logic1] <- 0
    result[Logic0] <- dinvgauss(tval ,mean=x,shape=1/h)
    return(result)
  }

}