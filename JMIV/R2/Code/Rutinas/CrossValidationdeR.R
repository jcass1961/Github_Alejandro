cv<-function (Vec, bw = NULL, ker, a = 0, b = 1) 
{
  if (is.null(bw)) {
    bw = seq((max(Vec) - min(Vec))/200, (max(Vec) - min(Vec))/2, 
             length.out = 100)
  }
  result1 <- 0
  result2 <- 0
  x = seq(min(Vec), max(Vec), length.out = 100)
  n1 <- length(x)
  n2 <- length(Vec)
  m1 = matrix(0, n1, length(Vec))
  m2 = matrix(0, n2, n2)
  kef2 <- Vectorize(kef2, vectorize.args = c("x", "t"))
  for (k in 1:length(bw)) {
    m1 <- outer(x, Vec, kef2, bw[k], ker)
    m2 <- outer(Vec, Vec, kef2, bw[k], ker)
    res1 <- apply(m1, 1, mean)
    diag(m2) <- 0
    res2 <- apply(m2, 1, sum)
    result1[k] = simp_int(x, res1^2)
    result2[k] = (2/((n2 - 1) * n2)) * sum(res2)
  }
  CV = result1 - result2
  index <- which.min(CV)
  hcv <- bw[index]
  return(list(hcv = hcv, CV = CV, bw = bw))
}