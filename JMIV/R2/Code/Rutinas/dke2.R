dke2<-function (vec_data, ker, bw, x = NULL, a = 0, b = 1) 
{
  n <- length(vec_data)
  if (is.null(x)) {
    x = seq(min(vec_data), max(vec_data), length.out = 100)
  }
  aux <- matrix(data = vec_data, nrow = length(vec_data), ncol = length(vec_data), 
                byrow = TRUE)
  aux <- kef2(x, aux, bw, ker, a, b)
  res <- apply(aux, 1, mean)
  C <- simp_int(x, res)
  result <- res/C
  result <- res/C
  return(list(C_n = C, f_n = result))
}