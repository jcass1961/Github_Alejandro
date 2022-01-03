

cuantil.GI0<-function(n,alfa,gama,L)
{
  cuantilGI0<-rep(0,n)
  for (i in 1 : n)
    cuantilGI0[i]<-qf(-alfa/gama*i,2*L,-2*alfa)
  
}

cuantil.GI0(9,-3,2,3)
