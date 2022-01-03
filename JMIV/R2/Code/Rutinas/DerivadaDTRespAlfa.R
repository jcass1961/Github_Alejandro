### Derivada DT respecto de alfa

derivDTalfa<-function(z,alfa,L,f1,f2) 
{
  derivGIalfa(z,alfa,L)*(2*(f1(z,alfa,L)-f2(z,datos))/(f1(z,alfa,L)+f2(z,datos))-
                           (f1(z,alfa,L)-f2(z,datos))^2/(f1(z,alfa,L)+f2(z,datos))^2)
}

             



