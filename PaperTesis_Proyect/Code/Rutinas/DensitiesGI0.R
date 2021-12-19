# Version 1: from scratch
dGI0 <- function(z, p_alpha, p_gamma, p_Looks, log=FALSE) {
  
  if(log==TRUE) {
    return(
      (p_Looks*log(p_Looks) + lgamma(p_Looks-p_alpha) + (p_Looks-1)*log(z) ) - 
        (p_alpha*log(p_gamma) + lgamma(-p_alpha) + lgamma(p_Looks) + 
           (p_Looks-p_alpha)*log(p_gamma + z*p_Looks) ) 
    )   
  }
  else { return( 
    ( p_Looks^p_Looks * gamma(p_Looks-p_alpha) * z^(p_Looks-1) ) / 
      (p_gamma^p_alpha * gamma(-p_alpha) * gamma(p_Looks) * (p_gamma + z*p_Looks)^(p_Looks-p_alpha)) 
  )
  }
}


require(ggplot2)
require(ggthemes)

# Densidades de la GI0(a, g*, 1)

# Los gráficos están raros :-()

ggplot(data.frame(x=0), aes(x=x)) +
  stat_function(fun=dGI0, args=list(-1.5, 0.5, 1), n=1000, aes(colour="black")) + 
  stat_function(fun=dGI0, args=list(-2.0, 1.0, 1), n=1000, aes(colour="red")) +
  stat_function(fun=dGI0, args=list(-5.0, 4.0, 1), n=1000, aes(colour="green")) +
  stat_function(fun=dGI0, args=list(-15.0, 14, 1), n=1000, aes(colour="blue")) +
  xlim(0,4) +
  scale_color_manual(name = "Densities",
                     values = c("black", "red", "green", "blue"), # Color specification
                     labels = c("-1.5", "-2", "-5", "-15"))

ggplot(data.frame(x=0), aes(x=x)) +
  stat_function(fun=dGI0, args=list(-1.5, .5, 1), n=1000, aes(colour="black")) + 
  stat_function(fun=dGI0, args=list(-2, 1, 1), n=1000, aes(colour="red")) +
  stat_function(fun=dGI0, args=list(-5, 4, 1), n=1000, aes(colour="green")) +
  stat_function(fun=dGI0, args=list(-15, 14, 1), n=1000, aes(colour="blue")) +
  scale_y_log10() +
  xlim(0,100) +
  scale_color_manual(name = "Densidades",
                   values = c("blue", "red", "green", "black"), # Color specification
                   labels = c("-1.5", "-2", "-5", "-15"))
