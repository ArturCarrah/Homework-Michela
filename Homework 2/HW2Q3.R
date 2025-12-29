box_muller_cpu_temp <- function(n) 
  #dados sobre a temperatura (media e desvio padrao)
  mu <- 62
  sigma <- 3.5
  
  #Item 1(a)
  m <- ceiling(n / 2)
  U1 <- runif(m)
  U2 <- runif(m)
  
  #Item 1(b): Box-Muller
  Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
  Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)
  Z  <- c(Z1, Z2)[1:n]
  
  #Item 1(c): Conversao
  T <- mu + sigma * Z
  return(T)
}

T_box <- box_muller_cpu_temp(1000)

T_rnorm <- rnorm(n = 1000, mean = 62, sd = 3.5)
