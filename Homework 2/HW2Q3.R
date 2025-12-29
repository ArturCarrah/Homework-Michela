box_muller_cpu_temp <- function(n){
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

mean_box <- mean(T_box)
sd_box <- sd(T_box)

mean_rnorm <- mean (T_rnorm)
sd_rnorm <- sd(T_rnorm)

min_box <- min(T_box)
max_box <- max(T_box)


min_rnorm <- min(T_rnorm)
max_rnorm <- max(T_rnorm)

#valores empiricos
prob_emp_box_D <- mean(T_box > 68)
prob_emp_rnorm_D <- mean(T_rnorm > 68)

#valores teoricos (vale para os dois)
prob_teo_D <- 1 - pnorm(68,mean = 62,sd = 3.5)

#valores empiricos
prob_emp_box_E <- mean(T_box > 60 & T_box < 65)
prob_emp_rnorm_E <- mean(T_rnorm > 60 & T_rnorm < 65)

#valores teoricos (vale para os dois)
prob_teo_E <- pnorm(65,mean = 62,sd = 3.5) - pnorm (60,mean = 62,sd = 3.5)

#valores empiricos
p_emp_box_F   <- mean(T_box > 75)
p_emp_rnorm_F <- mean(T_rnorm > 75)

#valores teoricos (vale para os dois)
p_teo_F <- 1 - pnorm(75, mean = 62, sd = 3.5)

par(mfrow = c(1, 2))

#Box-Muller
hist(T_box, breaks = 30,
     main = "Temperaturas simuladas (Box-Muller)",
     xlab = "Temperatura (°C)")

#Gerador do R
hist(T_rnorm, breaks = 30,
     main = "Temperaturas simuladas (rnorm)",
     xlab = "Temperatura (°C)")

# Item 4(b): PDF normal teórica sobreposta ao histograma
# (assumindo que já existe um vetor de temperaturas T: use T_box ou T_rnorm)

mu <- 62
sigma <- 3.5

hist(T_box, breaks = 30, probability = TRUE,
     main = "Box-Muller + PDF teórica",
     xlab = "Temperatura (°C)")
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, lwd = 2)

# Se quiser fazer para o outro conjunto também, troque T_box por T_rnorm:
hist(T_rnorm, breaks = 30, probability = TRUE,
     main = "rnorm + PDF teórica",
     xlab = "Temperatura (°C)")
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, lwd = 2)





