# Distribuição binomial com n=50 e p=0.7

x <- 0:50

pmf = dbinom(x, 50, 0.7) # Função da PMF (51 valores, então pmf[1] = 0 e P(X = x) = pmf[x+1])       

plot(x,pmf,type='h',col='red',lwd=2, # Gráfico da PMF
        xlab= 'x', ylab='P(x)')

abline(h=0) # Adiciona uma linha no valor de P(x) = 0 para aumentar a legibilidade


cdf = c(0, cumsum(pmf)) # Calculando a CDF a partir da PMF

plot(cdf ,type='h',col='blue',lwd=2, # Gráfico da CDF
         xlab= 'x', ylab='P(X <= x)')

abline(h=0) # Adiciona uma linha no valor de P(x) = 0 para aumentar a legibilidade



# O valor esperado de uma distribuição binomial é n*p, nesse caso sendo 50*0.7 = 35.

Ex = weighted.mean(x, pmf) # O valor esperado é uma média ponderada usando as probabilidades de pesos
cat("Valor Esperado:", Ex, "\n")

# A variância de uma distribuição binomial é n*p*(1-p), nesse caso sendo 50*0.7*0.3 = 10.5

Var = Ex*0.3
cat("Variância:", Var, "\n")

cat("Desvio-Padrão:", sqrt(Var), "\n")

# Para X>=20:
s = 0
for(i in 21:50){
    s <- s + pmf[i]
}
cat("P(X >= 20):", s, "\n")



# Para 30 < X < 43:
s = 0
for(i in 32:43){
    s <- s + pmf[i]
}
cat("P(30 < X < 43):", s, "\n")

# Para X = 31:


cat("P(X = 31):", pmf[32], "\n")

