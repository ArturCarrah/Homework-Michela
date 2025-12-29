# Como temos uma baixíssima probabilidade e um grande número de pessoas, pode-se modelar essa pesquisa como uma distribuição de Poisson. Além disso, como temos p = 10^-7 e n = 10^7, intuitivamente o número de pessoas que ganhariam o prêmio em um dia seria 10^7*10^-7 = 1 pessoa. Portanto, o parâmetro dessa distribuição seria 1.

x <- 0:20
pmf = dpois(x, 1)

# Veja também que podemos modelar esse prêmio como sendo uma distribuição binomial, bem mais simples. Isso ocorre porque o valor do número de tentativas (n) da distribuição binomial é muito grande. Portanto, isso faz com que a distribuição binomial tenda a se distribuir como uma de Poisson.

pmf2 = dbinom(x,10000000, 0.0000001)

plot(x,pmf,type='h',col='red',lwd=2,
        xlab= 'x', ylab='P(x)')
points(x,pmf, pch=19, col='red')

abline(h=0) # Adiciona uma linha no valor de P(x) = 0 para aumentar a legibilidade

plot(x,pmf2,type='h',col='blue',lwd=2,
        xlab= 'x', ylab='P(x)')
points(x,pmf2, pch=19, col='blue')

abline(h=0) # Adiciona uma linha no valor de P(x) = 0 para aumentar a legibilidade


# Valor esperado e Variância

Ex_Poisson = weighted.mean(x, pmf) # O valor esperado é uma média ponderada usando as probabilidades de pesos
cat("Valor Esperado da dist. de Poisson:", Ex_Poisson, "\n")

Ex_Binom = weighted.mean(x, pmf2) 
cat("Valor Esperado da dist. Binomial:", Ex_Binom, "\n")

cat("Variância da dist. de Poisson:", Ex_Poisson, "\n")

cat("Variância da dist. Binomial:", Ex_Binom*(1-0.0000001), "\n")


# A probabilidade de você ganhar dentre os outros vencedores é dado pela lei da probabilidade total

s = 0
for(i in 1:20){
    s <- s + pmf[i+1]*(1/i)
}

cat("Probabilidade de ganhar:", s)


y = rpois(1000000, 1)

hist(y, prob = TRUE, breaks = 9, border = 'black', right = FALSE,
    main = " ")
