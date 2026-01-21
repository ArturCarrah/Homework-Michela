# Tabela 1
x <- c(
  0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84,
  2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27
)

# Número de observações
n <- length(x)

# Soma dos valores das observações
sx <- sum(x)

# (2) log-verossimilhança para Exponencial(lambda)
loglik_exp <- function(lambda, x) {
  if (lambda <= 0) return(-Inf)  # Retorna -Inf se lambda for não positivo
  n <- length(x)
  n * log(lambda) - lambda * sum(x)
}

# (2) MLE: lambda_est = n / sum(x)
lambda_est <- n / sx
cat("Estimador de máxima verossimilhança (MLE) lambdâ =", lambda_est, "\n")

# (4) Gráfico da log-verossimilhança
lambda_grid <- seq(from = max(1e-6, 0.05 * lambda_est),
                   to   = 5 * lambda_est,
                   length.out = 3000)

# Calcula os valores de log-verossimilhança para cada lambda
ll_vals <- vapply(lambda_grid, loglik_exp, numeric(1), x = x)

# Prepara os dados para o gráfico
df_ll <- data.frame(lambda = lambda_grid, loglik = ll_vals)

# Gráfico da log-verossimilhança
p_ll <- ggplot(df_ll, aes(x = lambda, y = loglik)) +
  geom_line() +  # Linha do gráfico da log-verossimilhança
  geom_vline(xintercept = lambda_est, linetype = 2, color = "red") +  # Linha vertical para indicar o valor de lambda_hat
  geom_point(aes(x = lambda_est, y = loglik_exp(lambda_est, x)), size = 3, color = "blue") +  # Ponto no valor de lambda_hat
  annotate("text", x = lambda_est, y = loglik_exp(lambda_est, x), label = paste("λ̂ = ", round(lambda_est, 5)), 
           vjust = -1, color = "blue") +  # Anotação para indicar o valor de λ̂
  labs(
    title = "",
    x = expression(lambda),
    y = expression(l(lambda))
  ) +
  theme_minimal()  # Usando um tema minimalista para melhorar a visualização

# Salva o gráfico
print(p_ll)

# (5a) Tempo médio estimado: E[X] = 1/lambda
media_vida <- 1 / lambda_est
cat("\nTempo médio estimado (anos) =", media_vida, "\n")

# (5b) P(X > 5) = exp(-lambda * 5)
p_5 <- exp(-lambda_est * 5)
cat("P(X > 5) =", p_5, "\n\n")
