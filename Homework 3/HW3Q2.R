library(palmerpenguins)
library(ggplot2)

# Importar dados
penguins_data <- na.omit(penguins)

# Selecionar as colunas relevantes (massa corporal e comprimento do bico)
df <- penguins_data[, c("body_mass_g", "bill_length_mm")]
names(df) <- c("x", "y")  # Renomeando as colunas para x e y

# Verificar os dados
head(df)
     
df <- na.omit(df)  # Remove todas as linhas com valores ausentes

# (1) Gráfico de dispersão
dispersao_g <- ggplot(df, aes(x = x, y = y)) +
  geom_point() +  # Adicionando pontos de dispersão
  labs(
    title = "Grafico de Dispersao entre Massa Corporal e Comprimento do Bico",
    x = "Massa Corporal (g)",
    y = "Comprimento do Bico (mm)"
  )

print(dispersao_g)

# (2) MQO "na mão" (Método dos Mínimos Quadrados)
xv <- df$x
yv <- df$y

xbar <- mean(xv)  # Média de x 
ybar <- mean(yv)  # Média de y 

Sxx <- sum((xv - xbar)^2)
Sxy <- sum((xv - xbar) * (yv - ybar))

# Cálculo (b1) e (b0)
b1_manual <- Sxy / Sxx
b0_manual <- ybar - b1_manual * xbar

cat("Coeficientes da reta de regressão (manual):\n")
cat("  b0 =", b0_manual, "\n")
cat("  b1 =", b1_manual, "\n\n")

# Checagem com lm()
fit <- lm(y ~ x, data = df) 
print(coef(fit)) 
cat("\n")

# Adicionar a reta de regressão ao gráfico de dispersão
dispersao_g_line <- dispersao_g +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red")  # Linha da regressão

print(dispersao_g_line)

# (3) Resíduos, RMSE, R² e diagnósticos
res <- resid(fit)
fitted_vals <- fitted(fit)

# Calcular RMSE
rmse <- sqrt(mean(res^2))

# Calcular R² (cálculo direto)
SSE <- sum(res^2)
SST <- sum((yv - mean(yv))^2)
R2  <- 1 - SSE / SST

cat("Qualidade do ajuste (original):\n")
cat("  RMSE =", rmse, "\n")
cat("  R²   =", R2, "\n\n")

# Resíduos vs Ajustados
p_resid <- ggplot(data.frame(fitted = fitted_vals, resid = res),
                  aes(x = fitted, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(
    title = "Residuos vs Ajustados (original)",
    x = "Valores ajustados",
    y = "Residuos"
  )

# Salvar e mostrar o gráfico
print(p_resid)


# (4) Inserindo outlier artificial e repetindo análise
df_out <- df
idx <- 1  # Você pode trocar o índice do pinguim
df_out$x[idx] <- df_out$x[idx] * 3  # Triplica a massa do 1º pinguim (outlier forte)

# Ajuste com os dados com outlier
fit_out <- lm(y ~ x, data = df_out)

# Calcular resíduos para os dados com outlier
res_out <- resid(fit_out)
rmse_out <- sqrt(mean(res_out^2))

SSE_out <- sum(res_out^2)
SST_out <- sum((df_out$y - mean(df_out$y))^2)
R2_out  <- 1 - SSE_out / SST_out

# Comparação de coeficientes e métricas
cmp <- data.frame(
  modelo = c("Original", "Com outlier"),
  b0 = c(coef(fit)[1], coef(fit_out)[1]),
  b1 = c(coef(fit)[2], coef(fit_out)[2]),
  RMSE = c(rmse, rmse_out),
  R2 = c(R2, R2_out)
)

cat("Comparação (original vs com outlier):\n")
print(cmp)
cat("\n")

# Plot comparando as duas retas (original vs com outlier)
p_compare <- ggplot() +
  geom_point(data = df, aes(x = x, y = y), alpha = 0.7) +
  geom_point(data = df_out[idx, , drop = FALSE], aes(x = x, y = y), size = 3) +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], linetype = 1) +
  geom_abline(intercept = coef(fit_out)[1], slope = coef(fit_out)[2], linetype = 2) +
  labs(
    title = "Regressao: original vs com outlier",
    subtitle = "Ponto grande = observacao artificial; linha tracejada = modelo com outlier",
    x = "body_mass_g (g)",
    y = "bill_length_mm (mm)"
  )

# Salvar e mostrar o gráfico

print(p_compare)
