# ==========================================
# TI0111 - Homework III
# Tudo em R (Questões 1 e 2)
# ==========================================

# ---------- Setup ----------
dir.create("figures", showWarnings = FALSE)

need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

need_pkg("ggplot2")
need_pkg("palmerpenguins")

library(ggplot2)
library(palmerpenguins)

# Helper para salvar e mostrar plot
save_plot <- function(p, filename, w = 8, h = 5, dpi = 150) {
  ggsave(filename = file.path("figures", filename), plot = p, width = w, height = h, dpi = dpi)
  print(p)
}

cat("===== HOMEWORK III (R) =====\n\n")

# =========================================================
# QUESTÃO 1 — Exponencial
# =========================================================
cat("=== QUESTÃO 1: Exponencial ===\n")

# Dados (Tabela 1)
x <- c(
  0.99, 2.31, 10.85, 6.15, 10.81, 3.72, 5.75, 4.15, 9.27, 7.84,
  2.31, 10.85, 6.15, 1.81, 3.72, 5.75, 10.40, 10.04, 4.15, 9.27
)

n <- length(x)
sx <- sum(x)

cat("n =", n, "\n")
cat("sum(x) =", sx, "\n\n")

# (2) log-verossimilhança para Exponencial(lambda)
loglik_exp <- function(lambda, x) {
  if (lambda <= 0) return(-Inf)
  n <- length(x)
  n * log(lambda) - lambda * sum(x)
}

# (3) MLE: lambda_hat = n / sum(x)
lambda_hat <- n / sx
cat("MLE lambda_hat =", lambda_hat, "\n")

# (4) Gráfico da log-verossimilhança
lambda_grid <- seq(from = max(1e-6, 0.05 * lambda_hat),
                   to   = 5 * lambda_hat,
                   length.out = 3000)

ll_vals <- vapply(lambda_grid, loglik_exp, numeric(1), x = x)

df_ll <- data.frame(lambda = lambda_grid, loglik = ll_vals)

p_ll <- ggplot(df_ll, aes(x = lambda, y = loglik)) +
  geom_line() +
  geom_vline(xintercept = lambda_hat, linetype = 2) +
  geom_point(aes(x = lambda_hat, y = loglik_exp(lambda_hat, x)), size = 2) +
  labs(
    title = "Log-verossimilhança (Exponencial)",
    x = expression(lambda),
    y = expression(ell(lambda))
  )

save_plot(p_ll, "Q1_loglik.png")

# (5a) Tempo médio estimado: E[X] = 1/lambda
mean_life_hat <- 1 / lambda_hat

# (5b) P(X > 5) = exp(-lambda * 5)
p_gt_5 <- exp(-lambda_hat * 5)

cat("\nTempo medio estimado (anos) =", mean_life_hat, "\n")
cat("P(X > 5) =", p_gt_5, "\n\n")
