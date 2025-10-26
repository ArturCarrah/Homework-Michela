install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)

#leitura do arquivo .CSV
df <- read.csv("HW1_bike_sharing.csv", stringsAsFactors = FALSE)

#ITEM A =======================================================================================
n_obs  <- nrow(df) 
data_i <- as.Date(min(df$dteday, na.rm = TRUE)) 
data_f <- as.Date(max(df$dteday, na.rm = TRUE))

cat("Número de observações:", n_obs, "\n")
cat("Data inicial:", data_i, "\n")
cat("Data final:", data_f, "\n")

#ITEM B =======================================================================================
df$temp_real <- df$temp * 41
df$total_users <- df$casual + df$registered

num_vars <- c("instant", "temp", "temp_real", "casual", "registered", "total_users")

tabela_sumarios <- data.frame(
  Variável = num_vars,
  Média = sapply(df[num_vars], mean, na.rm = TRUE),
  Mediana = sapply(df[num_vars], median, na.rm = TRUE),
  Q1 = sapply(df[num_vars], quantile, probs = 0.25, na.rm = TRUE),
  Q3 = sapply(df[num_vars], quantile, probs = 0.75, na.rm = TRUE)
)
tabela_sumarios

#ITEM C =======================================================================================

# Atribuindo níveis
df$season <- factor(df$season,
                    levels = c(1, 2, 3, 4),
                    labels = c("Primavera", "Verão", "Outono", "Inverno"))

df$weathersit <- factor(df$weathersit,
                        levels = c(1, 2, 3, 4),
                        labels = c("Céu limpo", "Nublado", "Chuva fraca", "Chuva forte"))

# ✅ Criação das tabelas ANTES dos gráficos
df_estacao <- df %>%
  group_by(season) %>%
  summarise(média_usuarios = mean(total_users, na.rm = TRUE),
            total_usuarios = sum(total_users, na.rm = TRUE),
            .groups = "drop")

df_clima <- df %>%
  group_by(weathersit) %>%
  summarise(média_usuarios = mean(total_users, na.rm = TRUE),
            total_usuarios = sum(total_users, na.rm = TRUE),  # ← adicionado
            .groups = "drop")

# Gráfico de barras para 'season'
ggplot(df_estacao, aes(x = season, y = total_usuarios, fill = season)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("springgreen3", "gold", "darkorange", "deepskyblue3")) +
  labs(title = "Total de Usuários por Estação do Ano",
       x = "Estação do Ano", y = "Total de Usuários") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

# Gráfico de barras para 'weathersit'
ggplot(df_clima, aes(x = weathersit, y = total_usuarios, fill = weathersit)) +
  geom_col(width = 0.6, color = "black") +
  scale_fill_manual(values = c("deepskyblue3", "gray70", "gold", "darkred")) +
  labs(title = "Total de Usuários por Condição Climática",
       x = "Condição Climática", y = "Total de Usuários") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")

#ITEM D =======================================================================================
df$dteday <- as.Date(df$dteday)
df$total_users <- df$casual + df$registered
df$temp_real <- df$temp * 41

# Séries temporais
ggplot(df, aes(x = dteday, y = temp_real)) +
  geom_line(color = "steelblue") +
  labs(title = "Série Temporal — Temperatura Real",
       x = "Data", y = "Temperatura Real ") +
  theme_minimal()

ggplot(df, aes(x = dteday, y = total_users)) +
  geom_line(color = "darkorange") +
  labs(title = "Série Temporal — Número Total de Usuários",
       x = "Data", y = "Número Total de Usuários") +
  theme_minimal()