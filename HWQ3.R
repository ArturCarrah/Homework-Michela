#leitura do arquivo .CSV
df <- read.csv("HW1_bike_sharing.csv", stringsAsFactors = FALSE)

#ITEM A =======================================================================================

#número de amostras e dias finais e iniciais
n_obs  <- nrow(df)
data_i <- as.Date(min(df$dteday, na.rm = TRUE))
data_f <- as.Date(max(df$dteday, na.rm = TRUE))

#imprime esses valores
cat("Número de observações:", n_obs, "\n")
cat("Data inicial:", data_i, "\n")
cat("Data final:", data_f, "\n")

#ITEM B =======================================================================================

df$temp_real <- df$temp * 41
df$total_users <- df$casual + df$registered

# Escolher as colunas numéricas relevantes
num_vars <- c("instant", "temp", "temp_real", "casual", "registered", "total_users")

 #Calcular medidas de tendência central e quartis
tabela_sumarios <- data.frame(
  Variável = num_vars,
  Média = sapply(df[num_vars], mean, na.rm = TRUE),
  Mediana = sapply(df[num_vars], median, na.rm = TRUE),
  Q1 = sapply(df[num_vars], quantile, probs = 0.25, na.rm = TRUE),
  Q3 = sapply(df[num_vars], quantile, probs = 0.75, na.rm = TRUE)
)


tabela_sumarios

#ITEM C=======================================================================================

# Atribuindo níveis à variável 'season'
df$season <- factor(df$season,
                    levels = c(1, 2, 3, 4),
                    labels = c("Primavera", "Verão", "Outono", "Inverno"))

# Atribuindo níveis à variável 'weathersit'
df$weathersit <- factor(df$weathersit,
                        levels = c(1, 2, 3, 4),
                        labels = c("Céu limpo", "Nublado", "Chuva fraca", "Chuva forte"))

# Gráfico de barras para 'season' (estação do ano)
ggplot(df, aes(x = season)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Observações por Estação",
       x = "Estação do Ano",
       y = "Número de Dias")

# Gráfico de barras para 'weathersit' (condições meteorológicas)
ggplot(df, aes(x = weathersit)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Distribuição de Observações por Condição Climática",
       x = "Condição Climática",
       y = "Número de Dias")

# Média de usuários por estação
df_estacao <- df %>%
  group_by(season) %>%
  summarise(média_usuarios = mean(total_users, na.rm = TRUE),
            total_usuarios = sum(total_users, na.rm = TRUE),
            .groups = "drop")

# Exibir a tabela com a média de usuários por estação
df_estacao

# Boxplot para verificar a dispersão dos usuários por estação
ggplot(df, aes(x = season, y = total_users)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribuição de Usuários por Estação",
       x = "Estação do Ano", y = "Total de Usuários por Dia")

# Média de usuários por condição climática
df_clima <- df %>%
  group_by(weathersit) %>%
  summarise(média_usuarios = mean(total_users, na.rm = TRUE),
            .groups = "drop")

# Exibir a tabela com a média de usuários por condição climática
df_clima

#ITEM D =======================================================================================
df$dteday <- as.Date(df$dteday)
# Calcular o número total de usuários por dia
df$total_users <- df$casual + df$registered

# Converter a variável 'temp' para temperatura real (°C)
df$temp_real <- df$temp * 41

# Importando a biblioteca ggplot2 para os gráficos
library(ggplot2)

# Gráfico de séries temporais para temperatura real
ggplot(df, aes(x = dteday, y = temp_real)) +
  geom_line(color = "steelblue") +
  labs(title = "Série Temporal — Temperatura Real (°C)",
       x = "Data", y = "Temperatura Real (°C)") +
  theme_minimal()

# Gráfico de séries temporais para número total de usuários
ggplot(df, aes(x = dteday, y = total_users)) +
  geom_line(color = "darkorange") +
  labs(title = "Série Temporal — Número Total de Usuários",
       x = "Data", y = "Número Total de Usuários") +
  theme_minimal()

