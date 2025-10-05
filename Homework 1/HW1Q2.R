library(ggplot2)
df <- data.frame(
  idade = c(28, 34, 46, 26, 37, 29, 51, 31, 39, 43, 58, 44, 25, 23, 52, 42, 48, 33, 38, 46),
  nacionalidade = c("Italiana", "Inglesa", "Belga", "Espanhola", "Italiana", "Espanhola", 
                    "Francesa", "Belga", "Italiana", "Italiana", "Italiana", "Inglesa", 
                    "Francesa", "Espanhola", "Italiana", "Alemana", "Francesa", "Italiana", 
                    "Alemana", "Italiana"),
  renda = c(2.3, 1.6, 1.2, 0.9, 2.1, 1.6, 1.8, 1.4, 1.2, 2.8, 3.4, 2.7, 1.6, 1.2, 
            1.1, 2.5, 2.0, 1.7, 2.1, 3.2),
  experiencia = c(2, 8, 21, 1, 15, 3, 28, 5, 13, 20, 32, 23, 1, 0, 29, 18, 19, 7, 12, 23)
)

print("Idade")

media_idade = mean(df$idade)
mediana_idade = median(df$idade)
std_idade = sd(df$idade)
media_idade
mediana_idade
std_idade

print("Renda")

media_renda = mean(df$renda)
mediana_renda = median(df$renda)
std_renda = sd(df$renda)
media_renda
mediana_renda
std_renda

print("Experiencia")

media_exp = mean(df$experiencia)
mediana_exp = median(df$experiencia)
std_exp = sd(df$experiencia)
media_exp
mediana_exp
std_exp


print(df)

print("Dataframes divididos")

df_italiana <- subset(df, nacionalidade == "Italiana")
df_inglesa <- subset(df, nacionalidade == "Inglesa")
df_belga <- subset(df, nacionalidade == "Belga")
df_espanhola <- subset(df, nacionalidade == "Espanhola")
df_francesa <- subset(df, nacionalidade == "Francesa")
df_alema <- subset(df, nacionalidade == "Alemana")

print(df_italiana)
rendaMedia_ita <- mean(df_italiana$renda)
expMedia_ita <- mean(df_italiana$experiencia)
rendaMedia_ita
expMedia_ita

print(df_inglesa)
rendaMedia_eng <- mean(df_inglesa$renda)
expMedia_eng <- mean(df_inglesa$experiencia)
rendaMedia_eng
expMedia_eng

print(df_belga)
rendaMedia_bel <- mean(df_belga$renda)
expMedia_bel <- mean(df_belga$experiencia)
rendaMedia_bel
expMedia_bel

print(df_espanhola)
rendaMedia_esp <- mean(df_espanhola$renda)
expMedia_esp <- mean(df_espanhola$experiencia)
rendaMedia_esp
expMedia_esp

print(df_francesa)
rendaMedia_fra <- mean(df_francesa$renda)
expMedia_fra <- mean(df_francesa$experiencia)
rendaMedia_fra
expMedia_fra

print(df_alema)
rendaMedia_ger <- mean(df_alema$renda)
expMedia_ger <- mean(df_alema$experiencia)
rendaMedia_ger
expMedia_ger


plot(df$experiencia, df$renda, xlab = "Anos de experiencia", 
    ylab = "Renda desejada", col = "blue", pch = 19)

model <- lm(df$renda ~ df$experiencia, data = df)
abline(model)

correlacao <- cor(df$experiencia, df$renda, method = 'pearson')
correlacao


idade_ita <- df_italiana$idade
renda_ita <- df_italiana$renda

hist(idade_ita, main="Idade dos Italianos",
xlab="Idade", col="green")

hist(renda_ita, main="Renda dos Italianos",
xlab="Renda", col="green")


idade_eng <- df_inglesa$idade
renda_eng <- df_inglesa$renda

hist(idade_eng, main="Idade dos Ingleses",
xlab="Idade")

hist(renda_eng, main="Renda dos Ingleses",
xlab="Renda")


idade_bel <- df_belga$idade
renda_bel <- df_belga$renda

hist(idade_bel, main="Idade dos Belgas",
xlab="Idade", col="yellow3")

hist(renda_bel, main="Renda dos Belgas",
xlab="Renda", col="yellow3")



idade_esp <- df_espanhola$idade
renda_esp <- df_espanhola$renda

hist(idade_esp, main="Idade dos Espanhois",
xlab="Idade", col="red")

hist(renda_esp, main="Renda dos Espanhois",
xlab="Renda", col="red")



idade_fra <- df_francesa$idade
renda_fra <- df_francesa$renda

hist(idade_fra, main="Idade dos Franceses",
xlab="Idade", col="blue")

hist(renda_fra, main="Renda dos Franceses",
xlab="Renda", col="blue")



idade_ger <- df_alema$idade
renda_ger <- df_alema$renda

hist(idade_ger, main="Idade dos Alemaes",
xlab="Idade", col="yellow")

hist(renda_ger, main="Renda dos Alemaes",
xlab="Renda", col="yellow")
