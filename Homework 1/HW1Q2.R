library(ggplot2) # ggplot2 para fazer os gráficos

# Criando um dataframe com as colunas de idade, nacionalidade, renda e experiência (Obrigado ChatoGPT!)
print("Tabela Original")
cat("\n")

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

print(df)
cat("\n") # O uso do cat é para criar uma nova linha e espaçar as informações

print("Idade")
cat("\n")

# Para pegar só uma coluna do dataframe coloque df$nome_coluna
# Estatistícas de renda, idade e experiência

media_idade = mean(df$idade) 
mediana_idade = median(df$idade)
std_idade = sd(df$idade)
sprintf("Media de idade dos candidatos: %f", media_idade)
sprintf("Mediana das idades dos candidatos: %f", mediana_idade)
sprintf("Desvio padrao das idades dos candidatos: %f", std_idade)

cat("\n")

print("Renda")
cat("\n")

media_renda = mean(df$renda)
mediana_renda = median(df$renda)
std_renda = sd(df$renda)
sprintf("Media da renda desejada pelos candidatos: %f", media_renda)
sprintf("Mediana da renda desejada pelos candidatos: %f", mediana_renda)
sprintf("Desvio padrao das rendas desejadas pelos candidatos: %f", std_renda)

cat("\n")

print("Experiencia")
cat("\n")

media_exp = mean(df$experiencia)
mediana_exp = median(df$experiencia)
std_exp = sd(df$experiencia)
sprintf("Media de anos de experiencia dos candidatos: %f", media_exp)
sprintf("Mediana dos anos de experiencia dos candidatos: %f", mediana_exp)
sprintf("Desvio padrao dos anos de experiencia dos candidatos: %f", std_exp)

cat("\n")

# Agora mostrando os dataframes divididos por nacionalidade e fazendo os devidos cálculos
# Use subset para criar outro dataframe a partir de um dado específico

# Copia e cola infinito abaixo para printar cada dataframe

print("Dataframes divididos por nacionalidade: ")
cat("\n")

df_italiana <- subset(df, nacionalidade == "Italiana")
df_inglesa <- subset(df, nacionalidade == "Inglesa")
df_belga <- subset(df, nacionalidade == "Belga")
df_espanhola <- subset(df, nacionalidade == "Espanhola")
df_francesa <- subset(df, nacionalidade == "Francesa")
df_alema <- subset(df, nacionalidade == "Alemana")

print("Italianos")
cat("\n")

print(df_italiana)
rendaMedia_ita <- mean(df_italiana$renda)
expMedia_ita <- mean(df_italiana$experiencia)
cat("\n")
sprintf("Renda media italiana: %f", rendaMedia_ita)
sprintf("Experiencia media italiana: %f", expMedia_ita)
cat("\n")

print("Ingleses")
cat("\n")

print(df_inglesa)
rendaMedia_eng <- mean(df_inglesa$renda)
expMedia_eng <- mean(df_inglesa$experiencia)
cat("\n")
sprintf("Renda media inglesa: %f", rendaMedia_eng)
sprintf("Experiencia media inglesa: %f", expMedia_eng)
cat("\n")

print("Belgas")
cat("\n")

print(df_belga)
rendaMedia_bel <- mean(df_belga$renda)
expMedia_bel <- mean(df_belga$experiencia)
cat("\n")
sprintf("Renda media dos belgas: %f", rendaMedia_bel)
sprintf("Experiencia media dos belgas: %f", expMedia_bel)
cat("\n")

print("Espanhois")
cat("\n")

print(df_espanhola)
rendaMedia_esp <- mean(df_espanhola$renda)
expMedia_esp <- mean(df_espanhola$experiencia)
cat("\n")
sprintf("Renda media dos espanhois: %f", rendaMedia_esp)
sprintf("Experiencia media dos espanhois: %f", expMedia_esp)
cat("\n")

print("Franceses")
cat("\n")

print(df_francesa)
rendaMedia_fra <- mean(df_francesa$renda)
expMedia_fra <- mean(df_francesa$experiencia)
cat("\n")
sprintf("Renda media dos franceses: %f", rendaMedia_fra)
sprintf("Experiencia media dos franceses: %f", expMedia_fra)
cat("\n")

print("Alemaes")
cat("\n")

print(df_alema)
rendaMedia_ger <- mean(df_alema$renda)
expMedia_ger <- mean(df_alema$experiencia)
cat("\n")
sprintf("Renda media dos alemaes: %f", rendaMedia_ger)
sprintf("Experiencia media dos alemaes: %f", expMedia_ger)
cat("\n")


plot(df$experiencia, df$renda, xlab = "Anos de experiencia", # Plotando o gráfico de Renda x Experiência e traçando uma reta indicando
                                                             # correlação entre os dados
    ylab = "Renda desejada", col = "blue", pch = 19)

model <- lm(df$renda ~ df$experiencia, data = df)
abline(model)

correlacao <- cor(df$experiencia, df$renda, method = 'pearson') # Calculando o coeficiente de correlação de Pearson.
sprintf("Coeficiente de correlacao de Pearson: %f", correlacao) # Por ser próximo de 0.5 a correlação existe e é razoável

# Plotando histogramas para cada nacionalidade

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
