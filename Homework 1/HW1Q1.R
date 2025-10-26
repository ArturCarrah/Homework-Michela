library(ggplot2) # ggplot2 para gráficos

x <- c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9, 24.6, 19.4, 12.3, 15.9, 20.1, 17.0, 22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 20.9, 21.4, 18.0, 24.3, 11.8, 17.9, 18.7, 12.8, 15.5, 19.2, 13.9, 28.6, 19.4, 21.6, 13.5, 24.6, 20.0, 24.1, 9.0, 17.6, 25.7, 20.1, 13.2, 23.7, 10.7, 19.0, 14.5, 18.1, 31.8, 28.5, 22.7, 15.2, 23.0, 29.6, 11.2, 14.7, 20.5, 26.6, 13.3, 18.1, 24.8, 26.1, 7.7, 22.5, 19.3, 19.4, 16.7, 16.9, 23.5, 18.4)

y = sort(x)

# x é um vetor com os dados de emissão diários. Para calcular algumas medidas, criamos um vetor ordenado y contendo os valores de x 

# Gambiarra(?) para achar a moda do conjunto. A função de moda não é nativa do R. Abaixo o link que forneceu uma resposta funcional
#https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Cálculo de média, moda e mediana

mediana = median(x)
moda = Mode(x)
mean = mean(x)

sprintf("Media: %f", mean)
sprintf("Moda: %f", moda)
sprintf("Mediana: %f", mediana)



amplitude = y[80] - y[1] # Amplitude é definida como a diferença entre o valor máximo e mínimo do conjunto
sprintf("Amplitude: %f", amplitude)

var = var(x) # Variancia de amostra (dividido por n-1)
sprintf("Variancia: %f", var)

std = sd(x) # Desvio padrão de amostra (dividido por n-1)
sprintf("Desvio Padrao: %f", std)

coef_var = var/mean # Coeficiente de variação = variância/média
sprintf("Coeficiente de Variacao: %f", coef_var)



df <- as.data.frame(x) # Colocando x em um dataframe para fazer os gráficos

hist <- ggplot(df, aes(x=x)) + 
  geom_histogram(binwidth=5, color="black", fill="white")  # Histograma

hist

boxplot <- ggplot(df, aes(y=x))+
            geom_boxplot(fill = "dark green") +
            ylab("Emissoes de Gases (Unidades de Medida)") # Boxplot

boxplot

print("Valor dos elementos dos quartis: ")

quantils <-quantile(y, c(0.25,0.5,0.75), type = 2) # Tipo 2 tira a média se for par
quantils

IQR = 22.95 - 15.35 # Inter Quartile Range

sprintf("IQR: %f", IQR) 
