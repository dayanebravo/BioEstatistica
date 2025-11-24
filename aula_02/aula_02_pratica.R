################ TEMA 1 – MEDIDAS DE TENDÊNCIA CENTRAL ########################

### MEDIA
idades <- c(25, 30, 35, 40, 45)  
media <- mean(idades)  
print(media)


idades_outlier <- c(25, 30, 35, 40, 45, 100)  
media_outlier <- mean(idades_outlier)  
print(media_outlier)  





### MEDIANA
idades <- c(25, 30, 35, 40, 45)  
mediana <- median(idades)  
print(mediana) 


idades_outlier <- c(25, 30, 35, 40, 45, 100)  
mediana_outlier <- median(idades_outlier)  
print(mediana_outlier)  







################ TEMA 2 – MEDIDAS DE DISPERSÃO ################################

### VARIANCIA
idades <- c(25, 30, 35, 40, 45)
variancia <- var(idades)  
print(variancia)




### DESVIO PADRAO
idades <- c(25, 30, 35, 40, 45)
desvio_padrao <- sd(idades)
print(desvio_padrao)

# lembrando que a variancia é o quadrado do desvio padrão
desvio_padrao <- sqrt(variancia)
print(desvio_padrao)




### AMPLITUDE
idades <- c(25, 30, 35, 40, 45)
amplitude <- max(idades) - min(idades)
print(amplitude) 






################ TEMA 3 – MÉDIA GEOMÉTRICA, HARMÔNICA, QUARTIS E PERCENTIS ####

### MÉDIA GEOMÉTRICA
# Pela fórmula
valores <- c(1.5, 2.0, 3.0)
media_geometrica <- prod(valores)^(1/length(valores))
print(media_geometrica)


# Pela função
# Instalar (se necessário) e carregar o pacote
if(!require(psych)) install.packages('psych')
library(psych)

# Calcular a média geométrica
media_geom <- geometric.mean(valores)
print(media_geom) 



### MÉDIA HARMÔNICA
# Pela fórmula
velocidades <- c(10, 20, 30)
media_harmonica <- length(velocidades) / sum(1 / velocidades)
print(media_harmonica)


# Pelo pacote psych
media_harmonica <- harmonic.mean(velocidades)
print(media_harmonica)  





### QUARTIS
idades <- c(20, 22, 25, 28, 30, 32, 35, 40, 45, 50)
quartis <- quantile(idades, probs = c(0.25, 0.5, 0.75))
print(quartis)



### PERCENTIS
percentil_90 <- quantile(idades, probs = 0.90)
print(percentil_90)










################ TEMA 4 – TABELAS DE FREQUÊNCIA E DISTRIBUIÇÃO NORMAL #########

### Tabelas de Frequência
idades <- c(12, 15, 12, 18, 20, 15, 12, 14, 18, 20)  

# Tabela de frequência absoluta  
freq_abs <- table(idades)  # conta as ocorrências de cada valor único

# Tabela de frequência relativa  
freq_rel <- prop.table(freq_abs)*100  # divide cada contagem pelo total de elementos

# Tabela combinada  
tabela_frequencia <- data.frame(
  Idade = names(freq_abs),
  Frequencia_Absoluta = as.vector(freq_abs),  # remove a formatação antiga
  Frequencia_Relativa = round(as.vector(freq_rel), 2)
)

# Exibir tabela  
print(tabela_frequencia)





### Distribuição de Probabilidade Normal
# Gerar 100 valores aleatórios normalmente distribuídos 
# com média 120 e desvio padrão 10
dados_normais <- rnorm(100, mean = 120, sd = 10)  
print(dados_normais)

# Calcular probabilidade dos dados estarem entre 110 e 130
# Pergunta: qual a chance de um equipamento durar entre 110 e 130 meses?
prob <- pnorm(130, mean = 120, sd = 10) - pnorm(110, mean = 120, sd = 10)  
print(prob)  










################ TEMA 5 – VISUALIZAÇÃO DE DADOS ###############################

### Histograma
# Gerar o conjunto de dados normais  
pressao <- rnorm(1000, mean = 120, sd = 10)  

# Histograma  
hist(pressao, 
     main = "Distribuição Normal (Pressão Arterial)",  
     xlab = "Pressão Sistólica (mmHg)", 
     ylab = "Frequência",
     col = "lightblue", 
     border = "black")  


# Adicionando a curva de distribuição normal
x <- seq(min(pressao), max(pressao), length = 100)
y <- dnorm(x, mean = mean(pressao), sd = sd(pressao))

y <- y * diff(hist(pressao, plot = FALSE)$breaks)[1] * length(pressao)  # Ajustando a escala

lines(x, y, col = "red", lwd = 2)





### Boxplots
# Gerando os dados normais
homens <- rnorm(50, mean = 125, sd = 8)
mulheres <- rnorm(50, mean = 115, sd = 7)

# Criando boxplot
boxplot(homens, mulheres,
        names = c("Homens", "Mulheres"),
        main = "Pressão Arterial por Gênero",
        ylab = "Pressão (mmHg)",
        col = c("lightblue", "pink"))





### Gráficos de Dispersão
# Gerando os dados aleatórios
idade <- sample(18:80, 100, replace = TRUE)
pressao <- 100 + 0.5 * idade + rnorm(100, mean = 0, sd = 10)

plot(idade, pressao,
     main = "Relação entre Idade e Pressão Arterial",
     xlab = "Idade (anos)",
     ylab = "Pressão (mmHg)",
     pch = 16,  # Tipo de ponto
     col = "darkblue")





### Gráficos de Barras
# Gerando os dados da amostra
tipos <- c("A", "B", "AB", "O")
frequencia <- c(35, 25, 10, 30)

barplot(frequencia,
        names.arg = tipos,
        main = "Distribuição de Tipos Sanguíneos",
        xlab = "Tipo",
        ylab = "Número de Pacientes",
        col = "lightgreen")





### Gráficos de Setores
# Dados
alergias <- c("Alimentar", "Respiratória", "Cutânea")
frequencia <- c(45, 30, 25)

frequencia_relativa <- round(frequencia / sum(frequencia) * 100, 1)
rotulos <- paste(alergias, "(", frequencia_relativa, "%)", sep = "")

# Gráfico de pizza
pie(frequencia, 
    labels = rotulos,
    col = c("lightcoral", "lightgreen", "lightblue"),
    main = "Distribuição de Tipos de Alergias (%)")






################ Aplicação no dataset cats ####################################
# Instalar apenas uma vez
if(!require(MASS)) install.packages('MASS')

# Carregar o pacote e o dataset
library(MASS)
data(cats) # Contém o sexo, peso corporal e peso do coração de gatos

# A estrutura dos dados e as primeiras linhas
str(cats)
head(cats) 

# As medidas de tendência central
media_hwt <- mean(cats$Hwt)
mediana_hwt <- median(cats$Hwt)
print(paste("Média:", media_hwt))
print(paste("Mediana:", mediana_hwt))

# As medidas de dispersão
variancia_hwt <- var(cats$Hwt)
desvio_padrao_hwt <- sd(cats$Hwt)
amplitude_hwt <- max(cats$Hwt) - min(cats$Hwt)
print(paste("Variância:", variancia_hwt))
print(paste("Desvio Padrão:", desvio_padrao_hwt))
print(paste("Amplitude:", amplitude_hwt))

# Os quartis e percentis
quartis_hwt <- quantile(cats$Hwt, probs = c(0.25, 0.5, 0.75))
print("Quartis do Peso do Coração:")
print(quartis_hwt)


quartis_hwt_90 <- quantile(cats$Hwt, probs = c(0.90))
print("Quantil 90 do Peso do Coração:")
print(quartis_hwt_90)


# Tabelas de Frequência
tabela_sexo <- table(cats$Sex)
print("Frequência Absoluta (Sexo):")
print(tabela_sexo)

tabela_relativa_sexo <- prop.table(tabela_sexo) * 100
print("Frequência Relativa (Sexo %):")
print(tabela_relativa_sexo)

### VISUALIZAÇÃO DOS DADOS
# Histograma (distribuição do peso do coração)
hist(cats$Hwt, 
     main="Distribuição do Peso do Coração (gatos)", 
     xlab="Peso do Coração (g)", 
     col="lightblue")

# Gráfico de Barras (contagem de machos/fêmeas)
barplot(tabela_sexo, 
        main="Número de Gatos por Sexo", 
        ylab="Contagem", 
        col=c("pink", "lightblue"))

# Boxplot (comparando os grupos)
boxplot(Hwt ~ Sex, data=cats, 
        main="Peso do Coração x Sexo",
        ylab="Peso do Coração (g)",
        col=c("pink", "lightblue"))

# Gráfico de Dispersão (relação entre variáveis)
# Pergunta: Gatos mais pesados (Bwt) têm corações mais pesados (Hwt)?
plot(cats$Bwt, cats$Hwt,
     main="Peso Corporal x Peso do Coração",
     xlab="Peso Corporal (kg)",
     ylab="Peso do Coração (g)",
     pch=16)

