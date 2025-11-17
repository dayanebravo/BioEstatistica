################ TEMA 2 - TEOREMA DE BAYES ####################################
# Cenário: Teste para uma doença rara com as seguintes condições
# Sensibilidade (S): 95% | Especificidade (E): 90% | Prevalência (P): 1%

S <- 0.95  # P(Positivo | Doente)
E <- 0.90
P <- 0.01  # P(Doente) - doença rara

# Probabilidade de Teste Positivo (Total)
# P(Positivo) = (Doentes detectados) + (Sadios falsamente detectados)
prob_teste_positivo <- (S * P) + ((1 - E) * (1 - P))

# Valor Preditivo Positivo (VPP) - Teorema de Bayes
# P(Doente | Positivo) -> P(B∣A)=(P(A∣B)⋅P(B))/P(A)
vpp <- (S * P) / prob_teste_positivo

print(paste("A chance real de doença dado um teste positivo é:", round(vpp*100, 2), "%"))
# Observe que a prevalência baixa atrapalha a confiabilidade do teste!




################ TEMA 3 – SENSIBILIDADE E ESPECIFICIDADE ######################
### Avaliação da acurácia de testes diagnósticos
# Pacotes necessários
if(!require(mlbench)) install.packages('mlbench')
if(!require(caret)) install.packages('caret')
if(!require(pROC)) install.packages('pROC')
library(mlbench)
library(caret)
library(pROC)

# Carregar dados
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes
str(df)

# Ajustar um modelo preditivo (logística simples para exemplo)
set.seed(123)
# modelo linear generalizado logístico (binomial)
modelo <- glm(diabetes ~ glucose + mass + age, data = df, family = binomial)

# Para o modelo gerado, qual a probabilidade de acerto
prob <- predict(modelo, type = "response")

# Classificação da probabiliadde obtida usando ponto de corte de 0.5 (Threshold)
corte <- 0.5 
pred <- factor(ifelse(prob > corte, "pos", "neg"), levels = c("neg", "pos"))
real <- df$diabetes # seleciona a coluna com as condições reais


# Matriz de confusão
tabela <- table(Resultado_obtido = pred, Condição_real = real)
print(tabela)

# Extraindo valores
VP <- tabela["pos","pos"]
VN <- tabela["neg","neg"]
FP <- tabela["pos","neg"]
FN <- tabela["neg","pos"]

# Medidas calculadas manualmente
sens <- VP/(VP+FN)
esp  <- VN/(VN+FP)
vpp  <- VP/(VP+FP)
vpn  <- VN/(VN+FN)
acc  <- (VP+VN)/(VP+VN+FP+FN)
f1   <- 2 * (sens * (VP/(VP+FP))) / (sens + (VP/(VP+FP)))

# Exibindo resultados
cat("Sensibilidade:", round(sens,3), "\n")
cat("Especificidade:", round(esp,3), "\n")
cat("VPP:", round(vpp,3), "\n")
cat("VPN:", round(vpn,3), "\n")
cat("Acurácia:", round(acc,3), "\n")
cat("F1-score:", round(f1,3), "\n")

# Curva ROC e AUC
roc_obj <- roc(real, prob, levels = c("neg", "pos"))
plot(roc_obj, col="blue", main="Curva ROC", print.auc=TRUE)


### Pelo pacote CARET podemos obter tudo isso pela seguinte função
resultado_completo <- confusionMatrix(data = pred, reference = real, positive = "pos")
print(resultado_completo)










################ TEMA 4 – FUNÇÕES DENSIDADE E DISTRIBUIÇÃO ####################

### Função densidade de probabilidade (PDF)
# Criando uma sequência de valores x
x <- seq(80, 160, length.out = 100)

# Calculando a densidade normal com média 120 e desvio padrão 10
densidade <- dnorm(x, mean = 120, sd = 10)

# Plotando a curva de densidade
plot(x, densidade, type = "l", col = "blue", lwd = 2,
     main = "Função Densidade (PDF) - Distribuição Normal",
     xlab = "Pressão Sistólica", ylab = "Densidade - Frequência Relativa")





### Função de distribuição acumulada (CDF)
# Criando uma sequência de valores x
x <- seq(80, 160, length.out = 100)

# Calculando a probabilidade normal com média 120 e desvio padrão 10
probabilidade <- pnorm(x, mean = 120, sd = 10)

# Plotando a curva de probabilidade
plot(x, probabilidade, type = "l", col = "darkgreen", lwd = 2,
     main = "Função Probabilidade (CDF) - Distribuição Normal",
     xlab = "Pressão Sistólica", ylab = "Probabilidade")
# Aqui podemos saber qual a chance da pressão ser MENOR que x?
# ou quantos por cento estão abaixo ou acima de x?










################ TEMA 5 – DISTRIBUIÇÕES BINOMIAL E POISSON ####################

### Distribuição Binomial
# Cenário: 10 pacientes, chance de cura 85%. Qual a distribuição das curas?
n <- 10       # número de pacientes
k <- 0:n      # número de acertos desejados (0 até 10)
p <- 0.85     # probabilidade de cura

# Calculando a distriuição binomial do problema
prob <- dbinom(k, size = n, prob = p)

# Visualizando a distribuição
plot(k, prob, type = "h", lwd = 5, col = "darkorange",
     main = "Distribuição Binomial (n=10, p=0.85)",
     xlab = "Número de Pacientes Curados", ylab = "Probabilidade")





### Distribuição de Poisson
# Cenário: Média de 3 atendimentos/hora. Como isso varia?
lambda <- 3
ocorrencias <- 0:10 # Vamos ver a chance de 0 até 10 atendimentos

probs_pois <- dpois(ocorrencias, lambda = lambda)

plot(ocorrencias, probs_pois, type = "h", lwd = 5, col = "purple",
     main = "Distribuição de Poisson (Média = 3)",
     xlab = "Número de Atendimentos na Hora", ylab = "Probabilidade")







### Probabilidade Acumulada e Complementar
# Probabilidade acumulada (<=5)  
prob_acum <- ppois(5, lambda = 3)  
print(prob_acum) 


# Probabilidade complementar (>5)
prob_comp <- 1 - prob_acum
print(prob_comp)





### EXEMPLO PRÁTICO
"Você é um pesquisador em saúde pública analisando a eficácia de um novo 
protocolo de higiene em um hospital. 
Um indicador crítico é o número semanal de infecções hospitalares registradas. 
Estudos anteriores mostram que, antes do novo protocolo, o hospital tinha uma 
média de 6 infecções por semana (distribuição de Poisson). 
Calcule a probabilidade de ocorrerem entre 4 e 8 infecções (faixa controlada)
Para resolvermos esse problema, calculamos o intervalo das probabilidades da 
seguinte forma:
P(4≤X≤8)=P(X≤8)-P(X≤3)
Parando em 3, sem incluir o 4, pois estaríamos eliminando essa probabilidade do 
intervalo. Ou seja, P(X ≥ 4) = 1 - P(X ≤ 3).
"
# Probabilidade acumulada até 8
p8 <- ppois(8, lambda = 6)

# Probabilidade acumulada até 3
p3 <- ppois(3, lambda = 6)

# Probabilidade no intervalo [4, 8]
prob_intervalo <- p8 - p3
print(paste("Probabilidade de 4 a 8 infecções:", round(prob_intervalo*100, 2),"%"))

### VISUALIZAÇÃO DE CONTROLE DE QUALIDADE (POISSON) ###
# Parâmetros do Processo
lambda <- 6  # Média histórica
x <- 0:16    # Vamos visualizar de 0 a 16 casos

# Calcular probabilidades para cada número de casos
probs <- dpois(x, lambda = lambda)

# Definir as cores: Verde para "Normal" (4-8), Vermelho para "Alerta"
# ifelse(teste, valor_se_sim, valor_se_nao)
cores <- ifelse(x >= 4 & x <= 8, "lightgreen", "lightcoral")

# Plotar o Gráfico de Barras
barplot(probs, 
        names.arg = x, 
        col = cores, 
        main = "Controle de Infecções Hospitalares (Média = 6)",
        xlab = "Número de Infecções na Semana",
        ylab = "Probabilidade",
        border = "black",
        ylim = c(0, 0.18)) # Aumenta um pouco o teto do Y para caber a legenda
legend("topright", 
       legend = c("Zona Esperada (69.6%)", "Zona de Alerta"), 
       fill = c("lightgreen", "lightcoral"),
       bty = "n") # bty="n" remove a caixa em volta da legenda

