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

# Ajustar um modelo preditivo (logística simples para exemplo)
set.seed(123)
modelo <- glm(diabetes ~ glucose + mass + age, data = df, family = binomial)

# Probabilidades previstas
prob <- predict(modelo, type = "response")

# Classificação usando ponto de corte de 0.5
pred <- ifelse(prob > 0.5, 1, 0)

# Transformar desfecho real para binário (1=pos, 0=neg)
real <- ifelse(df$diabetes == "pos", 1, 0)

# Matriz de confusão
tabela <- table(Resultado = pred, Condição = real)
print(tabela)

# Extraindo valores
VP <- tabela["1","1"]
FP <- tabela["1","0"]
FN <- tabela["0","1"]
VN <- tabela["0","0"]

# Medidas
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
roc_obj <- roc(real, prob, levels = c(0,1))
plot(roc_obj, col="blue", main="Curva ROC")
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val,3), "\n")










################ TEMA 4 – FUNÇÕES DENSIDADE E DISTRIBUIÇÃO ####################

### Função densidade de probabilidade (PDF)
# Criando uma sequência de valores x
x <- seq(80, 160, length.out = 100)

# Calculando a densidade normal com média 120 e desvio padrão 10
densidade <- dnorm(x, mean = 120, sd = 10)

# Plotando a curva de densidade
plot(x, densidade, type = "l", col = "blue", lwd = 2,
     main = "Função Densidade (PDF) - Distribuição Normal",
     xlab = "Valor da variável", ylab = "Densidade")





### Função de distribuição acumulada (CDF)
# Criando uma sequência de valores x
x <- seq(80, 160, length.out = 100)

# Calculando a probabilidade normal com média 120 e desvio padrão 10
probabilidade <- pnorm(x, mean = 120, sd = 10)

# Plotando a curva de probabilidade
plot(x, probabilidade, type = "l", col = "darkgreen", lwd = 2,
     main = "Função Probabilidade (CDF) - Distribuição Normal",
     xlab = "Valor da variável", ylab = "Probabilidade")










################ TEMA 5 – DISTRIBUIÇÕES BINOMIAL E POISSON ####################

### Distribuição Binomial
# Parâmetros da binomial
n <- 10       # número de pacientes
k <- 8        # número de acertos desejados
p <- 0.85     # probabilidade de sucesso

# Calcular a probabilidade de exatamente 8 acertos
prob <- dbinom(k, size = n, prob = p)
print(prob)





### Distribuição de Poisson
# Probabilidade exata  
prob <- dpois(5, lambda = 3)  
print(prob)  





### Probabilidade Acumulada Discreta
# Probabilidade acumulada (<=5)  
prob_acum <- ppois(5, lambda = 3)  
print(prob_acum) 


# Probabilidade complementar (>5)
prob_comp <- 1 - prob_acum
print(prob_comp)





### EXEMPLO PRÁTICO
"Você é um pesquisador em saúde pública analisando a eficácia de um novo 
protocolo de higiene em um hospital. Um indicador crítico é o número semanal 
de infecções hospitalares registradas. 
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
print(prob_intervalo) 

