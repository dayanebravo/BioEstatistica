################ TEMA 1 – AMOSTRAGEM E TAMANHO AMOSTRAL ########################
# Queremos detectar uma redução de 10 mmHg na pressão arterial (Delta).
# Desvio padrão conhecido (sigma) = 15. Poder = 80%. Confiança = 95%.

# MODO 1: Pela Fórmula
alpha <- 0.05
beta <- 0.20
sigma <- 15
delta <- 10

Z_alpha <- qnorm(1 - alpha/2)
Z_beta  <- qnorm(1 - beta)

n_formula <- (2 * (Z_alpha + Z_beta)^2 * sigma^2) / delta^2
cat("Tamanho amostral (Fórmula):", ceiling(n_formula), "por grupo.\n")

# MODO 2: Função Nativa do R 
calculo_r <- power.t.test(delta = delta, sd = sigma, 
                          sig.level = alpha, power = 1-beta, 
                          type = "two.sample")
print(calculo_r)
# Nota: O R usa uma distribuição t não-central (mais precisa que a normal Z),
# por isso pode haver uma ligeira diferença decimal.












################ TEMA 2 – REGRESSÃO LINEAR ####################################

### Regressão linear simples
"vamos prever o peso (lbs) a partir da altura (inches) de mulheres utilizando dados do conjunto “women” nativo do R:
Y=β_0+β_1(altura)+ε"

# 1. Carregar os dados
data(women)
head(women)  # mostra as primeiras linhas para visualização

# 2. Ajuste do modelo de regressão linear simples
modelo_women <- lm(weight ~ height, data = women)

# 4. Extrair coeficientes estimados (betas)
betas <- coef(modelo_women)
cat("Intercepto (β0):", round(betas[1], 3), "\n")
cat("Altura (β1):", round(betas["height"], 3), "\n")

# 5. Escrever o modelo final
cat("\nModelo estimado:\n")
cat("Peso = ", round(betas[1],2), " + ", round(betas["height"],2), "*Altura\n")

# 6. Previsão para um paciente específico
novo_paciente <- data.frame(height = 65)  # altura em inches
prev <- predict(modelo_women, newdata = novo_paciente)
cat("\nPrevisão para paciente (Altura = 65 inches):\n")
cat("Peso =", round(prev, 2), "lbs\n")

# 7. Gráfico de aproximação do modelo com os dados
library(ggplot2)
ggplot(women, aes(x = height, y = weight)) +
  geom_point(color = 'blue', size = 3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'red') +
  labs(title = "Regressão Linear: Peso ~ Altura",
       x = "Altura (inches)",
       y = "Peso (lbs)") +
  theme_minimal()



### Regressão Linear Múltipla
"Vamos prever a Expectativa de Vida (anos) baseada em Murder (Homicídios),
HS_Grad (Educação) e Frost (Clima) utilizando dados nativos 'state.x77':
Y = β0 + β1(Murder) + β2(HS_Grad) + β3(Frost) + ε"

# 1. Carregar e preparar os dados
# state.x77 é uma matriz, convertemos para dataframe para usar nomes de colunas
dados_estados <- as.data.frame(state.x77)
# Renomeando colunas para facilitar (sem espaços)
names(dados_estados)[names(dados_estados) == "Life Exp"] <- "Life_Exp"
names(dados_estados)[names(dados_estados) == "HS Grad"] <- "HS_Grad"

head(dados_estados[, c("Life_Exp", "Murder", "HS_Grad", "Frost")]) # visualização

# 2. Ajuste do modelo de regressão linear múltipla
modelo_estados <- lm(Life_Exp ~ Murder + HS_Grad + Frost, data = dados_estados)

# 4. Extrair coeficientes estimados (betas)
betas <- coef(modelo_estados)
cat("\n--- Coeficientes Estimados ---\n")
cat("Intercepto (β0):", round(betas["(Intercept)"], 3), "\n")
cat("Murder (β1):    ", round(betas["Murder"], 3), "\n")
cat("HS_Grad (β2):   ", round(betas["HS_Grad"], 3), "\n")
cat("Frost (β3):     ", round(betas["Frost"], 3), "\n")

# 5. Escrever o modelo final (Equação)
cat("\nModelo estimado:\n")
cat("Life_Exp =", round(betas[1], 2), 
    "+ (", round(betas["Murder"], 2), "* Murder )",
    "+ (", round(betas["HS_Grad"], 2), "* HS_Grad )",
    "+ (", round(betas["Frost"], 2), "* Frost )\n")

# 6. Previsão para um estado específico (Simulação)
# Vamos criar um estado com alta violência, boa educação e muito frio
novo_estado <- data.frame(Murder = 10, HS_Grad = 55, Frost = 120)

prev <- predict(modelo_estados, newdata = novo_estado)
cat("\nPrevisão para novo estado (Murder=10, HS=55, Frost=120):\n")
cat("Expectativa de Vida =", round(prev, 2), "anos\n")

# 7. Gráfico de aproximação do modelo com os dados
# Obs: Na múltipla, plotamos Real vs Previsto (pois são 4 dimensões)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Adicionando previsão ao dataset original para plotagem
dados_estados$Previsto <- predict(modelo_estados)

ggplot(dados_estados, aes(x = Life_Exp, y = Previsto)) +
  geom_point(color = 'blue', size = 3) +
  # Linha vermelha representa a previsão perfeita (x=y)
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed", size = 1) +
  labs(title = "Regressão Múltipla: Real vs Previsto",
       subtitle = "Quanto mais perto da linha vermelha, melhor o modelo",
       x = "Expectativa de Vida REAL (anos)",
       y = "Expectativa de Vida PREVISTA (anos)") +
  theme_minimal()





################ TEMA 3 – REGRESSÃO LOGÍSTICA #################################
# Simulação de dados - Regressão Logística 
set.seed(123)
n <- 200
idade <- rnorm(n, mean = 60, sd = 12)   # idade média de 60 anos
comorb <- rbinom(n, 1, prob = 0.4)      # 40% com comorbidades

# Modelo artificial para gerar a probabilidade de óbito
logit_p <- -5 + 0.05*idade + 1.2*comorb
p <- 1/(1 + exp(-logit_p))

# Variável resposta: óbito (0 = não, 1 = sim)
obito <- rbinom(n, 1, prob = p)

# Ajuste do modelo logístico
modelo_logistico <- glm(obito ~ idade + comorb, family = binomial)

# Extrair coeficientes estimados
betas <- coef(modelo_logistico)

# Calcular Odds Ratios e IC 95%
odds <- exp(betas)
odds_ci <- exp(confint(modelo_logistico))

# Interpretação automática dos coeficientes
cat("Coeficientes do modelo e interpretação (Odds Ratio):\n")
for(i in 1:length(betas)){
  cat(names(betas)[i], "\n")
  cat("  Beta =", round(betas[i], 3), "\n")
  cat("  Odds Ratio (OR) =", round(odds[i], 3), "\n")
  cat("  IC 95% OR =", paste0("(", round(odds_ci[i,1],3), ", ", round(odds_ci[i,2],3), ")"), "\n")
  if(odds[i] > 1){
    cat("  Interpretação: aumento da chance de óbito\n\n")
  } else if(odds[i] < 1){
    cat("  Interpretação: efeito protetor (redução da chance)\n\n")
  } else {
    cat("  Interpretação: sem efeito\n\n")
  }
}

# Função para previsão automática de probabilidade e classificação
previsao_paciente <- function(modelo, paciente){
  prob <- predict(modelo, newdata = paciente, type = "response")
  cat("Previsão para paciente:\n")
  print(paciente)
  cat("  Probabilidade de óbito =", round(prob, 2), "\n")
  # Classificação automática
  classe <- ifelse(prob >= 0.5, "Alto risco", "Baixo risco")
  cat("  Classificação predita:", classe, "\n\n")
}

# Exemplo: paciente de 60 anos com comorbidade
novo_paciente <- data.frame(idade = 60, comorb = 1)
previsao_paciente(modelo_logistico, novo_paciente)










################ TEMA 5 – ANOVA PARA DOIS FATORES #############################
"Vamos analisar os níveis de colesterol em três grupos de 200 pacientes. 
Utilizaremos a ANOVA de dois fatores para analisar o efeito da dieta e do sexo 
nos níveis de colesterol"

set.seed(123)
sexo <- factor(rep(c("Masculino", "Feminino"), each = 30))
dieta <- factor(rep(c("Dieta1", "Dieta2", "Dieta3"), times = 20))

# Criando médias diferentes para simular efeitos principais e interação
colesterol <- rnorm(60, mean = 200, sd = 10) +
  ifelse(dieta == "Dieta2", 10, ifelse(dieta == "Dieta3", 20, 0)) +
  ifelse(sexo == "Feminino", 5, 0) +
  ifelse(dieta == "Dieta3" & sexo == "Feminino", 10, 0)

dados2 <- data.frame(sexo, dieta, colesterol)

# 1) Modelo ANOVA de dois fatores com interação
modelo2 <- aov(colesterol ~ sexo * dieta, data = dados2)
anova_tab2 <- summary(modelo2)

# 2) Validação dos pressupostos
sh <- shapiro.test(residuals(modelo2))
lev <- car::leveneTest(colesterol ~ sexo * dieta, data = dados2)

# 3) Resultados da ANOVA
print(anova_tab2)

# 4) Medidas de ajuste (R², R² ajustado, RMSE e MAE)
SQT2 <- sum((dados2$colesterol - mean(dados2$colesterol))^2)
SQE2 <- sum(residuals(modelo2)^2)
R2 <- 1 - SQE2/SQT2
n2   <- nrow(dados2)
k2   <- length(coef(modelo2))  # número de parâmetros estimados
R2_adj <- 1 - (SQE2/(n2 - k2)) / (SQT2/(n2 - 1))

# RMSE e MAE
RMSE <- sqrt(mean(residuals(modelo2)^2))
MAE  <- mean(abs(residuals(modelo2)))

# INTERPRETAÇÃO AUTOMÁTICA DAS MÉTRICAS
cat("\n INTERPRETAÇÃO DAS MÉTRICAS DE AJUSTE \n")
cat("- R² =", round(R2, 3), " O modelo explica aproximadamente", round(R2*100, 1), "% da variabilidade dos dados.\n")
cat("- R² Ajustado =", round(R2_adj, 3), " Considerando o número de grupos e tamanho da amostra, o ajuste realista é de", round(R2_adj*100, 1), "%.\n")
cat("- RMSE =", round(RMSE, 2), " O erro quadrático médio é de", round(RMSE, 2), "unidades de colesterol. Valores menores indicam melhor ajuste.\n")
cat("- MAE =", round(MAE, 2), " Em média, o erro absoluto é de", round(MAE, 2), "unidades de colesterol.\n")

# 5) Interpretação automática
cat("\nVALIDAÇÃO DOS PRESSUPOSTOS (p > 0.05) \n")
cat("Normalidade (Shapiro-Wilk): p =", round(sh$p.value, 4), "\n")
cat("Homoscedasticidade (Levene): p =", round(lev[1, "Pr(>F)"], 4), "\n")
cat("Independência: pressuposta pelo delineamento.\n\n")

pvals <- anova_tab2[[1]][["Pr(>F)"]]
names(pvals) <- rownames(anova_tab2[[1]])

cat("INTERPRETAÇÃO FINAL\n")
if (pvals[1] < 0.05) cat("- Há efeito principal de SEXO (p =", round(pvals[1], 4), ")\n")
if (pvals[2] < 0.05) cat("- Há efeito principal de DIETA (p =", round(pvals[2], 4), ")\n")
if (pvals[3] < 0.05) cat("- Há efeito de INTERAÇÃO sexo*dieta (p =", round(pvals[3], 4), ")\n")
if (all(pvals > 0.05)) cat("- Nenhum efeito significativo foi identificado.\n")




