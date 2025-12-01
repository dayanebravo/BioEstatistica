################ TEMA 1 – AMOSTRAGEM E TAMANHO AMOSTRAL ########################
# Queremos detectar uma redução de 10 mmHg na pressão arterial (Delta).
# Desvio padrão conhecido (sigma) = 15. Poder = 80%. Confiança = 95%.

# MODO 1: Pela Fórmula
alpha <- 0.05 # 1-confiança
beta <- 0.20 # 1-poder
sigma <- 15
delta <- 10

Z_alpha <- qnorm(1 - alpha/2)
Z_beta  <- qnorm(1 - beta)

n_formula <- (2 * (Z_alpha + Z_beta)^2 * sigma^2) / delta^2
cat("Tamanho amostral (Fórmula):", ceiling(n_formula), "por grupo.\n")
# o ceiling arredonda sempre para cima

# MODO 2: Função Nativa do R 
calculo_r <- power.t.test(delta = delta, sd = sigma, 
                          sig.level = alpha, power = 1-beta, 
                          type = "two.sample")
print(calculo_r)
# Atenção: O R usa uma distribuição t não-central (mais precisa que a normal Z),
# por isso pode haver uma diferença decimal.












################ TEMA 2 – REGRESSÃO LINEAR ####################################

### Regressão linear simples
"vamos prever o peso (lbs) a partir da altura (inches) de mulheres utilizando 
dados do conjunto “women” nativo do R:
Y=β_0+β_1(altura)+ε"

# 1. Carregar os dados
data(women)
str(women)
head(women)  # mostra as primeiras linhas para visualização

# 2. Ajuste do modelo de regressão linear simples
modelo_women <- lm(weight ~ height, data = women)

# 4. Extrair coeficientes estimados (betas)
betas <- coef(modelo_women)
print(betas)

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
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = "Regressão Linear: Peso ~ Altura",
       x = "Altura (inches)",
       y = "Peso (lbs)") +
  theme_minimal()



### Regressão Linear Múltipla
"Vamos prever a Life Exp (Expectativa de Vida) baseada em Murder (Homicídios),
HS Grad (Educação) e Frost (Clima) utilizando dados nativos 'state.x77':
Y = β0 + β1(Homicidios) + β2(Educacao) + β3(Clima) + ε"

# 1. Carregar e preparar os dados
# state.x77 é uma matriz, convertemos para dataframe para usar nomes de colunas
dados_estados <- as.data.frame(state.x77)

# Renomeando colunas para facilitar (sem espaços)
names(dados_estados)[names(dados_estados) == "Life Exp"] <- "Expectativa_Vida"
names(dados_estados)[names(dados_estados) == "Murder"] <- "Homicidios"
names(dados_estados)[names(dados_estados) == "HS Grad"] <- "Educacao"
names(dados_estados)[names(dados_estados) == "Frost"] <- "Clima"

str(dados_estados)
head(dados_estados[, c("Expectativa_Vida", "Homicidios", "Educacao", "Clima")]) # visualização

# 2. Ajuste do modelo de regressão linear múltipla
modelo_estados <- lm(Expectativa_Vida ~ Homicidios + Educacao + Clima, data = dados_estados)

# 4. Extrair coeficientes estimados (betas)
betas <- coef(modelo_estados)
cat("\n--- Coeficientes Estimados ---\n")
cat("Intercepto (β0):", round(betas["(Intercept)"], 3), "\n")
cat("Homicidios (β1):", round(betas["Homicidios"], 3), "\n")
cat("Educacao (β2):", round(betas["Educacao"], 3), "\n")
cat("Clima (β3):", round(betas["Clima"], 3), "\n")

# 5. Escrever o modelo final (Equação)
cat("\nModelo estimado:\n")
cat("Expectativa_Vida =", round(betas[1], 2), 
    "+ (", round(betas["Homicidios"], 2), "* Homicidios )",
    "+ (", round(betas["Educacao"], 2), "* Educacao )",
    "+ (", round(betas["Clima"], 2), "* Clima )\n")

# 6. Previsão para um estado específico (Simulação)
# Vamos criar um estado com alta violência, boa educação e muito frio
novo_estado <- data.frame(Homicidios = 10, Educacao = 55, Clima = 120)

prev <- predict(modelo_estados, newdata = novo_estado)
cat("\nPrevisão para novo estado (Homicidios=10, Educacao=55, Clima=120):\n")
cat("Expectativa de Vida =", round(prev, 2), "anos\n")

# 7. Gráfico de aproximação do modelo com os dados
# Obs: Na múltipla, plotamos Real vs Previsto (pois são 4 dimensões)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Adicionando previsão ao dataset original para plotagem
dados_estados$Previsto <- predict(modelo_estados)

ggplot(dados_estados, aes(x = Expectativa_Vida, y = Previsto)) +
  geom_point(color = 'blue', size = 3) +
  # Linha vermelha representa a previsão perfeita (x=y)
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = "dashed", size = 1) +
  labs(title = "Regressão Múltipla: Real vs Previsto",
       subtitle = "Quanto mais perto da linha vermelha, melhor o modelo",
       x = "Expectativa de Vida REAL (anos)",
       y = "Expectativa de Vida PREVISTA (anos)") +
  theme_minimal()





################ TEMA 3 – REGRESSÃO LOGÍSTICA #################################
# vamos prever se um carro é MANUAL (am=1) ou AUTOMÁTICO (am=0)
# Preditores selecionados do Dataset: 'mtcars': 
#   mpg (Milhas por galão - Consumo)
#   hp  (Horsepower - Potência)

# 1. Carregar dados
data(mtcars)
# Visualizar a distribuição da variável resposta
table(mtcars$am) # 0 = Auto, 1 = Manual

# 2. Ajuste do modelo logístico
# family = binomial é o que define a regressão logística
modelo_logistico <- glm(am ~ mpg + hp, data = mtcars, family = binomial)

# 3. Extrair coeficientes estimados (Log-Odds)
betas <- coef(modelo_logistico)

# 4. Calcular Odds Ratios (OR) e Intervalos de Confiança (IC 95%)
# Exponenciamos os coeficientes para transformar Log-Odds em Odds Ratio
odds <- exp(betas)
print(odds)


# 5. Interpretação automática dos coeficientes
cat("\n--- Coeficientes do modelo e interpretação (Odds Ratio) ---\n")
cat("Resposta: Probabilidade de ser MANUAL (am=1)\n\n")

for(i in 1:length(betas)){
  nome_var <- names(betas)[i]
  
  cat(nome_var, "\n")
  cat("  Beta (Log-Odds) =", round(betas[i], 3), "\n")
  cat("  Odds Ratio (OR) =", round(odds[i], 3), "\n")
  
  # Tratamento de erro caso o IC seja muito amplo (comum em datasets pequenos)
  ic_inf <- round(odds_ci[i,1], 3)
  ic_sup <- round(odds_ci[i,2], 3)
  cat("  IC 95% OR =", paste0("(", ic_inf, ", ", ic_sup, ")"), "\n")
  
  if(nome_var == "(Intercept)"){
    cat("  (Intercepto não tem interpretação direta de efeito)\n\n")
  } else if(odds[i] > 1){
    cat("  Interpretação: Aumenta a chance de ser Manual.\n")
    cat("  (Para cada unidade extra, a chance multiplica por", round(odds[i], 2), ")\n\n")
  } else if(odds[i] < 1){
    cat("  Interpretação: Reduz a chance de ser Manual (Favorece Automático).\n")
    cat("  (Para cada unidade extra, a chance multiplica por", round(odds[i], 2), ")\n\n")
  } else {
    cat("  Interpretação: Sem efeito na escolha do câmbio.\n\n")
  }
}

# 6. Função para previsão automática de probabilidade e classificação
previsao_carro <- function(modelo, carro_novo){
  # type="response" retorna a probabilidade (0 a 1) em vez do logit
  prob <- predict(modelo, newdata = carro_novo, type = "response")
  
  cat("--- Previsão para Novo Carro ---\n")
  print(carro_novo)
  cat("  Probabilidade de ser Manual =", round(prob * 100, 2), "%\n")
  
  # Classificação (Cutoff padrão de 0.5)
  classe <- ifelse(prob >= 0.5, "Câmbio Manual", "Câmbio Automático")
  cat("  Classificação Predita:", classe, "\n\n")
}

# 7. Exemplos de Previsão
# Exemplo A: Carro muito econômico (30 mpg) e potência baixa (70 hp) -> Típico popular manual
carro_economico <- data.frame(mpg = 30, hp = 70)
previsao_carro(modelo_logistico, carro_economico)

# Exemplo B: Carro com alto consumo (10 mpg) e muito potente (250 hp) -> Típico V8 automático
carro_potente <- data.frame(mpg = 10, hp = 250)
previsao_carro(modelo_logistico, carro_potente)










################ TEMA 5 – ANOVA PARA DOIS FATORES #############################
# Efeito de suplementos no crescimento de dentes (len) do Dataset 'ToothGrowth'
# Fator 1: Suplemento (supp) -> OJ (Suco de Laranja) x VC (Ácido Ascórbico)
# Fator 2: Dose (dose) -> 0.5, 1.0, 2.0 mg/dia
# Analisar se o tipo de suplemento e a dose (e a interação entre eles)
# afetam o comprimento do dente.

# Preparação dos Dados
data(ToothGrowth)
dados <- ToothGrowth
str(dados)

# IMPORTANTE: A dose vem como numérica, precisamos converter para Fator (categoria)
# para que a ANOVA entenda como grupos distintos, não como regressão linear.
dados$dose <- as.factor(dados$dose)

cat("Estrutura dos dados:\n")
str(dados)

# 1) Modelo ANOVA de dois fatores com interação (*)
# Y ~ Fator1 * Fator2
modelo_anova <- aov(len ~ supp * dose, data = dados)
anova_tab <- summary(modelo_anova)


# 2) Resultados da ANOVA
cat("\n--- Tabela ANOVA ---\n")
print(anova_tab)


# 3) Preparação para Validação dos pressupostos
# Normalidade dos resíduos
sh <- shapiro.test(residuals(modelo_anova))

# Homogeneidade das variâncias (Levene)
if(!require(car)) install.packages("car")
library(car)
lev <- leveneTest(len ~ supp * dose, data = dados)


# 4) Preparação para Medidas de ajuste (R², R² ajustado, RMSE e MAE)
# Cálculo manual das métricas de performance do modelo
SQT <- sum((dados$len - mean(dados$len))^2)   # Soma Quadrados Total
SQE <- sum(residuals(modelo_anova)^2)         # Soma Quadrados Erro (Resíduos)

# R² (Coeficiente de Determinação)
R2 <- 1 - SQE/SQT

# R² Ajustado
n <- nrow(dados)
k <- length(coef(modelo_anova)) # número de coeficientes estimados
R2_adj <- 1 - (SQE/(n - k)) / (SQT/(n - 1))

# RMSE (Raiz do Erro Quadrático Médio) e MAE (Erro Absoluto Médio)
RMSE <- sqrt(mean(residuals(modelo_anova)^2))
MAE  <- mean(abs(residuals(modelo_anova)))


# 5) Interpretação automática dos Testes e Pressupostos e Métricas
cat("\n--- Validação dos Pressupostos (p > 0.05 idealmente) ---\n")

# Shapiro
cat("Normalidade (Shapiro-Wilk): p =", round(sh$p.value, 4))
if(sh$p.value > 0.05) {
  cat(" [OK - Resíduos Normais]\n") 
} else {
  cat(" [Alerta - Resíduos não normais]\n")
}

# Levene
p_levene <- lev[1, "Pr(>F)"]
cat("Homoscedasticidade (Levene): p =", round(p_levene, 4))
if(p_levene > 0.05) {
  cat(" [OK - Variâncias Homogêneas]\n") 
} else {
  cat(" [Alerta - Variâncias Heterogêneas]\n")
}

# MÉTRICAS
cat("\n--- Interpretação das Métricas de Ajuste ---\n")
cat("- R² =", round(R2, 3), ": O modelo explica", round(R2*100, 1), "% da variabilidade no crescimento do dente.\n")
cat("- R² Ajustado =", round(R2_adj, 3), ": Ajuste realista considerando a complexidade do modelo (", round(R2_adj*100, 1), "%).\n")
cat("- RMSE =", round(RMSE, 2), ": O erro médio do modelo é de aprox.", round(RMSE, 2), "unidades de comprimento.\n")


# Interpretação Final da Significância (P-values da ANOVA)
pvals <- anova_tab[[1]][["Pr(>F)"]]
print(pvals)
# Removemos o último NA (que corresponde aos Resíduos na tabela)
pvals <- pvals[!is.na(pvals)]
names(pvals) <- rownames(anova_tab[[1]])[1:3] # supp, dose, supp:dose
print(pvals)

cat("\n--- Interpretação Final dos Efeitos ---\n")
# Efeito da Interação (Geralmente olhamos primeiro)
p_interacao <- pvals[3]
if (p_interacao < 0.05) {
  cat("[IMPORTANTE] Há INTERAÇÃO significativa (p =", round(p_interacao, 4), ").\n")
  cat("Isso significa que o efeito da Dose depende do tipo de Suplemento (e vice-versa).\n")
  cat("Não se deve analisar os efeitos principais isoladamente.\n")
} else {
  cat("Não há interação significativa (p > 0.05). Os efeitos são independentes.\n")
  
  # Se não há interação, olhamos os efeitos PRINCIPAIS
  if (pvals[1] < 0.05) cat("- Há efeito principal de SUPLEMENTO (p =", round(pvals[1], 4), "). Um tipo funciona melhor que o outro.\n")
  if (pvals[2] < 0.05) cat("- Há efeito principal de DOSE (p < 0.0001). Aumentar a dose altera o crescimento.\n")
}

# 6) Visualização Rápida (Boxplot)
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

ggplot(dados, aes(x = dose, y = len, fill = supp)) +
  geom_boxplot() +
  labs(title = "Crescimento dos Dentes: Dose vs Suplemento",
       x = "Dose (mg/dia)", y = "Comprimento do Dente", fill = "Suplemento") +
  theme_minimal()



