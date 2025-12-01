################ TEMA 1 – O TESTE QUI-QUADRADO DE INDEPENDÊNCIA ###############
### 1. Criando uma Tabela de Contingência Manualmente
# Estudo clínico (Tratamento vs Placebo) e Desfecho (Melhorou vs Não)
dados_clinicos <- matrix(c(60, 40, 30, 70), nrow = 2)
rownames(dados_clinicos) <- c("Tratamento", "Placebo")
colnames(dados_clinicos) <- c("Melhorou", "Não Melhorou")

print("--- Tabela Observada ---")
print(dados_clinicos)

# Teste Qui-Quadrado
# H0: As variáveis são independentes (O tratamento não afeta a melhora)
teste_chi <- chisq.test(dados_clinicos, correct = FALSE) 
# correct = F para dados grandes

# --- INTERPRETAÇÃO AUTOMÁTICA ---
cat("\n--- RESULTADO DO TESTE QUI-QUADRADO ---\n")
cat("Estatística X²:", round(teste_chi$statistic, 3), "\n")
cat("Valor-p:", format.pval(teste_chi$p.value, digits = 4), "\n")

if(teste_chi$p.value < 0.05) {
  cat("[CONCLUSÃO]: Rejeitamos H0. Há evidência estatística de associação significativa.\n")
  cat("Interpretação Clínica: O tratamento influenciou o resultado (Melhora).\n")
} else {
  cat("[CONCLUSÃO]: Não rejeitamos H0.\n")
  cat("Interpretação Clínica: Não há evidência de que o tratamento tenha efeito diferente do placebo.\n")
}


### 2. Teste Exato de Fisher (Para amostras pequenas)
# Teste de Biocompatibilidade de um Novo Polímero para Cateteres.
# Estamos comparando a ocorrência de "Reação Alérgica Severa" (Evento Raro).
# Como o estudo é piloto e caro, temos apenas 10 pacientes em cada grupo.
# - Grupo Novo Polímero (Experimento): 2 reações em 10 pacientes.
# - Grupo Polímero Padrão (Controle): 8 reações em 10 pacientes.

# Matriz de Contingência
dados_cateter <- matrix(c(2, 8, 8, 2), nrow = 2)

# Nomeando para dar contexto
rownames(dados_cateter) <- c("Novo Polímero", "Padrão")
colnames(dados_cateter) <- c("Reação Alérgica", "Sem Reação")

print("--- Tabela de Ocorrências (Estudo Piloto) ---")
print(dados_cateter)

# Executando o Teste de Fisher
# Usamos Fisher porque temos frequências esperadas < 5 (amostra pequena)
teste_fisher <- fisher.test(dados_cateter)

# --- INTERPRETAÇÃO AUTOMÁTICA DETALHADA ---
cat("\n--- RESULTADO DO TESTE EXATO DE FISHER ---\n")
cat("Odds Ratio (OR):", round(teste_fisher$estimate, 3), "\n")
cat("Valor-p:", format.pval(teste_fisher$p.value, digits = 4), "\n")

# Lógica de Decisão
if(teste_fisher$p.value < 0.05) {
  cat("\n[CONCLUSÃO]: Diferença Significativa (p < 0.05).\n")
  
  # Verifica se o OR é menor ou maior que 1 para dizer se protegeu ou piorou
  if(teste_fisher$estimate < 1) {
    cat("Interpretação: O Novo Polímero REDUZIU significativamente o risco de reação alérgica.\n")
    cat("O Odds Ratio < 1 indica um fator de proteção.\n")
  } else {
    cat("Interpretação: O Novo Polímero AUMENTOU o risco (Pior que o padrão).\n")
  }
  
} else {
  cat("\n[CONCLUSÃO]: Não há associação estatisticamente significativa.\n")
  cat("Apesar da diferença numérica, a amostra é muito pequena para confirmar eficácia.\n")
}


### 3. Aplicação com Dataset Real: 'Titanic'
# A Sobrevivência (Survived) depende da Classe (Class)?
data(Titanic)
# Convertendo a tabela 4D em uma tabela 2D (Classe x Sobrevivência)
tabela_titanic <- margin.table(Titanic, c(1, 4))
print(tabela_titanic)

cat("--- Análise do Titanic ---\n")
teste_titanic <- chisq.test(tabela_titanic)

# --- INTERPRETAÇÃO AUTOMÁTICA ---
cat("Valor-p:", format.pval(teste_titanic$p.value, digits = 4), "\n")

if(teste_titanic$p.value < 0.05) {
  cat("A classe social do passageiro influenciou a chance de sobrevivência.\n")
} else {
  cat("[OK]: Independência verificada. A classe não influenciou a sobrevivência.\n")
}


################ TEMA 3 – CURVAS DE KAPLAN-MEIER E TESTE LOG-RANK ##############

# Carregar pacotes (Instala se necessário)
if(!require(survival)) install.packages('survival')
if(!require(survminer)) install.packages('survminer')
library(survival)
library(survminer)

### Usando o dataset 'lung' (Câncer de Pulmão - NCCTG)
data(lung)
str(lung)

# Passo 1: Ajustar o modelo Kaplan-Meier (Sobrevivência por Sexo)
fit_km <- survfit(Surv(time, status) ~ sex, data = lung)

# Extraindo as medianas para interpretar
resumo_km <- summary(fit_km)$table
mediana_homens <- resumo_km[1, "median"] # Homens (1)
mediana_mulheres <- resumo_km[2, "median"] # Mulheres (2)

cat("\n--- ESTATÍSTICAS DESCRITIVAS (KAPLAN-MEIER) ---\n")
cat("Mediana de Sobrevivência (Homens):", mediana_homens, "dias\n")
cat("Mediana de Sobrevivência (Mulheres):", mediana_mulheres, "dias\n")
cat("Diferença observada:", abs(mediana_homens - mediana_mulheres), "dias\n")


# Passo 2: Teste Log-Rank (Validar estatisticamente a diferença)
teste_logrank <- survdiff(Surv(time, status) ~ sex, data = lung)

# Cálculo manual do p-valor (o objeto survdiff não guarda o p-valor direto)
p_val_logrank <- 1 - pchisq(teste_logrank$chisq, df = 1)

# --- INTERPRETAÇÃO AUTOMÁTICA ---
cat("\n--- RESULTADO DO TESTE LOG-RANK ---\n")
cat("Estatística Chi-Square:", round(teste_logrank$chisq, 2), "\n")
cat("Valor-p:", format.pval(p_val_logrank, digits = 4), "\n")

if(p_val_logrank < 0.05) {
  cat("[CONCLUSÃO]: Curvas de sobrevivência DIFERENTES (p < 0.05).\n")
  cat("Interpretação: O sexo do paciente influencia significativamente o tempo de sobrevivência.\n")
  if(mediana_homens < mediana_mulheres){
    cat("Nota: As mulheres (Sex=2) apresentaram maior tempo mediano de vida.\n")
  }
} else {
  cat("[CONCLUSÃO]: Curvas de sobrevivência IGUAIS (p >= 0.05).\n")
  cat("Interpretação: Não há evidência de diferença na sobrevivência entre os sexos.\n")
}

# Passo 3: Plotagem (Apenas gera o gráfico)
ggsurvplot(
  fit_km,
  data = lung,
  pval = TRUE,             
  conf.int = TRUE,         
  risk.table = TRUE,       
  legend.labs = c("Homens", "Mulheres"),
  xlab = "Tempo (dias)",
  ylab = "Probabilidade de Sobrevivência",
  title = "Sobrevivência em Câncer de Pulmão (NCCTG)",
  ggtheme = theme_light(),
  palette = c("#E7B800", "#2E9FDF") 
)


################ TEMA 5 – VISUALIZAÇÃO DE DADOS NO R PELO TIDYVERSE ###########

if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
data(iris)

cat("\n--- GERANDO VISUALIZAÇÕES GGPLOT2 ---\n")

### 1. Gráfico de Dispersão com Camadas
cat("Gráfico 1: Dispersão (Scatter Plot) - Relação Sépala Comprimento vs Largura\n")
ggplot(data = iris, 
       aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point(size = 3, alpha = 0.6) +       
  geom_smooth(method = "lm", se = FALSE) +  
  labs(
    title = "Morfologia das Sépalas por Espécie",
    subtitle = "Análise de regressão linear por grupo",
    x = "Comprimento (cm)",
    y = "Largura (cm)"
  ) +
  theme_minimal()


### 2. O Poder do 'Facet' para painéis separados
cat("Gráfico 2: Facets - Distribuição de Densidade separada por Espécie\n")
ggplot(iris, aes(x = Petal.Length, fill = Species)) +
  geom_density(alpha = 0.7) +
  facet_wrap(~Species) +  
  labs(title = "Distribuição do Comprimento da Pétala",
       y = "Densidade") +
  theme_bw() +
  theme(legend.position = "none")

