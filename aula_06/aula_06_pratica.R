################ TEMA 3 – CURVAS DE KAPLAN-MEIER E TESTE LOG-RANK #############
# Carregar os pacotes necessários para a análise e visualização
if(!require(survival)) install.packages('survival')
if(!require(survminer)) install.packages('survminer')
library(survival)
library(survminer)

# Passo 1: Criar os dados do estudo
set.seed(42) # Para reprodutibilidade
n_grupo <- 40
dados_estudo <- data.frame(
  # Tempo em meses
  tempo = c(
    # Grupo Terapia Inovadora (maior tempo de sobrevivência)
    # Weibull shape=2 -> risco crescente/ scale -> eficácia
    rweibull(n_grupo, shape = 2, scale = 18),
    # Grupo Padrão (menor tempo de sobrevivência)
    rweibull(n_grupo, shape = 2, scale = 12)
  ),
  # Grupo de tratamento
  grupo = rep(c("Terapia Inovadora", "Padrão"), each = n_grupo)
)

# Censura: alguns pacientes não terão o evento em 24 meses
dados_estudo$status <- ifelse(dados_estudo$tempo > 24, 0, 1) # 0=Censurado, 1=Evento
dados_estudo$tempo <- ifelse(dados_estudo$tempo > 24, 24, dados_estudo$tempo)

head(dados_estudo)

# Passo 2: Ajustar o modelo de sobrevivência Kaplan-Meier
# A fórmula Surv(tempo, status) ~ grupo significa que queremos estimar a sobrevivência baseada no tempo e status separadamente
fit_km <- survfit(Surv(tempo, status) ~ grupo, data = dados_estudo)

# Passo 3: Visualizar e interpretar o resumo do ajuste
sumario_km <- summary(fit_km)
print(sumario_km$table)
# No output, procure por 'median' para cada grupo

# Passo 4: Criar o gráfico Kaplan-Meier com o pacote survminer 
grafico_km <- ggsurvplot(
  fit_km,
  data = dados_estudo,
  pval = TRUE,                 # Adiciona o p-valor do teste Log-Rank
  conf.int = TRUE,             # Mostra o intervalo de confiança
  risk.table = TRUE,           # Adiciona a tabela de número em risco
  legend.title = "Tratamento",
  legend.labs = c("Padrão", "Terapia Inovadora"),
  xlab = "Tempo em Meses",
  ylab = "Probabilidade de Sobrevivência",
  title = "Curva de Sobrevivência de Kaplan-Meier",
  ggtheme = theme_minimal()    # Aplica um tema visual limpo
)
print(grafico_km)

# Passo 5: Realizar o Teste Log-Rank explicitamente 
teste_logrank <- survdiff(Surv(tempo, status) ~ grupo, data = dados_estudo)
print(teste_logrank)
# p-valor>0,05 - não há diferença de sobrevivência entre os dois tratamentos 










################ TEMA 5 – VISUALIZAÇÃO DE DADOS NO R PELO TIDYVERSE ###########
# Instalando e carregando o pacote ggplot2
if(!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

ggplot(data = iris, 
       aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point(size = 3)  

# Gráfico com linhas de tendência por espécie
ggplot(iris, 
       aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point(size = 3) +  # Pontos coloridos por espécie
  geom_smooth(method = "lm", se = FALSE) +  # Linhas de tendência por espécie
  labs(
    title = "Relação entre Comprimento e Largura das Sépalas",
    x = "Comprimento da Sépala (cm)",
    y = "Largura da Sépala (cm)")





