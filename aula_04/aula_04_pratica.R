################ TEMA 3 – TESTES PARAMÉTRICOS #################################

### Teste t para uma amostra
"Um laboratório afirma que o nível médio de glicose em jejum na população é 
95 mg/dL. Coletamos uma amostra de 30 pacientes e obtivemos média = 98 mg/dL 
e desvio = 12 mg/dL. Verifique se nossa amostra difere significativamente do 
valor de referência. "

# Dados
n <- 30
media <- 98
desvio <- 12
referencia <- 95

# Teste
resultado <- t.test(rnorm(n, media, desvio), mu = referencia)

# Resultado
cat("Estatística t:", round(resultado$statistic, 4), "\n")
cat("Valor-p:", round(resultado$p.value, 4), "\n")

if(resultado$p.value < 0.05) {
  cat("Conclusão: Rejeitamos H0 (p < 0.05). A média difere significativamente de 95 mg/dL.")
} else {
  cat("Conclusão: Não rejeitamos H0 (p ≥ 0.05). Não há evidência de diferença.")
}





### Teste t para duas amostras independentes com variâncias iguais
"Comparar a eficácia de dois medicamentos para dor (A e B) em grupos independentes. 
Grupo A (n=25, média=6,2 e dp=1,8)
Grupo B (n=25, média=5,6 e dp=1,5)
Verifique se há diferença significativa"

# Dados
grupo_A <- rnorm(25, 6.2, 1.8)
grupo_B <- rnorm(25, 5.6, 1.5)

# Teste de variâncias
var_test <- var.test(grupo_A, grupo_B)

# Teste t (variâncias iguais)
if(var_test$p.value > 0.05) {
  resultado <- t.test(grupo_A, grupo_B, var.equal = TRUE)
  cat("Variâncias iguais (p =", round(var_test$p.value, 4), ")\n")
} else {
  resultado <- t.test(grupo_A, grupo_B)
  cat("Variâncias diferentes (p =", round(var_test$p.value, 4), ")\n")
}

# Resultado
cat("Diferença de médias:", round(resultado$estimate[1]-resultado$estimate[2], 2), "\n")
cat("IC 95%:", round(resultado$conf.int[1], 2), "a", round(resultado$conf.int[2], 2), "\n")
cat("Valor-p:", round(resultado$p.value, 4), "\n")





### Teste t para duas amostras independentes com variâncias diferentes (Welch)
"Comparar tempo de recuperação em horas entre 
  jovens (n=15, média=8,5 e dp=2,1) 
  idosos (n=20, média=12,3 e dp=3,8)
As variâncias são diferentes? "

# Dados
jovens <- rnorm(15, 8.5, 2.1)
idosos <- rnorm(20, 12.3, 3.8)

# Teste de Levene
if(!require(car)) install.packages('car')
library(car)
levene_result  <- leveneTest(c(jovens, idosos), group = rep(c("J","I"), c(15,20)))

if(levene_result$`Pr(>F)`[1] < 0.05) {
  cat("Conclusão: Variâncias diferentes (p < 0.05). Usar teste t de Welch\n\n")
} else {
  cat("Conclusão: Variâncias homogêneas (p ≥ 0.05) - pode usar teste t padrão\n\n")
}

# Teste t de Welch
resultado <- t.test(jovens, idosos)

# Resultado
cat("Diferença:", round(diff(resultado$estimate), 2), "horas\n")
cat("Valor-p:", format.pval(resultado$p.value, digits=4), "\n")
cat("Graus de liberdade (Welch):", round(resultado$parameter, 1), "\n")





### Teste t para amostras pareadas
"Avaliar o efeito de uma dieta em 10 pacientes, medindo peso antes e depois (kg):
Antes: 78, 85, 92, 65, 70, 80, 75, 88, 82, 95
Depois: 75, 82, 88, 63, 68, 78, 72, 85, 79, 92"

# Dados
antes <- c(78,85,92,65,70,80,75,88,82,95)
depois <- c(75,82,88,63,68,78,72,85,79,92)
diferencas <- antes - depois

# Teste
resultado <- t.test(antes, depois, paired = TRUE)

# Resultado
cat("Média das diferenças:", round(resultado$estimate, 2), "kg\n")
cat("IC 95%:", round(resultado$conf.int[1], 2), "a", round(resultado$conf.int[2], 2), "\n")
cat("Valor-p:", format.pval(resultado$p.value, digits=4), "\n")










################ TEMA 5 – TAMANHO DO EFEITO ###################################

### Cohen’s d (diferença padronizada entre médias):
"avaliar o efeito de um novo fármaco na redução da pressão arterial sistólica (PAS)"

if(!require(effsize)) install.packages('effsize')
library(effsize)

set.seed(123)
controle <- rnorm(40, mean = 150, sd = 10)
tratado  <- rnorm(40, mean = 140, sd = 12)

# Cálculo automático do d de Cohen
resultado <- cohen.d(controle, tratado, pooled = TRUE, hedges.correction = FALSE)

# Extrair apenas o valor de d
d <- as.numeric(resultado$estimate)

cat("Cohen's d:", round(d, 3), "\n")

# Conclusão baseada nos limites de interpretação
if(d < 0.2) {
  cat("Conclusão: Efeito pequeno.\n")
} else if(d < 0.5) {
  cat("Conclusão: Efeito médio.\n")
} else if(d < 0.8) {
  cat("Conclusão: Efeito grande.\n")
} else {
  cat("Conclusão: Efeito muito grande.\n")
}





###	Razão de chances (Odds Ratio, OR):
"avaliar se um antibiótico reduz a recorrência de infecção."

tabela <- matrix(c(10, 90, 30, 70), nrow = 2, byrow = TRUE)
colnames(tabela) <- c("Infecção", "Sem_infecção")
rownames(tabela) <- c("Antibiótico", "Placebo")
tabela

# Cálculo do OR
a <- tabela[1,1] 
b <- tabela[1,2]
c <- tabela[2,1]
d <- tabela[2,2]
OR <- (a*d) / (b*c)

cat("Odds Ratio:", OR, "\n")
if(OR < 1) {
  cat("Conclusão: O antibiótico reduz as chances de infecção.\n")
} else if(OR > 1) {
  cat("Conclusão: O antibiótico aumenta as chances de infecção.\n")
} else {
  cat("Conclusão: O antibiótico não altera as chances de infecção.\n")
}





### Risco relativo (RR):
"avaliar se um antibiótico reduz o risco de infecção."

risco_tratado <- 10/100
risco_placebo <- 30/100
RR <- risco_tratado / risco_placebo

cat("Risco Relativo:", RR, "\n")
if(RR < 1) {
  cat("Conclusão: O antibiótico reduz o risco de infecção.\n")
} else if(RR > 1) {
  cat("Conclusão: O antibiótico aumenta o risco de infecção.\n")
} else {
  cat("Conclusão: O antibiótico não altera o risco de infecção.\n")
}
