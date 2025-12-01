################ TEMA 1 – INTERVALOS DE CONFIANÇA (IC) ########################

### IC para a Média (t-Student)
# Vamos utilizar o dataset 'sleep' que mostra o efeito de 2 drogas no sono 
# de 10 pacientes.
data(sleep)
str(sleep)
# Vamos pegar apenas a variação do tempo (extra) de sono do Grupo 1
grupo1 <- sleep$extra[sleep$group == 1]

# O R calcula o IC automaticamente na função t.test
resultado <- t.test(grupo1, conf.level = 0.95)

print(paste("Média Amostral:", mean(grupo1)))
print("Intervalo de Confiança (95%):")
print(resultado$conf.int)
# Interpretação: Temos 95% de confiança que o verdadeiro aumento do sono
# na população está dentro desse intervalo.


### IC para Proporção (Normal/Z)
# Dataset 'esoph' (Câncer de Esôfago que relaciona alcoolismo e tabagismo).
# Vamos estimar a proporção de casos de câncer (ncases) em relação ao total de 
# pacientes observados (ncases + ncontrols) neste estudo.

data(esoph)
str(esoph)
# Total de casos de câncer no estudo
total_casos <- sum(esoph$ncases)

# Total de pacientes (Casos + Controles)
total_pacientes <- sum(esoph$ncases + esoph$ncontrols)

# Calculando a proporção amostral
prop_cancer <- total_casos / total_pacientes

# Teste de Proporção (gera o IC automaticamente)
teste_prop <- prop.test(total_casos, total_pacientes, conf.level = 0.95)

print(paste("Total de Pacientes:", total_pacientes))
print(paste("Total de Casos:", total_casos))
print(paste("Proporção Amostral:", round(prop_cancer, 4)))
print("Intervalo de Confiança (95%) para a proporção de casos:")
print(teste_prop$conf.int)

# Interpretação: 
# Temos 95% de confiança que a verdadeira proporção de casos neste 
# recorte populacional está dentro desse intervalo.


################ TEMA 3 – TESTES PARAMÉTRICOS #################################

### Teste t para uma amostra
# Dataset: 'trees' (Dados sobre cerejeiras negras)
# Variável: Height (Altura em pés)
# A literatura diz que a altura média histórica dessa espécie é 70 pés.
# Queremos saber se nossa amostra (dataset 'trees') difere dessa média histórica.

data(trees)
str(trees)

# Dados
amostra_alturas <- trees$Height
media_amostra <- mean(amostra_alturas)
referencia <- 70 # Valor hipotético de referência

# Teste
resultado <- t.test(amostra_alturas, mu = referencia)

# Resultado
cat("Média da amostra:", round(media_amostra, 2), "\n")
cat("Média de referência (H0):", referencia, "\n")
cat("Estatística t:", round(resultado$statistic, 4), "\n")
cat("Valor-p:", format.pval(resultado$p.value, digits=4), "\n")

if(resultado$p.value < 0.05) {
  cat("Conclusão: Rejeitamos H0 (p < 0.05). A média de altura das árvores difere significativamente de 70 pés.\n")
} else {
  cat("Conclusão: Não rejeitamos H0. Não há evidência de diferença significativa.\n")
}





### Teste t para duas amostras independentes com variâncias iguais
# Dataset: 'ToothGrowth' (Crescimento de dentes em porquinhos-da-índia)
# Comparação: Comprimento do dente (len) por Tipo de Suplemento (supp: OJ vs VC)
# Vamos verificar se há diferença no crescimento do dente dependendo do suplemento.

data(ToothGrowth)
str(ToothGrowth)

# Separando os grupos
grupo_OJ <- subset(ToothGrowth, supp == "OJ")$len
grupo_VC <- subset(ToothGrowth, supp == "VC")$len

cat("Média Suco de Laranja (OJ):", mean(grupo_OJ), "\n")
cat("Média Vitamina C (VC):", mean(grupo_VC), "\n")

# Teste de variâncias (F-test)
var_teste <- var.test(grupo_OJ, grupo_VC)

# Lógica de decisão do teste t
if(var_teste$p.value > 0.05) {
  # Variâncias iguais (Homocedasticidade)
  resultado <- t.test(grupo_OJ, grupo_VC, var.equal = TRUE)
  cat("Variâncias: Homogêneas (p =", round(var_teste$p.value, 4), ") -> Usando t-test padrão\n")
} else {
  # Variâncias diferentes
  resultado <- t.test(grupo_OJ, grupo_VC, var.equal = FALSE)
  cat("Variâncias: Heterogêneas (p =", round(var_teste$p.value, 4), ") -> Usando Welch\n")
}

# Resultado
cat("Diferença de médias:", round(resultado$estimate[1] - resultado$estimate[2], 2), "\n")
cat("IC 95% da diferença:", round(resultado$conf.int[1], 2), "a", round(resultado$conf.int[2], 2), "\n")
cat("Valor-p:", format.pval(resultado$p.value, digits=4), "\n")
# Pelo p-valor -> Não há diferença entre os suplementos




### Teste t para duas amostras independentes com variâncias diferentes (Welch)
# Dataset: 'mtcars' (Dados de carros de 1974)
# Comparação: Consumo (mpg) entre carros Automáticos (am=0) e Manuais (am=1)
# Carros manuais geralmente são mais econômicos, ou seja, tem mpg maior
# mas a variância é a mesma?

data(mtcars)
str(mtcars)
# Separando os grupos
automaticos <- mtcars$mpg[mtcars$am == 0]
manuais <- mtcars$mpg[mtcars$am == 1]

# Teste de Levene (usando pacote car)
# Nota: Variância de MPG em manuais costuma ser maior que em automáticos
if(!require(car)) install.packages("car")
library(car)

levene_result <- leveneTest(mpg ~ as.factor(am), data = mtcars)
p_var <- levene_result$`Pr(>F)`[1]

if(p_var < 0.05) {
  cat("Teste de Variância com p =", round(p_var, 4), "- Variâncias Diferentes.\n")
  cat("Conclusão: Usar teste t de Welch.\n")
} else {
  cat("Teste de Variância com p =", round(p_var, 4), "- Variâncias Iguais.\n")
}

# Teste t de Welch (padrão do R quando não especificamos var.equal = TRUE)
resultado <- t.test(automaticos, manuais)

# Resultado
cat("Média Automáticos:", round(mean(automaticos), 2), "| Média Manuais:", round(mean(manuais), 2), "\n")
cat("Diferença estimada:", round(diff(resultado$estimate), 2), "mpg\n")
cat("Valor-p:", format.pval(resultado$p.value, digits=4), "\n")
cat("Graus de liberdade (Welch):", round(resultado$parameter, 1), "\n")
# De fato a diferença existe entre o consumo




### Teste t para amostras pareadas
# Dataset: 'sleep' (Efeito de drogas soporíferas)
# Comparação: Horas extras de sono com Droga 1 vs Droga 2
# O MESMO paciente (ID) tomou a droga 1 e depois a droga 2.
# Isso cria dependência entre as observações (pareamento).

data(sleep)
str(sleep)
# Organizando os dados para formato pareado 
droga1 <- sleep$extra[sleep$group == 1]
droga2 <- sleep$extra[sleep$group == 2]
# Nota: No dataset sleep, as linhas já estão ordenadas por ID do paciente (1 a 10)

cat("Observações (primeiros 5 pacientes):\n")
cat("Droga 1:", head(droga1, 5), "...\n")
cat("Droga 2:", head(droga2, 5), "...\n")

# Teste Pareado
resultado <- t.test(droga1, droga2, paired = TRUE)

# Resultado
cat("Média das diferenças (Droga 1 - Droga 2):", round(resultado$estimate, 2), "horas\n")
cat("IC 95%:", round(resultado$conf.int[1], 2), "a", round(resultado$conf.int[2], 2), "\n")
cat("Valor-p:", format.pval(resultado$p.value, digits=4), "\n")

if(resultado$p.value < 0.05) {
  cat("Conclusão: Há diferença significativa no efeito das drogas no mesmo paciente.")
} else {
  cat("Conclusão: Não há evidência de que uma droga funcione melhor que a outra.")
}











################ TEMA 5 – TAMANHO DO EFEITO ###################################
### Cohen’s d (Diferença padronizada entre médias)
# Dataset: 'mtcars'
# Já sabemos (do teste t) que carros manuais são mais econômicos que automáticos.
# Mas qual o TAMANHO desse efeito? É uma diferença trivial ou importante?

if(!require(effsize)) install.packages('effsize')
library(effsize)

data(mtcars)

# Separando os grupos
automaticos <- mtcars$mpg[mtcars$am == 0]
manuais     <- mtcars$mpg[mtcars$am == 1]

# Cálculo do d de Cohen
resultado <- cohen.d(automaticos, manuais, pooled = TRUE, hedges.correction = FALSE)

# Extrair apenas o valor de d (em módulo para facilitar interpretação da magnitude)
d <- abs(as.numeric(resultado$estimate))

cat("Média Automáticos:", round(mean(automaticos), 2), "\n")
cat("Média Manuais:    ", round(mean(manuais), 2), "\n")
cat("Cohen's d:", round(d, 3), "\n")

# Conclusão baseada nos limites de referência
if(d < 0.2) {
  cat("Conclusão: Efeito pequeno (d < 0.2).\n")
} else if(d < 0.5) {
  cat("Conclusão: Efeito médio (0.2 < d < 0.5).\n")
} else if(d < 0.8) {
  cat("Conclusão: Efeito grande (0.5 < d < 0.8).\n")
} else {
  cat("Conclusão: Efeito MUITO grande (d > 0.8).\n")
}




### Razão de Chances (Odds Ratio, OR)
# Dataset: 'Titanic'
# Qual a chance (Odds) de sobreviver sendo do sexo Feminino comparado ao Masculino?
# Vamos transformar o array 4D do Titanic em uma tabela 2x2 simples (Sexo x Sobrevivência).

data(Titanic)
head(Titanic)
# Agregando os dados para ignorar Classe e Idade, focando apenas em Sexo vs Survived
tabela_titanic <- apply(Titanic, c(2, 4), sum)
print(tabela_titanic)

# A tabela original vem com ordem alfabética ou padrão. Vamos garantir a ordem visual:
# Linhas: Male, Female | Colunas: No (Morreu), Yes (Sobreviveu)
# Para calcular o OR de "Ser Mulher" na sobrevivência, vamos reordenar:
# Linha 1: Female (Exposto), Linha 2: Male (Controle)
# Coluna 1: Yes (Evento), Coluna 2: No (Não Evento)
tabela_arrumada <- tabela_titanic[c("Female", "Male"), c("Yes", "No")]
print(tabela_arrumada)

# Cálculo do OR (adbc)
# a = Mulheres que sobreviveram
# b = Mulheres que morreram
# c = Homens que sobreviveram
# d = Homens que morreram
a <- tabela_arrumada[1,1] 
b <- tabela_arrumada[1,2]
c <- tabela_arrumada[2,1]
d <- tabela_arrumada[2,2]

OR <- (a*d) / (b*c)

cat("\nOdds Ratio:", round(OR, 2), "\n")

if(OR > 1) {
  cat("Conclusão: Ser do sexo feminino AUMENTA as chances de sobrevivência em", round(OR, 2), "vezes comparado aos homens.\n")
} else if(OR < 1) {
  cat("Conclusão: Ser do sexo feminino REDUZ as chances de sobrevivência.\n")
} else {
  cat("Conclusão: O sexo não altera as chances de sobrevivência (OR = 1).\n")
}




### Risco Relativo (RR)
# Dataset: 'Titanic' (Recorte por Classe)
# Diferente do Odds (chance), o Risco lida com probabilidades totais.
# Vamos avaliar o Risco de NÃO Sobreviver (Morrer) na 3ª Classe comparado à 1ª Classe.
# "Exposto": Passageiro da 3ª Classe
# "Controle": Passageiro da 1ª Classe


# Agregando por Classe (1) e Sobrevivência (4)
tabela_classe <- apply(Titanic, c(1, 4), sum)
print(tabela_classe)

# Extraindo dados específicos
# 1ª Classe
total_1a <- sum(tabela_classe["1st", ])
mortos_1a <- tabela_classe["1st", "No"]

# 3ª Classe
total_3a <- sum(tabela_classe["3rd", ])
mortos_3a <- tabela_classe["3rd", "No"]

# Calculando as Probabilidades (Riscos) Absolutos
risco_3a_classe <- mortos_3a / total_3a
risco_1a_classe <- mortos_1a / total_1a

# Cálculo do RR
RR <- risco_3a_classe / risco_1a_classe

cat("Risco de morte na 3ª Classe:", round(risco_3a_classe * 100, 1), "%\n")
cat("Risco de morte na 1ª Classe:", round(risco_1a_classe * 100, 1), "%\n")
cat("Risco Relativo (RR):", round(RR, 2), "\n")

if(RR > 1) {
  cat("Conclusão: Estar na 3ª classe aumenta o risco de morte em", round(RR, 2), "vezes em relação à 1ª classe.\n")
} else if(RR < 1) {
  cat("Conclusão: Estar na 3ª classe reduz o risco de morte.\n")
} else {
  cat("Conclusão: O risco é igual entre as classes.\n")
}
