################ TEMA 05 - ESTRUTURAS DE DADOS ################################

### Coerção de Tipos - Implícita
# números e caracteres -> converte para caracteres
num_char <- c(1, "a", TRUE)
print(typeof(num_char)) 


# números e valores lógicos -> converte para números
num_logic <- c(1, TRUE, FALSE)
print(typeof(num_logic))  
print(num_logic)


### Coerção de Tipos - Explícita 
# converter de caracteres para numéricos manualmente
vetor_caracteres <- c("1", "2", "3")
print(typeof(vetor_caracteres)) 

vetor_numerico <- as.numeric(vetor_caracteres)
print(typeof(vetor_numerico))  
print(vetor_numerico)





### VALORES ESPECIAIS
x <- c(1, NA, 3, NaN, Inf)
is.na(x)  
is.nan(x) 
is.infinite(x) 





### FATORES 
# Ordem automática -> alfabética
vetor_categorico <- c("baixa", "alta", "baixa", "baixa", "média")
exemplo_fator <- factor(vetor_categorico)
print(exemplo_fator)

# Ordenado manualmente
vetor_niveis = c("baixa", "média", "alta")
fator_ordenado <- factor(exemplo_fator, levels = vetor_niveis)
print(fator_ordenado)





### MATRIZES
# Preenchimento por coluna!!!
matriz_3x4 <- matrix(1:12, nrow = 3, ncol = 4)
print(matriz_3x4)

# Preenchimento por linha
matriz_linha_3x4 <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
print(matriz_linha_3x4)





### INDEXAÇÃO DE MATRIZES
# acessar elementos específicos da matriz usando índices
print(matriz_linha_3x4 [2, 3])


# acessar uma linha inteira
linha_2 = matriz_linha_3x4 [2, ]
print(linha_2)


# acessar uma coluna inteira
coluna_3 = matriz_linha_3x4 [ ,3]
print(coluna_3)


# acessar várias linhas ao mesmo tempo passando um vetor de índices
# deixando o espaço da coluna em branco
vetor_linhas_1_3 = c(1, 3)
print(matriz_linha_3x4 [vetor_linhas_1_3, ])


# acessar várias colunas ao mesmo tempo passando um vetor de índices
# deixando o espaço da linha em branco
vetor_colunas_2_4 = c(2, 4)
print(matriz_linha_3x4 [ ,vetor_colunas_2_4])


# acessar submatrizes especificando intervalos de linhas e colunas
matriz_linha_3x4[2:3, 2:4]


# alterar um determinado elemento da matriz
atribuicao_2_3 = matriz_linha_3x4 [2, 3] <- 35
print(matriz_linha_3x4)





### OPERAÇÕES COM MATRIZES  
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)
print(A)
print(B)


# soma de matrizes
print(A + B)


# subtração de matrizes
print(A - B)


# multiplicação por uma constante
print(A*5)


# multiplicação de matrizes - USUAL
mult_AxB <- A %*% B
print(mult_AxB)


# a multiplicação de matrizes NÃO É COMUTATIVA!
C <- matrix(5:10, nrow = 3)
print(A %*% C) # ERRO
print(C %*% A)


# multiplicação elemento a elemento - NÃO USUAL
# ATENÇÃO para o operador
print(A*B) 	


# calcular a inversa de uma matriz QUADRADA e det(A)\=0
print(A)
print(det(A))
print(solve(A))





### LISTAS
# Criando uma lista com diferentes tipos de dados
vetor_nome = c("Maria", 2) # cada OBJETO deve ser do mesmo tipo
valor_idade = 30
vetor_notas = c(8, 7, 9)

minha_lista <- list(
  nome = vetor_nome, 
  idade = valor_idade,
  notas = vetor_notas
)

print(minha_lista)
# coerção implícita: o 2 é convertido em caractere





### INDECAÇÃO DE LISTAS
# acessar os elementos das listas usando o operador $ 
print(minha_lista$nome)

# ou colchetes duplos [[ ]]
print(minha_lista[[3]]) 


# acessar objetos específicos utilizando o operador $ 
# com o nome do item e sua posição
print(minha_lista$nome[1])
print(minha_lista$notas[2])

# ou utilizando os colchetes duplos 
print(minha_lista[[1]][[1]])
print(minha_lista[[3]][[2]])

# ou informando um vetor com a posição do item 
# junto da posição de seu subitem
print(minha_lista[[c(1, 1)]])
print(minha_lista[[c(3, 2)]])


# Modificando elementos
minha_lista$nome[2]<-"Pedro"
print(minha_lista$nome) # Pedro no lugar de "2"


# listas aninhadas
# colocar dentro de uma lista, outra lista
dados_Maria = list(nome = "Maria", idade = 25)
dados_Pedro = list(nome = "Pedro", idade = 30)

lista_aninhada <- list(
  pessoa1 = dados_Maria,
  pessoa2 = dados_Pedro
)

print(lista_aninhada)





### DATAFRAMES
# Criando um data frame com informações de pessoas
vetor_nome = c("Joao", "Maria", "Pedro")
vetor_idade = c(30, 25, 28)
vetor_salario = c(3000, 3500, 4000)

meu_df <- data.frame(
  nome = vetor_nome,
  idade = vetor_idade,
  salario = vetor_salario
)

print(meu_df)





### INDEXAÇÃO DE DATAFRAMES
# acessar colunas específicas usando o operador $ 
print(meu_df$nome)

# ou colchetes simples
print(meu_df["nome"])


# Acessar a segunda linha (informações da Maria)
print(meu_df[2, ])   


# Acessar a terceira linha, segunda coluna (idade do Pedro)
print(meu_df[3, 2])


# Acessar a terceira linha da coluna nome
print(meu_df[3, "nome"])


# Adicionar a coluna cidade
meu_df$cidade <- c("Sao Paulo", "Curitiba", "Belo Horizonte")
print(meu_df)


# Remover a coluna cidade por meio da função NULL
meu_df$cidade <- NULL
print(meu_df)





### Funções úteis para df
# Mostra as primeiras 6 linhas (ou todas, se houver menos)
print(head(meu_df))


# Mostra as últimas 6 linhas (ou todas, se houver menos)
print(tail(meu_df))


# Quantidade de linhas
print(nrow(meu_df))  


# Quantidade de colunas
print(ncol(meu_df))


# Retorna os nomes das colunas.
print(names(meu_df))


