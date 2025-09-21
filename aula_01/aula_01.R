################ TEMA 04 - COMANDOS BÁSICOS ###################################

### IMPORTANTE: NO R O INDICE INICIA EM 1 E NÃO EM 0 
# lista de 101 até 126
101:126





### NOMEAR OBJETOS 
# nomes permitidos
nome_01 <- 5
Nome_01 <- 6
nome.01 <- 7

# nomes não permitidos
nome-01 <- 8
01_nome <- 9
_nome_01 <- 10





### SINTAXE BASICA FUNÇÃO 
# sintaxe básica para criar uma função
nome_da_funcao <- function(parametro_01, parametro_02, ...) {
  # corpo da função
  # operações a serem realizadas
  return(resultado)  # valor a ser retornado
}


# criando a função soma
soma <- function(valor_01, valor_02) {
  resultado_soma <- valor_01 + valor_02
  return(resultado_soma)
}


# executando a função soma
resultado_funcao_soma <- soma(4, 2)
print(resultado_funcao_soma)





### FUNÇÃO SOMA - OUTRAS VERSÕES 
# criando a função soma - versão 02
soma_v02 <- function(valor_01, valor_02) {
  return(valor_01 + valor_02)
}

# criando a função soma - versão 03
soma_v03 <- function(valor_01, valor_02) valor_01 + valor_02

# criando a função soma - versão 04
soma_v04 <- function(valor_01, valor_02) {
  resultado_soma <- sum(valor_01, valor_02)
  return(resultado_soma)
}


# executando a função soma versão 02, 03 e 04
resultado_funcao_soma_v02 <- soma_v02(1,2)
print(resultado_funcao_soma_v02)

resultado_funcao_soma_v03 <- soma_v03(2,2)
print(resultado_funcao_soma_v03)

resultado_funcao_soma_v04 <- soma_v04(3,2)
print(resultado_funcao_soma_v04)





### FUNÇÃO RUNINF PRÉ-DEFINIDA 
# Padrão da ordem dos argumentos runinf
runif(n, min = 0, max = 1)

# Gera 10 valores de 1 até 9
runif(10, 1, 9)

# Alterando a ordem dos argumentos máximo e mínimo
runif(n = 5, max = 50, min = 1)

### Quais argumentos a função possui
args(runif)
args(rnorm)





### MECANISMOS DE AJUDA 
?runif
?writeLines
?mean
help(median)

### MECANISMOS DE BUSCA 
# Encontrar termo no nome da função
apropos('model')

# Encontrar termo dentro da documentação
help.search("mean")

# Encontrar o pacote que a função pertence
find('mean')

# Encontrar quais pacotes estão carregados no seu workspace
search()

# Instalar um pacote
install.packages("readxl")

# Carregar um pacote
library(readxl)

# Para evitar de instalar mais de uma vez o mesmo pacote
if(!require(readxl)) install.packages('readxl')






