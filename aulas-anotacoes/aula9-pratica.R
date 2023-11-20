
library(tidyverse)
library(data.table)

# Aula 9 - MLE ------------------------------------------------------------

# Parte prática

# 1. Acessar <https://simonweschle.github.io/psc400.html>
# 2. baixar o .csv da week 3

# Base de dados -----------------------------------------------------------

df <- read_delim("datasets/BES.csv")

# Usando a função fread()
bes <- fread("datasets/BES.csv")

# Dados de um survey feito no Reino Unido antes do Brexit

# Objetivo ----------------------------------------------------------------

# Rodar uma regressão para verificar o efeito da idade sobre o voto

reg <- lm(leave ~ age, data = bes)

summary(reg)

# Interpretando -----------------------------------------------------------

# Intercept (alpha) ----
# Valor que espero de Y quando o preditor é 0
# Não faz sentido que tenhamos pessoas com 0 anos de idade

# variável (beta) ----
# coef. estimado * idade + intercept (alpha) : o resultado diz qual a porcentagem
# esperada de votos favoráveis ao brexit para a idade que passamos.

idade = 80

0.0072082 * idade + 0.1194782 # 27%

# beta

# E[Yi] = E[alpha] + E[beta . xi]
# E[Yi] = alpha + beta . E[xi]
# E[Yi] = alpha + beta . xi

# O E[Y] é a prop. de pessoas que votam a favor do brexit
# xi: se eu aumentar a idade em um ano, o quanto o E[Y] muda?

# Teste de hipótese ----

# H0: beta = 0
# H1: beta != 0

# A ideia é decidir se vamos rejeitar ou não a hipótese nula
# Para sabermos se vamos rejeitar ou não devemos calcular a probabilidade

# Supondo que H0 é verdadeira, qual a probabilidade de ter observado o 
# beta-observado?
# Essa prob. é dada por Pr(>|t|)
# Em geral, quando essa prob. é menor que 0,05, rejeitamos H0

# O nível de significância deve ser definido antes da análise

# Std. Error ----
# Erro padrão do nosso estimador

# Intervalo de confiança ----

# coef. + 2 * std. error
# coef. - 2 * std. error






