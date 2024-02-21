# Aula 4 ------------------------------------------------------------------

# Bibliografia: 
# Galdino. Introdução à Simulação (cap. 4); O Modelo de Regressão (cap. 5). In: Introdução à Regressão

# Pacotes utilizados ------------------------------------------------------

library(PNADcIBGE)
library(tidyverse)

# Importando a base de dados ----------------------------------------------

df <- PNADcIBGE::get_pnadc(year=2017,
                           quarter=4,
                           selected=FALSE,
                           vars=c("Ano", "Trimestre", "UF", 
                                  "V2007", "VD4020", "VD4035", "V1028", "V2010"),
                           design=FALSE,
                           savedir=tempdir())

# Selecionando variáveis de interesse
df <- df |> 
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035, V1028, V2010) |> 
  # Renomeando
  rename(sexo = V2007,
         renda = VD4020,
         horas_trabalhadas = VD4035,
         raca = V2010)

# Capítulo 5 --------------------------------------------------------------

# Quando falamos de regressão, falamos de esperança condicional

# Esperança condiconal ----------------------------------------------------

# Exemplo: E(salário | gênero = mulher)

# Exemplo com dados da PNAD

# Notas:
# log(1) = 1
# log(1 + 0.1) ~= 0.1

# A melhor previsão a partir da nossa amostra é a esperança condicional
# Por quê? 
# Devido ao Erro Quadrático Médio

# Erro Quadrático Médio = (observação - previsão)^2

# Quanto menor o EQM, melhor minha previsão

# Exercício: calcular o EQM
# OBS.: script no livro

df_erro <- df %>%
  group_by(genero) %>%
  mutate(cond_exp = mean(log_salario),
         cond_median = median(log_salario)) %>%
  ungroup() %>%
  mutate(erro = (log_salario - cond_exp),
         erro_median = (log_salario - cond_median))

df_erro %>%
  select(log_salario, genero, cond_exp, erro, cond_median, erro_median)

df_erro %>%
  summarise(eq = round(sum(erro),4),
            eqm =  sum(erro^2),
            eqm_median = sum(erro_median^2))


# Capítulo 6 --------------------------------------------------------------

# E[salário | genero] = E[Y|X]

# E[salário | genero = mulher] = E[Y|X = x1]

# E[Y|X = x1] = m(x1) -> isso é uma função

# Simulações

# Suponha que Y = X^2 + U

set.seed(234)
n <- 1000
x <- rnorm(n)
u <- rnorm(n)
y <- x^2 + u

m1 <- mean(y)

m2 <- median(y)

erro1 <- y - m1
erro2 <- y - m2

print(sum(erro1^2))

# CEF com x binário

x <- rbinom(1000, 2, .5)
u <- rnorm(1000)
y <- x^2 + u

df <- data.frame(x=x, y=y)

df %>% 
  group_by(x) %>% 
  summarise(exp_cond = mean(y))

df %>% 
  ggplot() +
  aes(x = x, y = y) %>% 
  geom_point()
