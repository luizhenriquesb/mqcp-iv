
# Material de apoio para esta lista:
# https://jonnyphillips.github.io/Analise_de_Dados_2022/

# Exercício 1 -------------------------------------------------------------

# Ver: https://cran.r-project.org/web/packages/PNADcIBGE/PNADcIBGE.pdf

# Instale o pacote
# install.packages("PNADcIBGE")
# Carregue o pacote

library(PNADcIBGE)
library(tidyverse)
library(tidylog)

# Importando a base de dados desejada
data <- PNADcIBGE::get_pnadc(year=2017,
                             quarter=4,
                             selected=FALSE,
                             vars=c("Ano", "Trimestre", "UF", 
                                    "V2007", "VD4020", "VD4035"),
                             design=FALSE,
                             savedir=tempdir())

# Selecionando variáveis de interesse
data <- data |> 
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)

# Renomeando as variáveis
data <- data |> 
  rename(sexo = V2007,
         renda = VD4020,
         horas_trabalhadas = VD4035)

# Exercício 2 -------------------------------------------------------------

# Calcule: 
  
# i) a renda média e ii) a variância da renda;

data |> 
  summarise(media = mean(renda, na.rm=TRUE),
            variancia = var(renda, na.rm=TRUE)) |> 
  knitr::kable()


# iii) a renda média dos homens e das mulheres;

data |> 
  group_by(sexo) |> 
  summarise(renda_media_sexo = mean(renda, na.rm = TRUE)) |> 
  knitr::kable()


# iv) a renda média em cada estado brasileiro;

data |> 
  group_by(UF) |> 
  summarise(renda_media_UF = mean(renda, na.rm = TRUE)) |> 
  arrange(desc(renda_media_UF)) |> 
  knitr::kable()

# v) a covariância entre a renda e o número de horas trabalhadas.
cov(data$renda, data$horas_trabalhadas, use = "complete.obs")

# Exercício 3 -------------------------------------------------------------

# Exemplifique a veracidade da equação abaixo, considerando X = Renda,
# Y = Horas trabalhadas, a = 2 e b = 3

# E[aX + bY] = a × E[X] + b × E[Y]

a <- 2
b <- 3

X <- na.omit(data$renda)             
Y <- na.omit(data$horas_trabalhadas)

espX <- mean(X)
espY <- mean(Y)

mean(a*X) + mean(b*Y) == a*espX + b*espY

# Exercício 4 -------------------------------------------------------------

# Apresente um gráfico que permita visualização adequada da média da renda por 
# estado brasileiro e sexo.

data |> 
  group_by(sexo, UF) |> 
  summarise(renda_media = mean(renda, na.rm = TRUE)) |> 
  ggplot() +
  aes(x = reorder(UF, +renda_media), fill = sexo, weight = renda_media) +
  geom_bar(position = position_dodge2(reverse = TRUE)) +
  scale_fill_hue(direction = 1) + 
  labs(
    x = "UF",
    y = "Renda Média",
    title = "Renda Média por Estado e Sexo"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12L,
                              face = "bold"),
    axis.title.y = element_text(size = 10L),
    axis.title.x = element_text(size = 10L)
  )

# Exercício 5 -------------------------------------------------------------

# Calcule:  
  
# 1. E[X|10 <= Y <= 20]

data |> 
  filter(horas_trabalhadas >= 10 & horas_trabalhadas <= 20) |> 
  summarise(mean(renda, na.rm = TRUE)) |> 
  knitr::kable()

# 2. E[X|Y >= 20]

data |> 
  filter(horas_trabalhadas >= 20) |> 
  summarise(mean(renda, na.rm = TRUE)) |> 
  knitr::kable()

# Exercício 6 -------------------------------------------------------------

# Para os itens seguintes (i a iv), remova todas as observações cuja renda seja 
# superior a 10.000 reais.

renda_menor_10k <- data |> # criando df com valores da renda até 10 mil
  filter(renda < 10000)

# i) apresente um gráfico de densidade da variável renda. Interprete;

renda_menor_10k |> 
  ggplot() +
  aes(x = renda) +
  geom_density(adjust = 1, fill = "#112446", alpha = .7) +
  labs(
    x = "Renda",
    y = "Densidade",
    title = "Gráfico de Densidade da Renda",
    subtitle = "Renda até 10 mil"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12L, face = "bold"))

# **Interpretação**  
# O gráfico mostra que existe uma concentração da renda em torno de 2500/2 
# reais e muitos valores extremos entre 5000 e 1000. Assim, trata-se de um gráfico 
# assimétrico à esquerda.  

# ii) qual é a probabilidade de que, ao retirarmos aleatoriamente uma observação 
# um indivíduo dessa base de dados, sua renda seja estritamente maior do que 1000 
# e estritamente menor do que 2000 reais? Apenas para propósitos didáticos, ignore 
# o erro amostral e trate a sua base de dados como uma população (não faça isso em
# sua pesquisa);

# Queremos calcular a P(1000 < X < 2000)

# Criando df com todos os valores das colunas "renda" e "horas_trabalhadas"
df <- data |> 
  select(renda, horas_trabalhadas)

# Criando objeto que guarda P(1000 < X < 2000)
prob1 <- renda_menor_10k |> 
  select(renda, horas_trabalhadas) |> 
  filter(renda > 1000 &     # especificando X > 1000
           renda < 2000) |> # especificando X < 2000
  nrow()/nrow(df)           # Calculando a probabilidade

knitr::kable(prob1)

# Outra maneira ----------------

# número de jogadas/simulações
n1 <- 1

# simulando n vezes
simulacao1 <- sample(data$renda, n1, replace = TRUE)

# somando a qtde de vezes que saem valores entre 1000 e 2000
soma1 <- sum(simulacao1 > 1000 & simulacao1 < 2000, na.rm = TRUE)

# calculando a prob
prob1 <- soma1 / n1

# ------------------------------

# iii) apresente um gráfico de densidade da renda dado que as horas trabalhadas (Y) 
# sejam menores ou iguais a 20;

renda_menor_10k |> 
  filter(horas_trabalhadas <= 20) |> 
  ggplot() +
  aes(x = renda) +
  geom_density(adjust = 1, fill = "#112446", alpha = .7) +
  labs(
    x = "Renda",
    y = "Densidade",
    title = "Gráfico de Densidade da Renda",
    subtitle = "Considerando Renda até 10 mil e Horas Trabalhadas menores ou iguais a 20"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14L, face = "bold"))

# iv) calcule: P(1000 < X < 2000|Y <= 20)

# Criando objeto que guarda P(1000 < X < 2000|Y <= 20)
prob2 <- renda_menor_10k |> 
  select(renda, horas_trabalhadas) |> 
  filter(renda > 1000 &                 # especificando X > 1000
           renda < 2000 &               # especificando X < 1000 
           horas_trabalhadas <= 20) |>  # especificando Y <= 20
  nrow()/nrow(df)

knitr::kable(prob2)

# Outra maneira -----------------

# número de jogadas/simulações
n2 <- 10

# simulando n vezes
simulacao2 <- sample(renda_menor_10k, n2, replace = TRUE)

# somando a qtde de vezes que saem valores entre 1000 e 2000
soma2 <- sum(simulacao2 > 1000 & simulacao2 < 2000, na.rm = TRUE)
soma3 <- suma()

# calculando a prob
prob2 <- soma2 / n2

# -------------------------------


