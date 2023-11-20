
# Material de apoio para esta lista:
# https://jonnyphillips.github.io/Analise_de_Dados_2022/

# Exercício 1 -------------------------------------------------------------

install.packages("devtools")
devtools::install_github("tbrugz/ribge")

library(ribge)

pop2020 <- populacao_municipios(2020)

# Exercício 2 -------------------------------------------------------------

# Selecione apenas as observações referentes ao estado de Sao Paulo.
# Antes da análise, limpe a base de dados conforme os seguintes passos: 
# (i) remova as variáveis ”codigo uf” e ”populacao str”
# (ii) renomeie ”nome_munic” para ”municipio” e
# (iii) para todos os nomes de municipios contidos na sua nova variável 
# municipio, coloque todos os caracteres em letra minuscula.

library(tidyverse)

pop2020_limpo <- pop2020 |> 
  # removendo as variáveis codigo_uf e populacao_str
  dplyr::select(-codigo_uf, -populacao_str) |> 
  # renomeando a variável nome_munic para municipio
  dplyr::rename(municipio = nome_munic) |> 
  # colocando todos os nomes dos municípios em letras minúsculas
  dplyr::mutate(municipio = tolower(municipio)) |> 
  # selecionando apenas os municípios do estado de São Paulo
  dplyr::filter(uf == "SP")

# Quantos municípios há no estado de São Paulo?

pop2020_limpo |> 
  dplyr::count(uf) 

# Qual é o menor município do estado? Quantos habitantes ele tem?

menor_municipio <- pop2020_limpo |> 
  # resumindo o dataset em apenas duas colunas para facilitar a visualização
  dplyr::select(municipio, populacao) |> 
  # colocando os dados em ordem crescente pela população
  dplyr::arrange(populacao)

# criando tabela com uma amostra de 10 municípios para facilitar a visualização
knitr::kable(menor_municipio[1:10, ])

# Exercício 3 -------------------------------------------------------------

# Para a variável ”populacao”, calcule: (i) a média, (ii) a mediana, 
# (iii) o desvio padrão e (iv) a variância

pop2020_limpo |> 
  dplyr::summarise(media = mean(populacao, na.rm = TRUE),
                   mediana = median(populacao, na.rm = TRUE),
                   desvio_padrao = sd(populacao, na.rm = TRUE),
                   variancia = var(populacao, na.rm = TRUE)) |> 
  knitr::kable()

# Exercício 4 -------------------------------------------------------------

# Note que essas estatísticas não fornecem informações suficientes sobre a 
# distribuição da população. Crie um gráfico de densidade que permita 
# visualizar essa distribuição (dica: use o pacote ggplot2).

# Criando gráfico de densidade
pop2020_limpo |> 
  ggplot(
    aes(x = populacao)
  ) +
  # geom_histogram(bins = 30L, fill = "#4682B4", alpha = 0.5) +
  geom_density(alpha = .5) +
  labs(
    x = "População",
    y = "Frequência",
    title = "Distribuição da população dos municípios do Estado de SP",
    subtitle = "Em escala logarítmica"
  ) +
  theme_bw() +
  # colocando em escala logarítmica 
  scale_x_log10() +
  theme(
    plot.title = element_text(size = 11,
                              face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  )

# O que você observa?
# Qual parece ser a medida mais adequada de tendência central: a média ou a mediana?

# Exercício 5 -------------------------------------------------------------

# Agora crie novamente o gráfico de densidade, mas apenas para os municípios 
# com menos de 50.000 habitantes.

pop2020_limpo |> 
  # filtrando apenas os municípios com até 50 mil habitantes
  dplyr::filter(populacao < 50000) |> 
  ggplot(
    aes(x = populacao)
  ) +
  # geom_histogram(bins = 30L, fill = "#4682B4", alpha = 0.5) +
  geom_density(alpha = .5) +  
  labs(
    x = "População",
    y = "Frequência",
    title = "Distribuição da população dos municípios do Estado de SP",
    subtitle = "Apenas municípios com até 50 mil habitantes"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 11,
                              face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10)
  )

# Quantos municípios com menos de 50.000 há?

pop2020_limpo |> 
  # filtrando os muncípios com até 50 mil habitantes
  dplyr::filter(populacao < 50000) |> 
  dplyr::count(uf) |> 
  knitr::kable()

# Em comparação ao gráfico anterior, o que você observa?

# Exercício 6 -------------------------------------------------------------

# Para esta questão, importe novamente a base de dados original (para 2020).
# Calcule a média da população para cada um dos estados brasileiros e informe 
# quais deles possuem maior e menor população média por município.

# utilizando a base original
pop2020 |>
  dplyr::group_by(uf) |> 
  # calculando a média populacional de cada estado por município
  dplyr::summarise(pop_media_municipio = mean(populacao)) |> 
  # colocando os dados em ordem crescente pela população média
  dplyr::arrange(pop_media_municipio) |> 
  knitr::kable()

# Exercício 7 -------------------------------------------------------------
