
# Exercício 1 -------------------------------------------------------------

# Rode ?rnorm no R e leia o help. Se preciso, consulte outras fontes. Explique o que
# a função faz. Verifique que essa função gera uma simulação de uma distribuição normal,
# em que você pode especificar a média e o desvio-padrão. Verifique que entendeu rodando
# função e gerando valores simulados da função rnorm. Pode usar média 0 e desvio-padrão 1,
# que são o default da função

?rnorm

# R: A função rnorm gera uma sequência de números aleatórios (a quantidade desses
# números é especificada pelo argumento `n`) que se distribuem de acordo com a
# distribuição normal. Abaixo, rodamos uma simulação rnorm de 100 números:

rnorm(100)

# Exercício 2 -------------------------------------------------------------

# Rode x <- rnorm(100, mean = 2, sd = 1). Por que a média da distribuição é diferente
# da média que você obtém rodando mean(x)?

x <- rnorm(100, mean = 2, sd = 1)

mean(x)

# R: A média da distribuição é um parâmetro, assim como o desvio padrão. Isso significa
# que as nossas observações geradas com base em rnorm estarão centralizadas em 
# torno da média (no caso, média = 2) com um dado desvio padrão (no caso, sd = 1).
# Para fins de ilustração, se diminuirmos os desvio padrão, é mais provável que
# a média das nossas observações se aproxime do parâmetro média = 2. Veja:

x2 <- rnorm(100, mean = 2, sd = .0001)

mean(x2)

# Exercício 3 (OPCIONAL) --------------------------------------------------

# Se você rodar de novo x <- rnorm(100, mean = 2, sd = 1) e calcular a média de x,
# obterá um valor um pouco diferente da primeira vez. Por quê?

x <- rnorm(100, mean = 2, sd = 1)

mean(x)

# R: Porque a função rnomr gera uma sequência de observações aleatórias com base
# em média = 2 e sd = 1. A média e o desvio padrão são parâmetros para a geração
# das observações.

# Exercício 4 -------------------------------------------------------------

# Uma forma de você armazenar as duas médias que você computou em um vetor é do
# seguinte modo:

vetor_medias <- numeric()
vetor_medias[1] <- mean(rnorm(100, mean=2, sd=1))
vetor_medias[2] <- mean(rnorm(100, mean=2, sd=1))

# Imprima o conteúdo de ”vetor_medias” e verifique que de fato armazenou duas médias

print(vetor_medias)

# Exercício 5 (OPCIONAL) --------------------------------------------------

# Repita esse procedimento 30 vezes no total, armazenando as 30 médias no vetor
# medias. Se possível, use um loop (laço) para fazer isso.

# Definindo uma semente
set.seed(11)

# Número de simulações
n <- 30

# Vetor para armazenar cada uma das n jogadas
vetor_medias <- numeric()

# Simulação
for (i in 1:n) {
  vetor_medias[i] <- mean(rnorm(n, mean=2, sd=1))
}

# Visualizando as 30 jogadas
head(vetor_medias, 30)

# Exercício 6 -------------------------------------------------------------

# Plote o histograma (use a função geom_histogram no ggplot) das médias. Para isso, crie
# um banco de dados (ggplot só aceita plotar variáveis de banco de dados) do seguinte modo:

df <- data.frame(medias = vetor_medias, sim_id = 1:30)

hist(df$medias, 
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma das médias simuladas", 
     col = "#2E72A2",
     border = "black")

# Você reconhece a distribuição apresentada pelo histograma? Se sim, qual é ela? 
# Consegue advinhar a média e desvio padrão da distribuição ou calculá-la?

# R: A distribuição se assemelha a uma distribuição normal. O cálculo da
# média e do desvio padrão estão apresentados abaixo:

mean(df$medias)
sd(df$medias)

# Exercício 7 (OPCIONAL) --------------------------------------------------

# Realize uma simulação estatística para verificar a distribuição de probabilidade dos
# resultados do lançamento de uma moeda.

# Número de simulações
n_moeda <- 1000

# Simulação do lançamento de uma moeda
moeda <- sample(1:2, size=1)

# Vetor para armazenar cada uma das n jogadas
vetor_moeda <- numeric()

# Simulação
for (i in 1:n_moeda) {
  vetor_moeda[i] <- sample(1:2, size=1)
}

# visualizando as primeiras 10 primeiras jogadas
head(vetor_moeda, 10)

# Probabilidade vetor_moeda = 1
sum(vetor_moeda == 1)/n_moeda

# Probabilidade vetor_moeda = 2
sum(vetor_moeda == 2)/n_moeda

# Exercício 8 (OPCIONAL) --------------------------------------------------

# Retire 10, 100, 1000 e 10000 valores de uma distribuição binomial (n = 20, p = 0.7).
# Apresente os histogramas.

# Z ~ N(0, 1)

# B ~ Bin(20, 0.7)

dez_rbinom <- rbinom(size = 10, n = 20, p = 0.52)

cem_rbinom <- rbinom(size = 100, n = 20, p = 0.52)

mil_rbinom <- rbinom(size = 1000, n = 20, p = 0.52)

dezk_rbinom <- rbinom(size = 10000, n = 20, p = 0.52)

df_rbinom <- data.frame(dez_rbinom, cem_rbinom, mil_rbinom, dezk_rbinom)

hist(df_rbinom$dez_rbinom,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

hist(df_rbinom$cem_rbinom,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

hist(df_rbinom$mil_rbinom,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

hist(df_rbinom$dezk_rbinom,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")


dez_rnorm <- rnorm(10, mean = 0, sd = 1)

cem_rnorm <- rnorm(100, mean = 0, sd = 1)

mil_rnorm <- rnorm(1000, mean = 0, sd = 1)

dezk_rnorm <- rnorm(10000, mean = 0, sd = 1)

df_rnorm <- data.frame(dez_rnorm, cem_rnorm, mil_rnorm, dezk_rnorm)

hist(df_rnorm$dez_rnorm,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

hist(df_rnorm$cem_rnorm,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

hist(df_rnorm$mil_rnorm,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

hist(df_rnorm$dezk_rnorm,
     freq = TRUE, # se FALSE, plota a densidade
     main = "Histograma", 
     col = "#2E72A2",
     border = "black")

