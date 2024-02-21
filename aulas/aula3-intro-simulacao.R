
# Aula 3 - Simulação no R -------------------------------------------------

# Bibliografia:
# Galdino. Introdução à Simulação (cap. 4). In: Introdução à Regressão

# Dois principais usos da simulação:

# 1. Criar um mundo aritifical
# 2. Aproximar de quantidades matemáticas

# Exemplo de aproximação -------------------------------------------------- 

# Fatorial de 10 (10!) = 3628800
factorial(10)

# Utilizando a fórmula de Stirling

# Passo 1: Especificar a semente para tornar a simulação reproduzível -----
set.seed(2)

# Passo 2: Criar função com a fórmula de Stirling -------------------------

stirling_aprox <- function(n) {
  
  sqrt(2*pi)*n^(n+1/2)*exp(-n)
  
}

stirling_aprox(10)

# Passo 3: Verificar razão da aproximação para o valor correto ------------
# sintaxe: stirling_aprox(n)/valor_correto

stirling_aprox(10)/3628800 # retorna 0.991704

1 - stirling_aprox(10)/3628800 # erro percentual = 0,8%

# "Como se vê, a fórmula de Stirling é bem precisa para aproximar fatorial e 
# muito mais fácil de computar"

# "a probabilidade de um evento pode ser aproximada por simulações, na medida 
# em que aproximamos a frequência relativa com que o fenômeno acontece em nossas
# simulações"

# Exemplo: dado de seis faces ---------------------------------------------
# Vamos simular a probabilidade do número 6 sair em um dado de seis lados

# especificando semente
set.seed(234)

# numero de amostras
n <- 10000

# 10000 amostras de um lançamento de dados de 6 lados
resultado <- sample(1:6, n, TRUE)

# frequência relativa de 6 é dada pelo número de 6 / total de amostras
prob_6 <- sum(resultado == 6)/n

prob_6 # = 0.1689
1/6    # = 0.1666667

# a aproximação converge para o verdadeiro valor à medida que n cresce

vec_amostra <- c(100, 1000, 10000, 100000, 1000000)

# lista vazia para armazenar os resultados das simulações
resultado_lista <- list()

# vetor vazio para armazenar a frequência relativa de 6
vec_prob6 <- numeric()

# loop sobre os tamanhos das amostras
for (i in 1:length(vec_amostra)) {
  
  # n amostras de um lançamento de dado de 6 lados
  resultado_lista[[i]] <- sample(1:6, vec_amostra[i], TRUE)
  
  # frequência relativa de 6 é dada por número de 6 / total de amostras
  vec_prob6[i] <- sum(resultado_lista[[i]] == 6)/vec_amostra[i]
  
}

print(vec_prob6)

# 4.1 Configuração --------------------------------------------------------

# Definir o modelo de probabilidade, variáveis aleatórias e eventos a 
# serem modelados

# 4.2 Simular -------------------------------------------------------------

for(i in 1:10) {
  print(i)
}

# o i vale de 1 até 10. print(i) imprime 1. Sys.sleep(1) o PC espera 1 seg antes
# de printar o próximo valor. Esse processo se chama "iteração"

for(i in 1:10) {
  print(i)
  Sys.sleep(1)
}

# Importância: automatizar instruções repetidas

# Número de jogadas/simulações
n <- 1000

# vetor X, para armazenar o resultado de cada uma das n jogadas
X <- numeric()

# Simulando n vezes

for (i in 1:n) {
  X[i] <- sample(1:4, size = 1)
}

# Visualizando as 20 primeiras jogadas
head(X, 20)

# Fazendo o laço manualmente

y <- numeric()
y[1] <- sample(1:4, 1)
y[2] <- sample(1:4, 1)
y[3] <- sample(1:4, 1)
y[4] <- sample(1:4, 1)
# ...

# Contexto de uso: rodar um modelo de regressão com várias especificações dierentes.
# O sys.sleep é uma boa prática porque conforme o PC é muito rápido, pode derrubar
# o site sobre o qual queremos fazer uma raspagem de dados


# 4.3 Resumir -------------------------------------------------------------

# Queremos não olhar todos os números simulados, mas resumir a simulação

# prob X = 1
sum(X == 1)/n

# prob X = 2
sum(X == 2)/n

# prob X = 3
sum(X == 3)/n

# prob X = 4
sum(X == 4)/n

# resumo geral
summary(X)

# Como estamos simulando e aproximando com um n finito o que deveria ser 
# infinito, temos de saber se nossa aproximação é boa

# Se sabemos a verdade, podemos calcular como o erro se comporta com o n que 
# escolhemos, ou ver como muda à medida que o n cresce

# Vamos supor que queremos sortear 1k amostras, guardar e sortear novamente
# 1k e guardar e refazer esse processo várias vezes. Para isso, precisamos 
# fazer um laço dentro de laço

# número de jogadas/simulações
n <- 1000

# vetor X, para armazenar o resultado de cada uma das n jogadas
X <- numeric()

# número de replicações da simulação
k <- 100

# vetor para armazenar o erro medio
erro_medio <- numeric()

# simulando n vezes
for (j in 1:k) {             # 
  for( i in 1:n){
    X[i] <- sample(1:4, size=1)  # sorteio
  }
  p1 <- sum(X==1)/n # guarda a prob em prop para X == 1
  p2 <- sum(X==2)/n # guarda a prob em prop para X == 2
  p3 <- sum(X==3)/n # guarda a prob em prop para X == 3
  p4 <- sum(X==4)/n # guarda a prob em prop para X == 4
  erro_medio[j] = (abs(p1 - .25) + # P(x == 1) - 0.25 (valor teórico)
                     abs(p2 - .25) + 
                     abs(p3 - .25) + 
                     abs(p3 - .25)) /4
}

summary(erro_medio)

# abs (número absoluto)

# Qual n usar? Por que não 10000, 100000, etc.?

# Interpretação: quero fazer 100x um sorteio de 1000 amostras

# Capítulo 5 --------------------------------------------------------------

# 5.1 Teoria --------------------------------------------------------------

# Previsão é mais fácil do que causalidade, por isso iniciamos por esse tema

# Modelos de regressão não é a mesma coisa que modelos de regressão linear

# Modelo de regressão: origem ---------------------------------------------

# Ideia de regressão à média
# Exemplo: notas escolares
# Existem componentes que são determinados (que determinam a nota, como esforço), 
# outros que são aleatórios (como um dia ruim no dia de uma prova. por ex.)

# Como os dados estão em relação à média? 

# Esperança Condicional ---------------------------------------------------

# Podemos interpretar a esp. condicional de 3 maneiras
# 1. Causal
# 2. Descritiva
# 3. Preditiva

# Exemplo: se aumentamos a qtde de horas trabalhadas, quanto a renda de uma 
# pessoa aleatória aumentaria? Estamos pensando aqui no nosso banco de dados

# Vamos falar somente de previsão, deixaremos a causalidade de lado devido a 
# sua dificuldade

# Exemplo: estimar um índice de saúde de uma pessoa dado que ela está no 
# hospital

# 1º passo: índice de saúde de uma pessoa
# Estimação: aleatório

# 2º passo: ...dado que a pessoa está no hospital
# Estimação: com essa nova info. temos razões para estimar que o índice dela
# é baixo

# ATENÇÃO: não é causalidade, porque não é o hospital que deixa a pessoa doente


# Perguntas causais -------------------------------------------------------

# Existem dois tipos de perguntas, em geral

# 1. Quais as causas da 2ª GM? - Efeito de várias variáveis

# 2. Qual o efeito da vacina sobre as internações? - Efeito de 01 variável

# Indicação de leitura
# Pearl. The book of why
# Rubin
# Mostly Harmless: Econometrics

# 5.3 CEF -----------------------------------------------------------------

# Conditional Expectation Function

# X : usado para prever (v. independente, v. preditora, v. regressora, v. input, v. explicativa)
# Y : previsão (v. dependente, v. resposta, v. predita, v. resultado, regressando)




