
# Lista 5 (resolução) -----------------------------------------------------

# Discente: Luiz Henrique da Silva Batista
# Número USO: 12687228

# Exercício 1 -------------------------------------------------------------

# Ver: https://cran.r-project.org/web/packages/PNADcIBGE/PNADcIBGE.pdf
# Instale o pacote
# install.packages("PNADcIBGE")

# Carregue o pacote
library(PNADcIBGE)

# Importe os dados desejados
data <- get_pnadc(year=2017,
                  quarter=4,
                  selected=FALSE,
                  vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
                  design=FALSE,
                  savedir=tempdir()
                  )

# Por razões didáticas, selecionamos "design=FALSE" para ignorar o plano amostral.
# Não faça isso em sua pesquisa.

# Selecione apenas as variáveis úteis para esta lista:
library(tidyverse)
library(tidylog)

data <- data |> 
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)

# Renomeie as variáveis:
data <- data |> 
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)

# Exercício 2 -------------------------------------------------------------

### Utilize uma regressão linear simples para estimar a correlação entre a renda
### (variável dependente) e o sexo (variável independente)

# Transformando a v. sexo em dummy (0 e 1)
data <- data |> 
  mutate(sexo_dummy = case_when(
    Sexo == "Homem" ~ 1, 
    Sexo == "Mulher" ~ 0
    )
    )

modelo1 <- lm(Renda ~ sexo_dummy, data = data)

summary(modelo1)

### Escreva a equação correspondente a essa regressão (**OPCIONAL**)

# Y = \alpha + \beta X
# Renda = \alpha + \beta Sexo

### Inteprete os coeficientes

# O alpha $\alpha$ representa o intercepto do nosso modelo e nada mais é que o 
# valor de $Y$ quando $X$ é igual a 0. Portanto, o alpha $\alpha$ é o ponto em 
# que a nossa reta cruza o eixo $y$.

# Já o coeficiente angular da reta $\beta$ fornece a inclinação da reta. Assim, 
# podemos interpretá-lo como quanto em média $y$ deve aumentar para um aumento de
# uma unidade em $X$.

# Em nosso exemplo, o salário esperado das mulheres (x = 0) é igual a R$ 1720.78
# e o salario esperado dos homens é cerca de R$ 357.17 maior do que o das 
# mulheres

### Apresente os resultados da sua regressão em uma tabela utilizando a 
# função "stargazer".

# Exercício 3 -------------------------------------------------------------

### Com base na questão anterior, qual é a renda média das mulheres? E a dos 
### homens? Confirme que os resultados coincidem com o cálculo das médias para 
### cada sexo utilizando a função "summarise".

# A renda média das mulheres é R$ 1720.78 e a renda média dos homens é 
# R$ 2077.95 (R$ 1720.78 + R$ 357.17)

# Exercício 4 -------------------------------------------------------------

### Utilize uma regressão linear simples para estimar a correlação entre a renda 
### (variável dependente) e as horas trabalhadas (variável independente).

modelo2 <- lm(Renda ~ Horas_trabalhadas, data = data)

### Escreva a equação correspondente a essa regressão (**OPCIONAL**)

# Renda = \alpha + \beta Horas trabalhadas

### Inteprete os coeficientes

summary(modelo2)

# Quando x = 0 a renda estimada é de R$ 846.7765
# A cada hora a mais tralhada é esperado um aumento de R$ 28.9009 na reda
 
### Qual é a renda prevista para uma pessoa que trabalha 40 horas por semana? 
### Considere que a variável dependente refere-se à renda mensal

renda_40 = 846.7765 + 28.9009 * 40
 
### Apresente os resultados da sua regressão em uma tabela utilizando a função 
### "stargazer"

# Exercício 5 -------------------------------------------------------------

# Calcule os intervalos de confiança para os coeficientes das duas regressões
# das questões anteriores ao nível de confiança de 95%.

summary(modelo1)
library(tidyverse)

modelo2

ic_alpha1 = 846.8 + (2 * 10.08)
ic_alpha1 = 846.8 - (2 * 10.08)

ic_alpha1 <- sum(846.8 - 2 * 10.08) |> c()
sum(1*2)

pull(ic_alpha1)

tibble(
  intervalor_conf = c(846.8 + 2 * 10.08)
)


# Explique o que representam os intervalos de confiança

# O que eles informam a respeito da significância estatística (ao nível de 5%) 
# dos coeficientes estimados?
  
# O que significa dizer que os coeficientes são estatisticamente significantes 
# ou insignificantes?
