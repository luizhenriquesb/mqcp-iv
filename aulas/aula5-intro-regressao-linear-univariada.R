
# Aula 5 ------------------------------------------------------------------

# Bibliografia: 
# Galdino. CEF (cap. 6). In: Introdução à Regressão

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

# Capítulo 6 --------------------------------------------------------------

# CEF ---------------------------------------------------------------------

