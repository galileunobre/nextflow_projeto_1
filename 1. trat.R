# abrindo pacotes
library(tidyverse)

### buscando base de dados
dados <- read.csv("PNS.csv", sep = ",", header = T)
head(dados)

dados <- dados[1:8000,]

dados <- filter(dados, V0015 == 1)

### criando as variáveis da análise

#var UF (estado), idade e salário
dados <- dados %>%
  rename(c("UF" = "V0001", 
           "Idade" =  "C008", 
           "Salario" = "E01602",
           "consultas" = "J012"
           ))

#var mulher
dados$sexo <- dados$C006
dados$sexo <- ifelse(dados$sexo == 1, 1, 0)
dados$mulher <- ifelse(dados$sexo == 0, TRUE, FALSE)
dados$mulher

#var casado
dados$casado <- ifelse(dados$C011 == 1, TRUE, FALSE)
dados$casado

#var nao_branco
dados$nao_branco <- ifelse(dados$C009 == 1, FALSE, TRUE)
dados$nao_branco

#var rural
dados$rural <- ifelse(dados$V0026 == 2,TRUE, FALSE)
dados$rural

#var redes_sociais
dados$redes_sociais <- ifelse(dados$P04502 == 6, FALSE, TRUE)
dados$redes_sociais

#var ocupado
dados$ocupado <- ifelse(dados$VDE002 == 1, TRUE, FALSE)
dados$ocupado

## Filtrar somente as colunas que serão usadas
dados <- select(dados, UF, Idade, Salario, sexo, mulher, casado,
                nao_branco, rural, redes_sociais, ocupado, consultas)

dados <- dados %>%
  filter(consultas <= 52)


write.csv(dados, file = "pns_19t.csv")
