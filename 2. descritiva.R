### buscando base de dados
dados <- read.csv("pns_19t.csv", sep = ",", header = T)
head(dados)

### Analise descritiva da base de dados

# tabela com a Média e desvio padrão das variáves explicativas
variaveis <- tribble(~Variáveis, ~Médias, ~ Desvio_Padrão,
        "Idade", mean(dados$Idade, na.rm = T), sd(dados$Idade, na.rm = T),
        "Salário", mean(dados$Salario, na.rm = T), sd(dados$Salario, na.rm = T),
        "Mulher", mean(dados$mulher, na.rm = T), sd(dados$mulher, na.rm = T),
        "Casado",  mean(dados$casado, na.rm = T), sd(dados$casado, na.rm = T),
        "Não branco", mean(dados$nao_branco, na.rm = T), sd(dados$nao_branco, na.rm = T),
        "Rural", mean(dados$rural, na.rm = T), sd(dados$rural, na.rm = T),
        "Redes Sociais", mean(dados$redes_sociais, na.rm = T), sd(dados$redes_sociais, na.rm = T),
        "Ocupado", mean(dados$ocupado, na.rm = T), sd(dados$ocupado, na.rm = T)
        )

variaveis
## Analisando a variável dependente

consultas <- tribble(~Média, ~Desvio_Padrão,
                     mean(dados$consultas, na.rm = T), sd(dados$consultas, na.rm = T))
# salvando tabelas
write.table(variaveis, file = "Analise_descritiva.csv")
write.table(consultas, file = "Estatisticas_consultas.csv")
