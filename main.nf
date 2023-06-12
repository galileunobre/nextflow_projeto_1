
params.resultados = 'resultados'
params.pns = "$projectDir/PNS.csv"

process TRATAMENTO {
    publishDir "resultados", mode: 'copy'
    container 'galileunobre/analise_reg:v1.1'

    input: path PNS

    output: path 'pns19t.csv'
    
    script:
    """
     #!/usr/bin/Rscript

    library(tidyverse)
    
    ### buscando base de dados
    dados <- read.csv("PNS.csv", sep = ";", header = TRUE)
    head(dados)
    
    dados <- filter(dados, V0015 == 1)

    ### criando as variáveis da análise

    #var UF (estado), idade e salário
    dados <- dados %>%
    rename(UF = V0001,
             Idade = C008,
             Salario = E01602,
             consultas = J012)

    #var mulher
    dados\$sexo <- dados\$C006
    dados\$sexo <- ifelse(dados\$sexo == 1, 1, 0)
    dados\$mulher <- ifelse(dados\$sexo == 0, TRUE, FALSE)
    dados\$mulher
    
    #var casado
    dados\$casado <- ifelse(dados\$C011 == 1, TRUE, FALSE)
    dados\$casado

    #var nao_branco
    dados\$nao_branco <- ifelse(dados\$C009 == 1, FALSE, TRUE)
    dados\$nao_branco
    
    #var rural
    dados\$rural <- ifelse(dados\$V0026 == 2, TRUE, FALSE)
    dados\$rural
    
    #var redes_sociais
    dados\$redes_sociais <- ifelse(dados\$P04502 == 6, FALSE, TRUE)
    dados\$redes_sociais
    
    #var ocupado
    dados\$ocupado <- ifelse(dados\$VDE002 == 1, TRUE, FALSE)
    dados\$ocupado
    
    ## Filtrar somente as colunas que serão usadas
    dados <- select(dados, UF, Idade, Salario, sexo, mulher, casado,
                    nao_branco, rural, redes_sociais, ocupado, consultas)
    
    dados <- dados %>%
    filter(consultas <= 52)

    write.csv(dados, file = "pns19t.csv")

    """
}

process DESCRITIVA {
    publishDir "resultados", mode: 'copy'
    container 'galileunobre/analise_reg:v1.1'

    input: path pns19t
    output: 
    path 'Analise_descritiva.csv'
    path 'Estatisticas_consultas.csv'

    script:
    """
    #!/usr/bin/Rscript

    library(tidyverse)

    ### buscando base de dados
    dados <- read.csv("pns19t.csv", sep = ",", header = T)
    head(dados)

    ### Analise descritiva da base de dados

    # tabela com a Média e desvio padrão das variáves explicativas
    variaveis <- tribble(~Variáveis, ~Médias, ~ Desvio_Padrão,
        "Idade", mean(dados\$Idade, na.rm = T), sd(dados\$Idade, na.rm = T),
        "Salário", mean(dados\$Salario, na.rm = T), sd(dados\$Salario, na.rm = T),
        "Mulher", mean(dados\$mulher, na.rm = T), sd(dados\$mulher, na.rm = T),
        "Casado",  mean(dados\$casado, na.rm = T), sd(dados\$casado, na.rm = T),
        "Não branco", mean(dados\$nao_branco, na.rm = T), sd(dados\$nao_branco, na.rm = T),
        "Rural", mean(dados\$rural, na.rm = T), sd(dados\$rural, na.rm = T),
        "Redes Sociais", mean(dados\$redes_sociais, na.rm = T), sd(dados\$redes_sociais, na.rm = T),
        "Ocupado", mean(dados\$ocupado, na.rm = T), sd(dados\$ocupado, na.rm = T)
        )

    variaveis
    ## Analisando a variável dependente

    consultas <- tribble(~Média, ~Desvio_Padrão,
                     mean(dados\$consultas, na.rm = T), sd(dados\$consultas, na.rm = T))
    # salvando tabelas
    write.table(variaveis, file = "Analise_descritiva.csv")
    write.table(consultas, file = "Estatisticas_consultas.csv")

    """

}

process ANALISE {
    publishDir "resultados", mode: 'copy'
    container 'galileunobre/analise_reg:v1.1'

    input: path pns19t

    output: path 'Resultados.csv'

    script:
    """
    #!/usr/bin/Rscript

    library(AER)
    library(MASS)

    ### buscando base de dados
    dados <- read.csv("pns19t.csv", sep = ",", header = T)

    attach(dados)
    ## rodando o modelo de Poisson
    poisson <- glm(data = dados, consultas ~ Idade + 
                 Salario + 
                 mulher + 
                 casado + 
                 nao_branco + 
                 rural + 
                 redes_sociais + 
                 ocupado)
    summary(poisson)

    # Verificando a distribuição dos resíduos
    res_p <- resid(poisson, type = "pearson")
    sum(res_p^2)/poisson\$df.residual

    ## rodando o modelo binomial negativo
    bi_negativo <- glm.nb(data = dados, consultas ~ Idade + 
                 Salario + 
                 mulher + 
                 casado + 
                 nao_branco + 
                 rural + 
                 redes_sociais + 
                 ocupado)
    summary(bi_negativo)

    # Verificando a distribuição dos resíduos
    res_bn <- resid(bi_negativo, type = "pearson")
    sum(res_p^2)/bi_negativo\$df.residual

    resultados <- tribble(~Modelo, ~Dist._Residuos, ~AIC,
                     "Poisson", sum(res_p^2)/poisson\$df.residual, AIC(poisson),
                     "Binomial Negativo", sum(res_p^2)/bi_negativo\$df.residual, AIC(bi_negativo)
                     )

    ## Arquivo de resultados 

    # Salvando tabelas
    write.table(resultados, file = "Resultados.csv")
    """
}

workflow {
    TRATAMENTO(params.pns)
    DESCRITIVA(TRATAMENTO.out[0])
    ANALISE(TRATAMENTO.out[0])
}
