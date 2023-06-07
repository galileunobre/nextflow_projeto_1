library(AER)
library(MASS)
library(COUNT)

### buscando base de dados
dados <- read.csv("pns_19t.csv", sep = ",", header = T)

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
sum(res_p^2)/poisson$df.residual


# Previsões do modelo ajustado
prpoisson <- predict(poisson, type = "response")
plot(prpoisson)

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
sum(res_p^2)/bi_negativo$df.residual


# Previsões do modelo ajustado
prbn <- predict(bi_negativo, type = "response")
plot(prpoisson)

probabilidades <- data.frame(prpoisson, prbn)
plot(probabilidades$prpoisson)

summary(probabilidades)

resultados <- tribble(~Modelo, ~Dist._Residuos, ~AIC,
                     "Poisson", sum(res_p^2)/poisson$df.residual, AIC(poisson),
                     "Binomial Negativo", sum(res_p^2)/bi_negativo$df.residual, AIC(bi_negativo)
                     )
resultados
min(resutados$Dist._Residuos)

## Arquivo de resultados 

# Salvando tabelas
write.table(resultados, file = "Resultados.csv")
