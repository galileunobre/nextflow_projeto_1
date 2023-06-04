FROM r-base

LABEL maintainer = "galileunobre <galileunobree@gmail.com>"

# Atualizar e instalar dependências do sistema
RUN apt-get update \
    && apt-get install -y \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev

# Instalar dependências do R
RUN R -e "install.packages(c('tidyverse', 'AER', 'MASS'))"

# Definir diretório de trabalho
WORKDIR /diretorio
