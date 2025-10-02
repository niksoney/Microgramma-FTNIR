---
#title: "PRÉ-PROCESSAMENTO PARA DADOS ESPECTRAIS" - CAP 1 (DISSERTAÇÃO)
#Niksoney Azevedo Mendonça
#Email: niksoneyazevedo2017@gmail.com
#Este estudo foi realizado para o complexo de espécies scaly (8 spp.) do genêro Microgramma
---
#############################################################################################################################################
################################################Standard Normal Variate (SNV)################################################################
#############################################################################################################################################
library(writexl)
library(readxl)

dados <- read_excel("Tabela/Matriz_NIR.xlsx")

# Função para aplicar SNV a um único espectro
apply_snv <- function(spectrum) {
  mean_spectrum <- mean(as.numeric(spectrum))
  sd_spectrum <- sd(as.numeric(spectrum))
  snv_spectrum <- (as.numeric(spectrum) - mean_spectrum) / sd_spectrum
  return(snv_spectrum)
}

# Separar os dados em informações categóricas e valores espectrais
dados_categoricos <- dados[, 1:11]
valores_espectrais <- dados[, 12:ncol(dados)]

# Garantir que valores_espectrais contenha apenas números
valores_espectrais <- apply(valores_espectrais, 2, as.numeric)

# Aplicar SNV aos valores espectrais
valores_espectrais_snv <- t(apply(valores_espectrais, 1, apply_snv))

# Manter os nomes originais das colunas
colnames(valores_espectrais_snv) <- colnames(valores_espectrais)

# Combinar de volta as informações categóricas com os valores espectrais normalizados
dados_snv <- cbind(dados_categoricos, valores_espectrais_snv)

# Visualizar os dados após SNV
str(dados_snv)

# Salvar os dados como um arquivo Excel
write_xlsx(dados_snv, path = "Tabela/dados_snv.xlsx")

#############################################################################################################################################
###################################################Centralizar na média################################################################
#############################################################################################################################################
#centralizar na média

library(writexl)

dados <- read_excel("Tabela/dados_snv.xlsx")

# Função para centralizar na média um único espectro
centralizar_na_media <- function(spectrum) {
  media_espectro <- mean(as.numeric(spectrum))
  espectro_centralizado <- as.numeric(spectrum) - media_espectro
  return(espectro_centralizado)
}

# Separar os dados em informações categóricas e valores espectrais
dados_categoricos <- dados[, 1:11]
valores_espectrais <- dados[, 12:ncol(dados)]

# Aplicar centralização na média aos valores espectrais
valores_espectrais_centralizados <- t(apply(valores_espectrais, 1, centralizar_na_media))

# Manter os nomes originais das colunas
colnames(valores_espectrais_centralizados) <- colnames(valores_espectrais)

# Combinar de volta as informações categóricas com os valores espectrais centralizados
dados_centralizados <- cbind(dados_categoricos, valores_espectrais_centralizados)

# Visualizar os dados após centralização na média
str(dados_centralizados)

# Salvar os dados centralizados como um arquivo Excel
library(writexl)
write_xlsx(dados_centralizados, path = "Tabela/dados_outli_snv_media.xlsx")

#############################################################################################################################################
#################################################Derivada de primeira ordem##################################################################
#############################################################################################################################################

dados <- read_excel("Tabela/Matriz_NIR.xlsx")

# Função para aplicar a derivada de primeira ordem a um único espectro
derivada_primeira_ordem <- function(spectrum) {
  derivada_spectrum <- diff(as.numeric(spectrum))
  return(derivada_spectrum)
}

# Separar os dados em informações categóricas e valores espectrais
dados_categoricos <- dados[, 1:11]
valores_espectrais <- dados[, 12:ncol(dados)]

# Aplicar a derivada de primeira ordem aos valores espectrais
valores_espectrais_derivada <- t(apply(valores_espectrais, 1, derivada_primeira_ordem))

# Ajustar os nomes das colunas após a derivada (uma coluna a menos devido ao diff)
novos_nomes_colunas <- paste("Derivada", 1:(ncol(valores_espectrais_derivada)), sep = "_")
colnames(valores_espectrais_derivada) <- novos_nomes_colunas

# Combinar de volta as informações categóricas com os valores espectrais derivados
dados_derivada <- cbind(dados_categoricos, valores_espectrais_derivada)

# Visualizar os dados após aplicar a derivada de primeira ordem
str(dados_derivada)

# Salvar os dados derivados como um arquivo Excel
library(writexl)
write_xlsx(dados_derivada, path = "Tabela/dados_derivada.xlsx")


















dados <- read_excel("Tabela/Matriz_NIR.xlsx")

library(tidyverse)
library(dplyr)
library(reshape2)

# Supondo que seu dataframe se chame 'dados'
# Convertendo os dados espectrais para formato longo
dados_long <- melt(dados, id.vars = colnames(dados)[1:11], 
                   variable.name = "Comprimento_de_onda", 
                   value.name = "Valor_Espectral")

# Renomeando a coluna 3 para 'Especie'
colnames(dados_long)[2] <- "Species"

# Aplicando o teste de Kruskal-Wallis para cada comprimento de onda
resultados <- dados_long %>%
  group_by(Comprimento_de_onda) %>%
  summarise(
    p_value = kruskal.test(Valor_Espectral ~ Species)$p.value,
    chi_squared = kruskal.test(Valor_Espectral ~ Species)$statistic
  )

# Calculando uma medida geral de significância
estatistica_total <- sum(resultados$chi_squared)
gl_total <- length(unique(dados_long$Comprimento_de_onda)) * (length(unique(dados_long$Species)) - 1)

# Calculando o p-valor geral
p_valor_total <- pchisq(estatistica_total, df = gl_total, lower.tail = FALSE)

# Exibindo o p-valor geral
print(paste("P-valor geral: ", p_valor_total))
  