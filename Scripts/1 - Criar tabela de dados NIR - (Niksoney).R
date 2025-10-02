---
# title: "CRIAR TABELA COM OS DADOS DO NIR" - CAP 1 (DISSERTAÇÃO)
# Niksoney Azevedo Mendonça
# Email: niksoneyazevedo2017@gmail.com
# Este estudo foi realizado para o complexo de espécies scaly (8 spp.) do genêro Microgramma
---
############################################################################################################################################
install.packages("readr")
#############################################################################################################################################
library(readr)
#############################################################################################################################################
###################################### CRIAR A TABELA COM OS ARQUIVOS DAS LEITURAS###########################################################
#############################################################################################################################################

# Defina o caminho da pasta que contém os arquivos CSV
folder_path <- "C:/Users/nikso/OneDrive/Vida acadêmica - Niksoney Azevedo/2.Biologia Vegetal_UFPE (Mestrado_2023-2024)/_NIKSONEY AZEVEDO/1 - ESPECTROSCOPIA - (NIR)/Diretório_NIR/Leituras/260624"

# Inicialize um data frame vazio para armazenar os dados extraídos
result_df <- data.frame()

# Lista todos os arquivos na pasta
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Itere sobre cada arquivo na pasta
for (file_path in files) {
  # Leia o arquivo CSV
  df <- read.csv(file_path, sep = ";")

  # Extraia os dados após o ";" e concatene em uma única string
  extracted_data <- paste(df[, 2], collapse = ";")

  # Crie uma nova linha com os dados extraídos e o nome do arquivo como identificador
  result_df <- rbind(result_df, data.frame("Nome do Arquivo" = file_path, "resultados" = extracted_data))
}

# Salve o data frame de resultados em um novo arquivo CSV
write.csv(result_df, file = "NYresults.csv", row.names = FALSE)

# Imprima uma mensagem de sucesso
cat("NYresults")
