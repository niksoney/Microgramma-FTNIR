---
#title: "PLS-da (LOOCV/K-fold)" - CAP 1 (DISSERTAÇÃO)
#Niksoney Azevedo Mendonça
#Email: niksoneyazevedo2017@gmail.com
#Este estudo foi realizado para o complexo de espécies scaly (8 spp.) do genêro Microgramma
---
#############################################################################################################################################
library(caret)
library(MASS)
library(pls)
library(magrittr)
library(dplyr)
library(boot)
library(reshape2)
#############################################################################################################################################
#######################################PREPARA OS CONJUNTOS DE TREINO E TESTE################################################################
#############################################################################################################################################
rm(list=ls()) #Limpar a lista de arquivos

setwd("C:/Users/nikso/OneDrive/Vida acadêmica - Niksoney Azevedo/2.Biologia Vegetal_UFPE (Mestrado_2023-2024)/_NIKSONEY AZEVEDO/1 - ESPECTROSCOPIA - (NIR)/Diretório_NIR")

#Carrega dado NIR
NIRdata = read.table ('Tabela/Tabela_dados_medio_fertile.csv',sep="\t", as.is=T, header=T)
NIRdata = read.table ('Tabela/Tabela_dados_medio_sterile.csv',sep="\t", as.is=T, header=T)
NIRdata = read.table ('Tabela/Tabela_dados_medio_combinadoF&S.csv',sep="\t", as.is=T, header=T)

NIRdata$Species <- make.names(NIRdata$Species)

dados = as.data.frame(NIRdata)

#----------------------------------------------------------------------------------------------------------------------------
#SÓ USAR SE EU FOR DIVIDIR OS DADOS EM TREINO/TESTE

#define um inicio para sorteios aleatorios em todos os processos (garante reprodutibilidade)
set.seed(123)

#carrega o pacote necessario
indice = createDataPartition(dados$Species,
                             p = .7, #define an 70%/30% train/test split of the dataset
                             list = FALSE, 
                             times = 1)
treino = dados[indice,]
table(treino$Species)

teste = dados[-indice,]
table(teste$Species)
#############################################################################################################################################
#######################################PLS-DA USANDO A VALIDAÇÃO "LOOCV"#####################################################################
#############################################################################################################################################

set.seed(123)
# Definindo controle para validação cruzada leave-one-out
pls_ctrl <- trainControl(
  method = "LOOCV",
  verboseIter = TRUE,
  classProbs = TRUE,
  returnData = TRUE,
  savePredictions = "final"
)

# Definindo um intervalo amplo para ncomp para permitir seleção automática
tuneLength = 20  # Testar até 20 componentes, o que deixa a escolha mais automática

pls_grid <- expand.grid(
  ncomp = seq(1, 20)  # Grid de valores para ncomp de 1 a 20
)

pls_loocv <- train(
  x = dados[, 12:ncol(dados)],
  y = dados[, 4],  
  method = "pls",
  trControl = pls_ctrl,
  tuneGrid = pls_grid,  # Utilizar a grade de parâmetros definida para ncomp
  tuneLength = tuneLength,  # Tentar ajustar com ncomp = 5 inicialmente
  verbose = TRUE,
  maxit = 4000  # Número máximo de iterações
)

# Criando a matriz de confusão para os dados de teste
# conf_matrix_teste <- confusionMatrix(pls_kfold$pred$pred, pls_kfold$pred$obs)

# Extraindo a tabela de confusão
# conf_table <- conf_matrix_teste$table

# Especificando as espécies de interesse
# especies_interesse <- c('Mnan', 'Mrep', 'Mtect', 'Mtob')
# especies_interesse <- c('Mper', 'Mlat', 'Mdic', 'Mpil')

# Filtrando a tabela de confusão para incluir apenas as espécies de interesse
# conf_table_interesse <- conf_table[rownames(conf_table) %in% especies_interesse, colnames(conf_table) %in% especies_interesse]

# Exibindo a tabela de confusão filtrada
# print(conf_table_interesse)

# Criando uma nova matriz de confusão com a tabela filtrada
# conf_matrix_interesse <- confusionMatrix(conf_table_interesse)

# Exibindo todas as estatísticas da nova matriz de confusão
# print(conf_matrix_interesse)

#----------------------------------------------------------------------------------------------------------

# Criando a matriz de confusão para os dados de teste
conf_matrix_teste <- confusionMatrix(pls_loocv$pred$pred, pls_loocv$pred$obs)

# Exibindo os resultados da matriz de confusão
model_accuracy_teste <- conf_matrix_teste
print(model_accuracy_teste)

# Criando a matriz de confusão original
conf_matrix_teste <- confusionMatrix(pls_loocv$pred$pred, pls_loocv$pred$obs)

# Extraindo a tabela de confusão
conf_table <- conf_matrix_teste$table

# Transpondo a tabela de confusão
conf_table_transposed <- t(conf_table)

# Criando uma nova matriz de confusão com a tabela transposta
conf_matrix_teste_transposed <- confusionMatrix(conf_table_transposed)

# Exibindo os resultados da nova matriz de confusão
print(conf_matrix_teste_transposed)

#-------------------------------------------------------------------------------

# Criando um vetor com os 8 valores desejados para a nova coluna "BalAcc%"
BalAcc_values <- c(94, 94, 91, 100, 85, 87, 86, 90)

# Adicionando os valores como uma nova coluna na matriz de confusão transposta
conf_table_transposed <- cbind(conf_table_transposed, 'BalAcc%' = BalAcc_values)

# Criando um vetor com os nomes das classes
classes_values <- c("Mdic", "Mlat", "Mnan", "Mper", "Mpil", "Mrep", "Mtect", "Mtob")

# Adicionando a coluna "Classes" como a primeira coluna
conf_table_transposed <- cbind(Classes = classes_values, conf_table_transposed)

# Convertendo a matriz para um data frame
conf_table_transposed <- data.frame(conf_table_transposed, stringsAsFactors = FALSE)

# Reordenando a matriz para garantir que a coluna "Classes" seja a primeira
conf_table_transposed <- conf_table_transposed[, c("Classes", setdiff(colnames(conf_table_transposed), "Classes"))]

# Exibindo a matriz de confusão transposta atualizada sem aspas
print(conf_table_transposed, quote = FALSE)

# Exportando os resultados da matriz de confusão por classe
write.table(conf_table_transposed, file='Tabela/fertilmatriz_loocv.csv', sep="\t", row.names=TRUE)
write.table(conf_table_transposed, file='Tabela/esterilmatriz_loocv.csv', sep="\t", row.names=TRUE)
write.table(conf_table_transposed, file='Tabela/combinadomatriz_loocv.csv', sep="\t", row.names=TRUE)

#----------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) #Limpar a lista de arquivos

#LS-DA_Plotar a matriz de confusão com os dados gerados
#Chamar as matrizes para fazer as analises
#loocv
tab = read.table('Tabela/fertilmatriz_loocv.csv',sep="\t", as.is=T, header=T)
tab = read.table('Tabela/esterilmatriz_loocv.csv',sep="\t", as.is=T, header=T)
tab = read.table('Tabela/combinadomatriz_loocv.csv',sep="\t", as.is=T, header=T)

#Transformar a tabela em um formato longo
data_long <- melt(tab, id.vars = "Classes")

#Renomear as colunas
colnames(data_long) <- c("Classe_predita", "Classe_real", "Valor")

#Criar a matriz de confusão
confusion_matrix <- table(data_long$Classe_predita, data_long$Classe_real)

#Recodificar 'BalAcc.' para 'BalAcc%'
data_long <- data_long %>%
  mutate(Classe_real = recode(Classe_real, "BalAcc." = "BalAcc%")) 

#Plot com ggplot2
ggplot() +
  # Parte para o preenchimento contínuo (excluindo a coluna BalAcc%)
  geom_tile(data = filter(data_long, Classe_real != "BalAcc%" & Valor != 0), aes(x = Classe_real, y = Classe_predita, fill = Valor), color = "white") +
  scale_fill_gradient(low = "#dcdcdc", high = "#808080", guide = "none") +  # Remove a legenda de preenchimento contínuo
  
  # Adiciona uma nova escala de preenchimento para a coluna BalAcc%
  ggnewscale::new_scale_fill() +
  
  # Parte para a coluna BalAcc%
  geom_tile(data = filter(data_long, Classe_real == "BalAcc%"), aes(x = Classe_real, y = Classe_predita, fill = "highlight"), color = "white") +
  scale_fill_manual(values = c("highlight" = "#545454"), guide = "none") +  # Remove a legenda de preenchimento manual
  
  # Adiciona os textos com cores condicionais e ajusta o tamanho dos números
  geom_text(data = filter(data_long, Valor != 0), aes(x = Classe_real, y = Classe_predita, label = ifelse(Valor != 0, Valor, ""), color = ifelse(Classe_real == "BalAcc%", "highlight", "normal")), size = 5) +
  scale_color_manual(values = c("normal" = "black", "highlight" = "white"), guide = "none") +  # Remove a legenda de cores de texto
  
  # Tema e ajustes
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),  # Ajusta tamanho e centraliza texto no eixo x
        axis.text.y = element_text(size = 12),  # Ajusta tamanho do texto no eixo y
        axis.title = element_text(size = 14)) +  # Ajusta tamanho dos nomes dos eixos
  labs(x = "", y = "")     

#Salvar o gráfico como um arquivo PNG
#loocv
ggsave("Figuras/matrizconfusao_fertil_LOOCV.png", width = 7, height = 7, dpi = 300)
ggsave("Figuras/matrizconfusao_esteril_LOOCV.png", width = 7, height = 7, dpi = 300)
ggsave("Figuras/matrizconfusao_combinada_LOOCV.png", width = 7, height = 7, dpi = 300)

#############################################################################################################################################
#############################################################################################################################################
#####################################PLS-DA USANDO A VALIDAÇÃO "k-FOLD"######################################################################
#############################################################################################################################################
#############################################################################################################################################
rm(list=ls()) #Limpar a lista de arquivos

setwd("C:/Users/nikso/OneDrive/Vida acadêmica - Niksoney Azevedo/2.Biologia Vegetal_UFPE (Mestrado_2023-2024)/_NIKSONEY AZEVEDO/1 - ESPECTROSCOPIA - (NIR)/Diretório_NIR")

#Carrega dado NIR
NIRdata = read.table ('Tabela/Tabela_dados_medio_fertile.csv',sep="\t", as.is=T, header=T)
NIRdata = read.table ('Tabela/Tabela_dados_medio_sterile.csv',sep="\t", as.is=T, header=T)
NIRdata = read.table ('Tabela/Tabela_dados_medio_combinadoF&S.csv',sep="\t", as.is=T, header=T)

NIRdata$Speciesabrev <- make.names(NIRdata$Speciesabrev)

dados = as.data.frame(NIRdata)

set.seed(123)
#Definindo controle para validação cruzada k-fold
pls_ctrl <- trainControl(
  method = "cv",  # Usando k-fold cross-validation
  number = 10,    # Número de folds (k)
  verboseIter = TRUE,
  classProbs = TRUE,
  returnData = TRUE,
  savePredictions = "final"
)

#Definindo um intervalo amplo para ncomp para permitir seleção automática
tuneLength = 20  # Testar até 20 componentes, o que deixa a escolha mais automática

#Treinando o modelo PLS-DA
pls_kfold <- train(
  x = dados[, 12:ncol(dados)],  # Selecione suas variáveis preditoras corretamente
  y = dados[, 4],               # Selecione sua variável de resposta corretamente
  method = "pls",
  trControl = pls_ctrl,
  tuneLength = tuneLength,  # Deixar o caret escolher o melhor ncomp automaticamente
  verbose = TRUE,
  maxit = 2000    # Número máximo de iterações
)

# Criando a matriz de confusão para os dados de teste
# conf_matrix_teste <- confusionMatrix(pls_kfold$pred$pred, pls_kfold$pred$obs)

# Extraindo a tabela de confusão
# conf_table <- conf_matrix_teste$table

# Especificando as espécies de interesse
# especies_interesse <- c('Mnan', 'Mrep', 'Mtect', 'Mtob')
# especies_interesse <- c('Mper', 'Mlat', 'Mdic', 'Mpil')

# Filtrando a tabela de confusão para incluir apenas as espécies de interesse
# conf_table_interesse <- conf_table[rownames(conf_table) %in% especies_interesse, colnames(conf_table) %in% especies_interesse]

# Exibindo a tabela de confusão filtrada
# print(conf_table_interesse)

# Criando uma nova matriz de confusão com a tabela filtrada
# conf_matrix_interesse <- confusionMatrix(conf_table_interesse)

# Exibindo todas as estatísticas da nova matriz de confusão
# print(conf_matrix_interesse)

#---------------------------------------------------------------------------------------------

# Criando a matriz de confusão para os dados de teste
conf_matrix_teste <- confusionMatrix(pls_kfold$pred$pred, pls_kfold$pred$obs)

# Exibindo os resultados da matriz de confusão
model_accuracy_teste <- conf_matrix_teste
print(model_accuracy_teste)

# Criando a matriz de confusão original
conf_matrix_teste <- confusionMatrix(pls_kfold$pred$pred, pls_kfold$pred$obs)

# Extraindo a tabela de confusão
conf_table <- conf_matrix_teste$table

# Transpondo a tabela de confusão
conf_table_transposed <- t(conf_table)

# Criando uma nova matriz de confusão com a tabela transposta
conf_matrix_teste_transposed <- confusionMatrix(conf_table_transposed)

# Exibindo os resultados da nova matriz de confusão
print(conf_matrix_teste_transposed)

#-------------------------------------------------------------------------------

# Criando um vetor com os 8 valores desejados para a nova coluna "BalAcc%"
BalAcc_values <- c(90, 94, 90, 100, 83, 83, 81, 88)

# Adicionando os valores como uma nova coluna na matriz de confusão transposta
conf_table_transposed <- cbind(conf_table_transposed, 'BalAcc%' = BalAcc_values)

# Criando um vetor com os nomes das classes
classes_values <- c("Mdic", "Mlat", "Mnan", "Mper", "Mpil", "Mrep", "Mtect", "Mtob")

# Adicionando a coluna "Classes" como a primeira coluna
conf_table_transposed <- cbind(Classes = classes_values, conf_table_transposed)

# Convertendo a matriz para um data frame
conf_table_transposed <- data.frame(conf_table_transposed, stringsAsFactors = FALSE)

# Reordenando a matriz para garantir que a coluna "Classes" seja a primeira
conf_table_transposed <- conf_table_transposed[, c("Classes", setdiff(colnames(conf_table_transposed), "Classes"))]

# Exibindo a matriz de confusão transposta atualizada sem aspas
print(conf_table_transposed, quote = FALSE)

# Exportando os resultados da matriz de confusão por classe
write.table(conf_table_transposed, file='Tabela/fertilmatriz_K-fold.csv', sep="\t", row.names=TRUE)
write.table(conf_table_transposed, file='Tabela/esterilmatriz_K-fold.csv', sep="\t", row.names=TRUE)
write.table(conf_table_transposed, file='Tabela/combinadomatriz_K-fold.csv', sep="\t", row.names=TRUE)

#----------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) #Limpar a lista de arquivos

#LS-DA_Plotar a matriz de confusão com os dados gerados
#Chamar as matrizes para fazer as analises
#k-fold
tab = read.table('Tabela/fertilmatriz_K-fold.csv',sep="\t", as.is=T, header=T)
tab = read.table('Tabela/esterilmatriz_K-fold.csv',sep="\t", as.is=T, header=T)
tab = read.table('Tabela/combinadomatriz_K-fold.csv',sep="\t", as.is=T, header=T)

#Transformar a tabela em um formato longo
data_long <- melt(tab, id.vars = "Classes")

#Renomear as colunas
colnames(data_long) <- c("Classe_predita", "Classe_real", "Valor")

#Criar a matriz de confusão
confusion_matrix <- table(data_long$Classe_predita, data_long$Classe_real)

#Recodificar 'BalAcc.' para 'BalAcc%'
data_long <- data_long %>%
  mutate(Classe_real = recode(Classe_real, "BalAcc." = "BalAcc%")) 

#Plot com ggplot2
ggplot() +
  # Parte para o preenchimento contínuo (excluindo a coluna BalAcc%)
  geom_tile(data = filter(data_long, Classe_real != "BalAcc%" & Valor != 0), aes(x = Classe_real, y = Classe_predita, fill = Valor), color = "white") +
  scale_fill_gradient(low = "#dcdcdc", high = "#808080", guide = "none") +  # Remove a legenda de preenchimento contínuo
  
  # Adiciona uma nova escala de preenchimento para a coluna BalAcc%
  ggnewscale::new_scale_fill() +
  
  # Parte para a coluna BalAcc%
  geom_tile(data = filter(data_long, Classe_real == "BalAcc%"), aes(x = Classe_real, y = Classe_predita, fill = "highlight"), color = "white") +
  scale_fill_manual(values = c("highlight" = "#545454"), guide = "none") +  # Remove a legenda de preenchimento manual
  
  # Adiciona os textos com cores condicionais e ajusta o tamanho dos números
  geom_text(data = filter(data_long, Valor != 0), aes(x = Classe_real, y = Classe_predita, label = ifelse(Valor != 0, Valor, ""), color = ifelse(Classe_real == "BalAcc%", "highlight", "normal")), size = 5) +
  scale_color_manual(values = c("normal" = "black", "highlight" = "white"), guide = "none") +  # Remove a legenda de cores de texto
  
  # Tema e ajustes
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),  # Ajusta tamanho e centraliza texto no eixo x
        axis.text.y = element_text(size = 12),  # Ajusta tamanho do texto no eixo y
        axis.title = element_text(size = 14)) +  # Ajusta tamanho dos nomes dos eixos
  labs(x = "", y = "")              

#Salvar o gráfico como um arquivo PNG
#k-fold
ggsave("Figuras/matrizconfusao_fertil_K-fold.png", width = 7, height = 7, dpi = 300)
ggsave("Figuras/matrizconfusao_esteril_K-fold.png", width = 7, height = 7, dpi = 300)
ggsave("Figuras/matrizconfusao_combinada_K-fold.png", width = 7, height = 7, dpi = 300)
