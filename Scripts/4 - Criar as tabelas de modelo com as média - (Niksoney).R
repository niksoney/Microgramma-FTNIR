---
#title: "CRIAR TABELAS COM DADOS MÉDIOS PARA DEPOIS REALIZAR ANÁLISES DISCRIMINANTES" - CAP 1 (DISSERTAÇÃO)
#Niksoney Azevedo Mendonça
#Email: niksoneyazevedo2017@gmail.com
#Este estudo foi realizado para o complexo de espécies scaly (8 spp.) do genêro Microgramma
---
#############################################################################################################################################
library(readxl) #para importar arquivo excel
#############################################################################################################################################
############################################FILTRANDO DADOS PARA CRIAR TABELAS###############################################################
#############################################################################################################################################

rm(list=ls()) #Limpar a lista de arquivos

#Lê os dados brutos gerados pelos outros scripts
Matriz_NIR <- read_excel("Tabela/Matriz_NIR.xlsx")
#write.table(Matriz_NIR, file='Tabela/Matriz_NIR.csv',sep="\t", row.names=T)
 
#Filtrar pelo nome de uma coluna
quecoluna <- "Leaf type" #nome da coluna cujas categorias quer filtrar
queclasse1 <- "Fertile leaf" #filtro que quer usar
queclasse2 <- "Sterile leaf"

#cria um vetor logico, ou seja TRUE/FALSE para cada linha que corresponde ou não ao filtro
vl <- Matriz_NIR[,quecoluna]==queclasse1
v2 <- Matriz_NIR[,quecoluna]==queclasse2

#filtra os dados com esse vetor de verdadeiros e falsos
dados.fertilebrutos = Matriz_NIR[vl,]
dados.sterilebrutos = Matriz_NIR[v2,]

# Combine os dados filtrados em uma única tabela
dados.combinados <- rbind(dados.fertilebrutos, dados.sterilebrutos)

#salva o dado filtrado
write.table(dados.fertilebrutos, file='Tabela/dadosfertile_brutos.csv',sep="\t", row.names=T)
write.table(dados.sterilebrutos, file='Tabela/dadossterile_brutos.csv',sep="\t", row.names=T)
write.table(dados.combinados, file='Tabela/MATRIZNIR_brutos.csv',sep="\t", row.names=T)

#############################################################################################################################################
################################TABELAS FILTRADAS PARA CRIAR TABELAS DE MÉDIAS F&E###########################################################
#############################################################################################################################################

rm(list=ls()) #Limpar a lista de arquivos

#Importar a tabela com dados brutos
dados <- read.table(file='Tabela/dadosfertile_brutos.csv',sep="\t")
dados <- read.table(file='Tabela/dadossterile_brutos.csv',sep="\t")

colunainden <- "Identifier"
colunaspeci <- "Species"
colunaspeciabr <- "Speciesabrev"
colunafunct <- "Functional.type.of.leaf"
Colunaleaft <- "Leaf.type"
colunaABAD <- "ABAD"
colunacollec <- "Collector.and.n."
colunacoutry <- "Country"
colunaherbarium <- "Herbarium" 
colunatype <- "Type.of.leaf.and.n."
colunapart <- "Part.of.the.leaf.and.n."

#Pegar colunas NIR
cls <- grep("X", colnames(dados), ignore.case = FALSE)

#Filtrar os dados que podem calcular a média
dd <- dados[, cls]

#Calcular a média por indivíduo
dt <- aggregate(dd, by = list(dados[, colunaherbarium]), FUN = mean)
colnames(dt)[1] <- colunaherbarium
rownames(dt) <- dt[, colunaherbarium]

#Pegar os dados dos indivíduos
db <- dados[, c(colunainden, colunaspeci, colunaspeciabr, colunafunct, Colunaleaft, colunaABAD, colunacollec, colunacoutry, colunaherbarium, colunatype, colunapart)] 
db <- unique(db)

#Remover duplicatas da coluna Herbarium
db_unique <- subset(db, !duplicated(db$Herbarium))

#Atribuir os nomes únicos ao dataframe
rownames(db_unique) <- db_unique$Herbarium

#Exibir o dataframe com as duplicatas removidas
head(db_unique)                     

#Unir os dataframes db_unique e dt
Tabela_dados_medio_fertile <- merge(db_unique, dt, by = "Herbarium", all.x = TRUE)
Tabela_dados_medio_sterile <- merge(db_unique, dt, by = "Herbarium", all.x = TRUE)

#Unir as duas tabelas
Tabela_dados_medio_combinado <- merge(Tabela_dados_medio_fertile, Tabela_dados_medio_sterile, all = TRUE)

write.table(Tabela_dados_medio_fertile, file='Tabela/Tabela_dados_medio_fertile.csv',sep="\t", row.names=T)
write.table(Tabela_dados_medio_sterile, file='Tabela/Tabela_dados_medio_sterile.csv',sep="\t", row.names=T)
write.table(Tabela_dados_medio_combinado, file='Tabela/Tabela_dados_medio_combinadoF&S.csv',sep="\t", row.names=T)

#############################################################################################################################################
#####################################TABELAS FILTRADAS PARA MÉDIAS DO MODELO COMBINADO#######################################################
#############################################################################################################################################

rm(list=ls()) #Limpar a lista de arquivos

dados <- read.table(file='Tabela/Tabela_dados_medio_combinadoF&S.csv',sep="\t")

colunainden <- "Identifier"
colunaspeci <- "Species"
colunaspeciabr <- "Speciesabrev"
colunafunct <- "Functional.type.of.leaf"
Colunaleaft <- "Leaf.type"
colunaABAD <- "ABAD"
colunacollec <- "Collector.and.n."
colunacoutry <- "Country"
colunaherbarium <- "Herbarium" 
colunatype <- "Type.of.leaf.and.n."
colunapart <- "Part.of.the.leaf.and.n."

#Pegar colunas NIR
cls <- grep("X", colnames(dados), ignore.case = FALSE)

#Filtrar os dados que podem calcular a média
dd <- dados[, cls]

#Calcular a média por indivíduo
dt <- aggregate(dd, by = list(dados[, colunaherbarium]), FUN = mean)
colnames(dt)[1] <- colunaherbarium
rownames(dt) <- dt[, colunaherbarium]

#Pegar os dados dos indivíduos
db <- dados[, c(colunainden, colunaspeci, colunaspeciabr, colunafunct, Colunaleaft, colunaABAD, colunacollec, colunacoutry, colunaherbarium, colunatype, colunapart)] 
db <- unique(db)

#Remover duplicatas da coluna Herbarium
db_unique <- subset(db, !duplicated(db$Herbarium))

#Atribuir os nomes únicos ao dataframe
rownames(db_unique) <- db_unique$Herbarium

#Exibir o dataframe com as duplicatas removidas
head(db_unique) 

#Unir os dataframes db_unique e dt
dados <- merge(db_unique, dt, by = "Herbarium", all.x = TRUE)

write.table(dados, file='Tabela/Tabela_dados_medio_combinadoF&S.csv',sep="\t", row.names=T)
