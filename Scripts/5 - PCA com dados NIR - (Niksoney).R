---
#title: "REALIZAR A ANÁLISE DE COMPONENTES PRINCIPAIS" - CAP 1 (DISSERTAÇÃO)
#Niksoney Azevedo Mendonça
#Email: niksoneyazevedo2017@gmail.com
#Este estudo foi realizado para o complexo de espécies scaly (8 spp.) do genêro Microgramma
---
#############################################################################################################################################
{library(readxl) #para importar arquivo excel
library(ggplot2) #para plotar os gráficos
library(gridExtra) #organiza os gráficos em grade
library(grid) #adiciona legendas e texto geral da figura em grade
library(stats) #para fazer a PCA, porém a função prcomp que fiz a PCA ja vem na configuração base do R
library(plotly)} #para identificar os pontos no gráfico
#############################################################################################################################################
######################################INICIAR AS ANÁLISES DE PCA#############################################################################
#############################################################################################################################################

#Importar tabela
#Preferir usar Excel
Matriz_NIR <- read_excel("Tabela/Matriz_NIR.xlsx")

#Seleciona as colunas numéricas (da oitava em diante)
dados_numericos <- Matriz_NIR[, 12:ncol(Matriz_NIR)]

#Converte as colunas para numérico
dados_numericos <- apply(dados_numericos, 2, as.numeric)

#Realiza a PCA apenas nos dados numéricos
pca_result <- prcomp(dados_numericos, scale. = TRUE)

#Realizar PCA
var_explicada <- summary(pca_result)$importance[2,]

#Para visualização
View(var_explicada)

#############################################################################################################################################
####################################ESSE É PARA FAZER DAS ESPÉCIES###########################################################################
#############################################################################################################################################

#Obtém as espécies únicas na primeira coluna da matriz
especies <- unique(Matriz_NIR[, 2])

#Cria um dataframe a partir da matriz
dfespecies <- data.frame(x = pca_result$x[,1], y = pca_result$x[,2], especie = Matriz_NIR[,2])

#Define as cores manualmente
cores_manualdfespecies <- c("DeepSkyBlue", "LawnGreen", "Gold", "MediumOrchid","black", "Sienna", "Tomato", "DeepPink1")

#Multiplica os valores por 100 e formata como porcentagem
var_explicada <- var_explicada * 100

g1 <- ggplot(dfespecies, aes(x = x, y = y, color = Species)) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdfespecies) +
  guides(color = guide_legend(title = NULL)) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicada[1]), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicada[2]), "%)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20, face = "italic"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.margin = margin(0, 0, 0, 10))

plot(g1)

ggsave("Figuras/Espécies_geral.png", plot = g1, width = 10, height = 12, dpi= 300, units = "in")

#--------------------------------------------------
#plotar3D

# Carrega a biblioteca plotly
library(plotly)

# Cria um dataframe para o PCA com três componentes principais
dfespecies <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  PC3 = pca_result$x[, 3],
  Species = Matriz_NIR[, 2]
)

# Define as cores manualmente
cores_manualdfespecies <- c("DeepSkyBlue", "LawnGreen", "Gold", "MediumOrchid", "black", "Sienna", "Tomato", "DeepPink1")

# Cria o gráfico 3D
plot_ly(dfespecies, x = ~PC1, y = ~PC2, z = ~PC3, color = ~ Species, colors = cores_manualdfespecies, marker = list(size = 3)) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = paste0("PCA1 (", sprintf("%.2f", var_explicada[1] * 100), "%)")),
    yaxis = list(title = paste0("PCA2 (", sprintf("%.2f", var_explicada[2] * 100), "%)")),
    zaxis = list(title = paste0("PCA3 (", sprintf("%.2f", var_explicada[3] * 100), "%)"))
  ))

#-------------------------------------------------------------------------------------------------------------------------

# Definir manualmente as cores para cada espécie
cores_manual <- c(
  "M. dictyophylla" = "DeepSkyBlue",
  "M. latevagans" = "LawnGreen",
  "M. nana" = "Gold",
  "M. percussa" = "MediumOrchid",
  "M. piloselloides" = "black",
  "M. reptans" = "Sienna",
  "M. tecta" = "Tomato",
  "M. tobagensis" = "DeepPink1"
)

# Definir limites fixos dos eixos X e Y
x_lim <- range(dfespecies$x)
y_lim <- range(dfespecies$y)

# Lista para armazenar os gráficos
lista_graficos <- list()

# Loop para criar os gráficos de dispersão sem legenda
for(sp in unique(dfespecies$Species)) {
  # Filtrar os dados para a espécie atual
  dados <- dfespecies[dfespecies$Species == sp, ]
  
  # Criar o gráfico sem legenda
  g <- ggplot(dados, aes(x = x, y = y)) +
    geom_point(aes(color = Species), shape = 20, size = 2) +
    scale_color_manual(values = cores_manual) +
    scale_x_continuous(limits = x_lim) +
    scale_y_continuous(limits = y_lim) +
    labs(x = NULL, y = NULL) +  # Remover os títulos dos eixos
    theme_re() +           # Usar um tema minimalista
    theme(legend.position = "none")  # Remover a legenda
  
  # Armazenar o gráfico na lista
  lista_graficos[[sp]] <- g
}

# Exibir os gráficos em uma grade com 4 colunas
multiplot <- do.call(grid.arrange, c(lista_graficos, ncol = 4))
print(multiplot)

ggsave("Figuras/Espéciesemduplas.png", plot = multiplot, width = 11, height = 7, dpi= 300, units = "in")

#############################################################################################################################################
#############################IDENTIFICAR QUAIS SÃO MEUS PONTOS NO GRÁFICO####################################################################
#############################################################################################################################################

#Adicionar informações textuais aos pontos
pca_resul_df <- as.data.frame(pca_result$x)
pca_resul_df$Species <- Matriz_NIR$Species
pca_resul_df$Identifier <- Matriz_NIR$Identifier

#Criar o gráfico ggplot com a camada de texto
g1 <- ggplot(data = pca_resul_df, aes(x = PC1, y = PC2, color = Species, text = paste("Identifier:", Identifier))) +
  geom_point(shape = 20, size = 1) +
  scale_color_manual(values = cores_manualdfespecies) +
  theme(legend.position = "none",
        legend.text = element_text(size = 20),  
        axis.title = element_text(size = 20),   
        legend.margin = margin(0, 0, 0, 10))

#Converter para plotly e configurar a exibição do tooltip
ggplotly(g1, tooltip = "text") %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick", opacityDim = 0.3)

#############################################################################################################################################
##########################ESSE É PARA FAZER DOS TIPOS FUNCIONAIS DE FOLHA D&M################################################################
#############################################################################################################################################

#Obtém os tipos funcionais na segunda coluna da matriz
função <- unique(Matriz_NIR[, 4])

#Cria um dataframe a partir da matriz
dffunção <- data.frame(x = pca_result$x[,1], y = pca_result$x[,2], especie = Matriz_NIR[,4])

#Define as cores manualmente
cores_manualdffunção <- c("darkslategray", "coral")

#Cria o gráfico
g2 <- ggplot(dffunção, aes(x = x, y = y, color = Functional.type.of.leaf)) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdffunção) +
  guides(color = guide_legend(title = NULL)) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicada[1]), "%)"), 
      y = paste0("PCA2 (", sprintf("%.2f", var_explicada[2]), "%)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20),  # Define o tamanho da fonte da legenda
        axis.title = element_text(size = 20),   # Define o tamanho da fonte dos rótulos dos eixos
        axis.text = element_text(size = 14),    # Define o tamanho dos números das margens 
        legend.margin = margin(0, 0, 0, 10))    # Define a margem da legenda

plot(g2)

ggsave("Figuras/Folhas_dimorfas_monomorfas.png", plot = g2, width = 10, height = 12, dpi= 300, units = "in")

#############################################################################################################################################
##################ESSE É PARA FAZER DAS FOLHAS FERTEIS E ESTEREIS INDIVIDUALMENTE############################################################
#############################################################################################################################################

#Realizar a PCA apenas nos dados numéricos
pca_sterile <- prcomp(dados_numericos[Matriz_NIR$`Leaf type` == "Sterile leaf",], scale. = TRUE)
pca_fertile <- prcomp(dados_numericos[Matriz_NIR$`Leaf type` == "Fertile leaf",], scale. = TRUE)

var_explicativasterile <- summary(pca_sterile)$importance[2,] #Para fazer a PCA sterile
var_explicativafertile <- summary(pca_fertile)$importance[2,]

View(var_explicativasterile) #Para ver a variação especifica da PCA sterile
View(var_explicativafertile) #Para ver a variação especifica das PCA Fertile

#Plotar os resultados
cores_manualdfespecies <- c("DeepSkyBlue", "LawnGreen", "Gold", "MediumOrchid","black", "Sienna", "Tomato", "DeepPink1")

#Multiplica os valores por 100 e formata como porcentagem
var_explicativa_sterile <- var_explicativasterile * 100

# Determinar limites comuns para PC1 e PC2
x_lim <- range(c(pca_sterile$x[, "PC1"], pca_fertile$x[, "PC1"]))
y_lim <- range(c(pca_sterile$x[, "PC2"], pca_fertile$x[, "PC2"]))

# Gráfico para folhas estéreis
g5 <- ggplot(data = as.data.frame(pca_sterile$x), 
             aes(x = PC1, y = PC2, color = Matriz_NIR$Species[Matriz_NIR$`Leaf type` == "Sterile leaf"])) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdfespecies) +
  guides(color = guide_legend(title = NULL)) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicativa_sterile[1]), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicativa_sterile[2]), "%)"),
       title = "Sterile leaves") +
  xlim(x_lim) + ylim(y_lim) +
  theme(axis.text = element_text(size = 10),    
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 20),  
        legend.position = c(0.09, 0.75),  
        legend.background = element_blank(),  
        legend.text = element_text(size = 12, face = "italic", margin = margin(t = 5, r = 5, b = 5, l = 5)),  
        legend.key.height = unit(1.5, "lines"),  
        legend.key.width = unit(1.5, "lines")) +
  stat_ellipse(level = 0.95, linetype = "solid", size = 1)

# Print the plot
plot(g5)

#Multiplica os valores por 100 e formata como porcentagem
var_explicativa_fertile <- var_explicativafertile * 100

# Gráfico para folhas férteis
g6 <- ggplot(data = as.data.frame(pca_fertile$x), 
             aes(x = PC1, y = PC2, color = Matriz_NIR$Species[Matriz_NIR$`Leaf type` == "Fertile leaf"])) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdfespecies) +
  guides(color = guide_legend(title = NULL)) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicativa_fertile[1]), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicativa_fertile[2]), "%)"),
       title = "Fertile leaves") +
  xlim(x_lim) + ylim(y_lim) +
  theme(axis.text = element_text(size = 10),    
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        plot.title = element_text(hjust = 0.5, size = 20),  
        legend.position = c(0.09, 0.25),  
        legend.background = element_blank(),  
        legend.text = element_text(size = 12, face = "italic", margin = margin(t = 5, r = 5, b = 5, l = 5)),  
        legend.key.height = unit(1.5, "lines"),  
        legend.key.width = unit(1.5, "lines")) +
  stat_ellipse(level = 0.95, linetype = "solid", size = 1)

plot(g6)

ggsave("Figuras/Folhas_estereis.png", plot = g5, width = 11, height = 7, dpi= 300, units = "in")
ggsave("Figuras/Folhas_ferteis.png", plot = g6, width = 11, height = 7, dpi= 300, units = "in")

#############################################################################################################################################
########################ESSE É PARA FAZER DAS FOLHAS FERTEIS E ESTEREIS jUNTAS###############################################################
#############################################################################################################################################

#Obtém se as folhas são ferteis ou estereis na terceira coluna da matriz
FE <- unique(Matriz_NIR[, 5])

#Cria um dataframe a partir da matriz
dfFE <- data.frame(x = pca_result$x[,1], y = pca_result$x[,2], especie = Matriz_NIR[,5])

#Define as cores manualmente
cores_manualdfFE <- c("Sienna", "DeepSkyBlue")

#Cria o gráfico
g3 <- ggplot(dfFE, aes(x = x, y = y, color = Leaf.type)) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdfFE) +
  guides(color = guide_legend(title = NULL)) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicada[1]), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicada[2]), "%)")) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),  # Define o tamanho da fonte da legenda
        axis.title = element_text(size = 18),   # Define o tamanho da fonte dos rótulos dos eixos
        axis.text = element_text(size = 14),
        legend.margin = margin(0, 0, 0, 10))    # Define a margem da legenda

plot(g3)

ggsave("Figuras/Folhas_ferteis_estereis_juntas.png", plot = g3, width = 11, height = 7, dpi= 300, units = "in")

#############################################################################################################################################
##############ESSE É PARA FAZER DAS FOLHA FERTEIS E ESTEREIS INDIVIDUALMENTE POR ESPÉCIE#####################################################
#############################################################################################################################################

dfFE$coluna3 <- Matriz_NIR[, 2] # Peguei a coluna que tem o nome das espécies para ficar junto com as informações de F

#Lista de espécies únicas
species_list <- unique(dfFE$coluna3$Species)

#Define as cores manualmente
cores_manualdfFEindiv <- c("Sienna", "DeepSkyBlue")
cores_boxplot <- c("#d3d3d3", "#d3d3d3")

# Ordem desejada para as espécies
ordem_especies <- c("M. nana", "M. reptans", "M. tecta", "M. tobagensis", "M. percussa", "M. latevagans", "M. dictyophylla", "M. piloselloides")

# Obtém os limites do eixo y a partir do gráfico g3
y_limits <- range(dfFE$y)  # Aqui, pegamos os valores da coluna y no dataframe dfFE

# Inicializa a lista para armazenar os gráficos
plots <- list()

# Loop sobre cada espécie na ordem desejada
for (especie_desejada in ordem_especies) {
  
  # Filtra o dataframe apenas para a espécie desejada
  species_df <- dfFE[dfFE$coluna3$Species == especie_desejada, ]
  
  # Criando o gráfico boxplot com pontos sobrepostos
  g4 <- ggplot(species_df, aes(x = Leaf.type, y = y, fill = Leaf.type)) +
    geom_boxplot(width = 0.7, outlier.shape = 16, fill = cores_boxplot, coef = 1.5) +  # Inclui os whiskers para máximo e mínimo
    geom_jitter(position = position_jitter(width = 0.3), aes(color = Leaf.type), size = 1, shape = 21, fill = "white", stroke = 0.9) +  # Adiciona pontos jittered com preenchimento branco
    scale_fill_manual(values = cores_manualdfFEindiv) +  # Ajusta as cores conforme necessário
    scale_color_manual(values = cores_manualdfFEindiv) +  # Ajusta as cores dos pontos conforme necessário
    scale_x_discrete(labels = c("Fertile leaf" = "Fertile", "Sterile leaf" = "Sterile")) +  # Ajusta os rótulos no eixo x
    ggtitle(paste("", especie_desejada)) +
    scale_y_continuous(limits = y_limits) +  # Ajusta os limites do eixo y para os mesmos valores de g3
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "italic"),  # Título centralizado
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14),
          legend.position = "none",  # Remove a legenda
          plot.margin = margin(10, 10, 10, 10, "pt"))  # Ajusta as margens internas do gráfico
  
  # Adiciona o gráfico à lista
  plots[[length(plots) + 1]] <- g4
}

# Organiza os gráficos em uma grade, ajustando o espaço entre eles
g4 <- grid.arrange(grobs = plots, ncol = 4, heights = c(1, 1), widths = rep(1, 4))

# Exibe a grade de gráficos ajustada
print(g4)

#####################################################################################
#####################################################################################

library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(coin)  # Para o teste de permutação

# Seus dados e configurações iniciais
dfFE$coluna3 <- Matriz_NIR[, 2]
species_list <- unique(dfFE$coluna3$Species)
cores_manualdfFEindiv <- c("Sienna", "DeepSkyBlue")
cores_boxplot <- c("#d3d3d3", "#d3d3d3")
ordem_especies <- c("M. nana", "M. reptans", "M. tecta", "M. tobagensis", 
                    "M. percussa", "M. latevagans", "M. dictyophylla", "M. piloselloides")
y_limits <- range(dfFE$y)

# Converter Leaf.type para fator
dfFE$Leaf.type <- as.factor(dfFE$Leaf.type)

# Lista para armazenar resultados estatísticos
resultados_perm <- list()

# Loop sobre cada espécie
plots <- list()
for (especie_desejada in ordem_especies) {
  species_df <- dfFE[dfFE$coluna3$Species == especie_desejada, ]
  
  # Verifica se há dados suficientes (mínimo 2 amostras por grupo)
  n_fertile <- sum(species_df$Leaf.type == "Fertile leaf")
  n_sterile <- sum(species_df$Leaf.type == "Sterile leaf")
  
  # Cria o gráfico base
  g4 <- ggplot(species_df, aes(x = Leaf.type, y = y, fill = Leaf.type)) +
    geom_boxplot(width = 0.7, outlier.shape = 16, fill = cores_boxplot, coef = 1.5) +
    geom_jitter(position = position_jitter(width = 0.3), 
                aes(color = Leaf.type), size = 1, shape = 21, fill = "white", stroke = 0.9) +
    scale_fill_manual(values = cores_manualdfFEindiv) +
    scale_color_manual(values = cores_manualdfFEindiv) +
    scale_x_discrete(labels = c("Fertile leaf" = "Fertile", "Sterile leaf" = "Sterile")) +
    ggtitle(paste("", especie_desejada)) +
    scale_y_continuous(limits = y_limits) +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "italic"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14),
          legend.position = "none",
          plot.margin = margin(10, 10, 10, 10, "pt"))
  
  # Adiciona o teste estatístico se houver amostras suficientes
  if (n_fertile >= 2 & n_sterile >= 2) {
    # Executa o teste de permutação
    # Garante que Leaf.type seja fator no subset também
    species_df$Leaf.type <- as.factor(species_df$Leaf.type)
    
    # Teste de permutação com tratamento de possíveis valores missing
    species_df <- species_df[complete.cases(species_df$y, species_df$Leaf.type), ]
    
    teste_perm <- independence_test(y ~ Leaf.type, data = species_df,
                                   distribution = approximate(nresample = 10000))
    
    # Armazena os resultados
    resultados_perm[[especie_desejada]] <- list(
      p_value = pvalue(teste_perm)[1],
      statistic = statistic(teste_perm),
      n_fertile = n_fertile,
      n_sterile = n_sterile
    )
    
    # Adiciona o valor p ao gráfico (formato personalizado)
    p_valor <- round(pvalue(teste_perm)[1], 4)
    g4 <- g4 + annotate("text", x = 1.5, y = max(y_limits) * 0.95,
                        label = ifelse(p_valor < 0.0001, "p < 0.0001", 
                                      paste("p =", format(p_valor, scientific = FALSE))),
                        size = 5)
    
  } else {
    resultados_perm[[especie_desejada]] <- list(
      p_value = NA,
      message = paste("Amostras insuficientes (Fértil:", n_fertile, "Estéril:", n_sterile, ")")
    )
    
    g4 <- g4 + annotate("text", x = 1.5, y = max(y_limits) * 0.95,
                        label = "Dados insuficientes", size = 4)
  }
  
  plots[[length(plots) + 1]] <- g4
}

# Organiza e exibe os gráficos
g_final <- grid.arrange(grobs = plots, ncol = 4, heights = c(1, 1), widths = rep(1, 4))
print(g_final)

# Tabela de resultados completos
tabela_resultados <- do.call(rbind, lapply(resultados_perm, function(x) {
  data.frame(
    Especie = names(x)[1],
    p_value = x$p_value,
    Estatistica = ifelse(is.null(x$statistic), NA, x$statistic),
    n_Fertile = x$n_fertile,
    n_Sterile = x$n_sterile
  )
}))
print(tabela_resultados)

#Adiciona rótulos gerais
grid.text("PCA1 (81.21%)", x = 0.5, y = 0.014, gp = gpar(fontsize = 18))  #Aumenta o tamanho da fonte
grid.text("PCA2 (18.17%)", x = 0.007, y = 0.5, gp = gpar(fontsize = 18), rot = 90)

ggsave("Figuras/Folhas_ferteis_estereis_por_espéciecomoPP.png", plot = g4, width = 11, height = 7, dpi= 300, units = "in")












# Carregar bibliotecas necessárias
library(ggplot2)
library(gridExtra)
library(coin)

# Seus dados e configurações iniciais
dfFE$coluna3 <- Matriz_NIR[, 2]
species_list <- unique(dfFE$coluna3$Species)
cores_manualdfFEindiv <- c("Sienna", "DeepSkyBlue")
cores_boxplot <- c("#d3d3d3", "#d3d3d3")
ordem_especies <- c("M. nana", "M. reptans", "M. tecta", "M. tobagensis", 
                    "M. percussa", "M. latevagans", "M. dictyophylla", "M. piloselloides")
y_limits <- range(dfFE$y)

# Converter Leaf.type para fator
dfFE$Leaf.type <- as.factor(dfFE$Leaf.type)

# Listas para armazenar resultados
plots <- list()
resultados_completos <- list()

# Loop sobre cada espécie
for (especie_desejada in ordem_especies) {
  species_df <- dfFE[dfFE$coluna3$Species == especie_desejada, ]
  
  # Verifica se há dados suficientes
  n_fertile <- sum(species_df$Leaf.type == "Fertile leaf")
  n_sterile <- sum(species_df$Leaf.type == "Sterile leaf")
  
  # Cria o gráfico base
  g4 <- ggplot(species_df, aes(x = Leaf.type, y = y, fill = Leaf.type)) +
    geom_boxplot(width = 0.7, outlier.shape = 16, fill = cores_boxplot, coef = 1.5) +
    geom_jitter(position = position_jitter(width = 0.3), 
                aes(color = Leaf.type), size = 1, shape = 21, fill = "white", stroke = 0.9) +
    scale_fill_manual(values = cores_manualdfFEindiv) +
    scale_color_manual(values = cores_manualdfFEindiv) +
    scale_x_discrete(labels = c("Fertile leaf" = "Fertile", "Sterile leaf" = "Sterile")) +
    ggtitle(paste("", especie_desejada)) +
    scale_y_continuous(limits = y_limits) +
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "italic"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_text(size = 14),
          legend.position = "none",
          plot.margin = margin(10, 10, 10, 10, "pt"))
  
  # Processamento estatístico se houver amostras suficientes
  if (n_fertile >= 2 & n_sterile >= 2) {
    # Garante que Leaf.type seja fator no subset
    species_df$Leaf.type <- as.factor(species_df$Leaf.type)
    species_df <- species_df[complete.cases(species_df$y, species_df$Leaf.type), ]
    
    # Teste de permutação
    teste_perm <- independence_test(y ~ Leaf.type, data = species_df,
                                    distribution = approximate(nresample = 10000))
    p_valor <- round(pvalue(teste_perm)[1], 4)
    
    # Cálculo do Hedge's g
    fertile_data <- species_df$y[species_df$Leaf.type == "Fertile leaf"]
    sterile_data <- species_df$y[species_df$Leaf.type == "Sterile leaf"]
    
    mean_fertile <- mean(fertile_data)
    mean_sterile <- mean(sterile_data)
    sd_fertile <- sd(fertile_data)
    sd_sterile <- sd(sterile_data)
    
    n1 <- length(fertile_data)
    n2 <- length(sterile_data)
    diff_means <- mean_fertile - mean_sterile
    pooled_sd <- sqrt(((n1-1)*sd_fertile^2 + (n2-1)*sd_sterile^2)/(n1+n2-2))
    
    # Correção de viés para pequenas amostras
    df <- n1 + n2 - 2
    J <- 1 - 3/(4*df - 1)
    hedges_g <- J * (diff_means / pooled_sd)
    
    # Intervalo de confiança
    se_g <- sqrt((n1+n2)/(n1*n2) + (hedges_g^2)/(2*(n1+n2)))
    ci_lower <- hedges_g - 1.96 * se_g
    ci_upper <- hedges_g + 1.96 * se_g
    
    # Armazena os resultados
    resultados_completos[[especie_desejada]] <- list(
      p_value = p_valor,
      statistic = statistic(teste_perm),
      hedges_g = hedges_g,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      mean_fertile = mean_fertile,
      mean_sterile = mean_sterile,
      n_fertile = n_fertile,
      n_sterile = n_sterile
    )
    
    # Adiciona anotações ao gráfico
    g4 <- g4 + 
      annotate("text", x = 1.5, y = max(y_limits) * 0.95,
               label = ifelse(p_valor < 0.0001, "p < 0.0001", 
                              paste("p =", format(p_valor, scientific = FALSE))),
               size = 5) +
      annotate("text", x = 1.5, y = max(y_limits) * 0.85,
               label = sprintf("g = %.2f [%.2f, %.2f]", hedges_g, ci_lower, ci_upper),
               size = 4)
    
  } else {
    resultados_completos[[especie_desejada]] <- list(
      p_value = NA,
      hedges_g = NA,
      message = paste("Amostras insuficientes (Fértil:", n_fertile, "Estéril:", n_sterile, ")")
    )
    
    g4 <- g4 + annotate("text", x = 1.5, y = max(y_limits) * 0.95,
                        label = "Dados insuficientes", size = 4)
  }
  
  plots[[length(plots) + 1]] <- g4
}

# Organiza e exibe os gráficos
g_final <- grid.arrange(grobs = plots, ncol = 4, heights = c(1, 1), widths = rep(1, 4))
print(g_final)

# Cria e exibe tabela de resultados completos
tabela_final <- do.call(rbind, lapply(names(resultados_completos), function(especie) {
  res <- resultados_completos[[especie]]
  data.frame(
    Espécie = especie,
    p_value = ifelse(is.na(res$p_value), NA, format(res$p_value, scientific = FALSE)),
    Estatística = ifelse(is.null(res$statistic), NA, round(res$statistic, 3)),
    Hedge_g = ifelse(is.na(res$hedges_g), NA, round(res$hedges_g, 3)),
    IC_95 = ifelse(is.na(res$hedges_g), NA, 
                   paste0("[", round(res$ci_lower, 2), ", ", round(res$ci_upper, 2), "]")),
    Média_Fértil = ifelse(is.null(res$mean_fertile), NA, round(res$mean_fertile, 2)),
    Média_Estéril = ifelse(is.null(res$mean_sterile), NA, round(res$mean_sterile, 2)),
    N_Fértil = res$n_fertile,
    N_Estéril = res$n_sterile
  )
}))

# Imprime a tabela formatada
print(tabela_final)

# Interpretação dos resultados
cat("\nINTERPRETAÇÃO DOS RESULTADOS:\n")
cat("1. Valor p: probabilidade de observar diferenças tão extremas sob a hipótese nula\n")
cat("2. Hedge's g: tamanho do efeito padronizado (valores absolutos):\n")
cat("   - ~0.2: efeito pequeno\n   - ~0.5: efeito médio\n   - ~0.8: efeito grande\n")
cat("3. Intervalo de confiança: se inclui zero, o efeito pode não ser significativo\n")
cat("4. Comparação entre espécies: valores maiores indicam diferenças morfológicas mais acentuadas\n")








#############################################################################################################################################
####################ESSE É PARA FAZER DA PARTE ABAXIAL E ADAXIAL PARA AS ESPECIES############################################################
#############################################################################################################################################

# Realizar a PCA apenas nos dados numéricos
pca_abaxial <- prcomp(dados_numericos[Matriz_NIR$ABAD == "Abaxial",], scale. = TRUE)
pca_adaxial <- prcomp(dados_numericos[Matriz_NIR$ABAD == "Adaxial",], scale. = TRUE)

var_explicativa_abaxial <- summary(pca_abaxial)$importance[2,]
var_explicativa_adaxial <- summary(pca_adaxial)$importance[2,]

View(var_explicativa_abaxial) #Para ver a variação especifica da PCA abaxial
View(var_explicativa_adaxial)#Para ver a variação especifica das PCA adaxial

#Plotar os resultados
cores_manualdfespecies <- c("DeepSkyBlue", "LawnGreen", "Gold", "MediumOrchid","black", "Sienna", "Tomato", "DeepPink1")

#Multiplica os valores por 100 e formata como porcentagem
var_explicada_percentabaxial <- var_explicativa_abaxial * 100

g7 <- ggplot(data = as.data.frame(pca_abaxial$x), aes(x = PC1, y = PC2, color = Matriz_NIR$Species[Matriz_NIR$ABAD == "Abaxial"])) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdfespecies) +
  guides(color = FALSE) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicada_percentabaxial[1]), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicada_percentabaxial[2]), "%)")) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20), 
        legend.margin = margin(0, 0, 0, 10))

#Multiplica os valores por 100 e formata como porcentagem
var_explicada_percentadaxial <- var_explicativa_adaxial * 100

g8 <- ggplot(data = as.data.frame(pca_adaxial$x), aes(x = PC1, y = PC2, color = Matriz_NIR$Species[Matriz_NIR$ABAD == "Adaxial"])) +
  geom_point(shape = 20, size = 4) +
  scale_color_manual(values = cores_manualdfespecies) +
  guides(color = FALSE) +
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicada_percentadaxial[1]), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicada_percentadaxial[2]), "%)")) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 20), 
        legend.margin = margin(0, 0, 0, 10))

theme

plot(g7)
plot(g8)

ggsave("Figuras/Folhas_abaxiais.png", plot = g7, width = 10, height = 12, dpi= 300, units = "in")
ggsave("Figuras/Folhas_adaxiais.png", plot = g8, width = 10, height = 12, dpi= 300, units = "in")

####################################################
parte1 <- (g1 + multiplot)
ggsave("parte1.png", plot = parte1, width = 24, height = 30, units = "in")
print(parte1)

parte2 <- (g5 + g6)
ggsave("parte2.png", plot = parte2, width = 24, height = 30, units = "in")
print(parte2)
####################################################

grid.arrange(g3, g4, ncol = 1, nrow = 2)



