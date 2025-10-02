############################################################################################################################
{library(readxl) #para importar arquivo excel
  library(ggplot2) #para plotar os gráficos
  library(gridExtra) #organiza os gráficos em grade
  library(grid)} #adiciona legendas e texto geral da figura em grade
############################################################################################################################

setwd("C:/Users/nikso/OneDrive/Vida acadêmica e pessoal - Niksoney/2.Biologia Vegetal_UFPE (Mestrado_2023-2024)/_NIKSONEY AZEVEDO_mestrado/1 - ESPECTROSCOPIA - (NIR)/Diretório_NIR")

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
  labs(x = paste0("PCA1 (", sprintf("%.2f", var_explicada[1] * 100), "%)"), 
       y = paste0("PCA2 (", sprintf("%.2f", var_explicada[2] * 100), "%)")) +
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

dfFE$coluna3 <- Matriz_NIR[, 2] # Peguei a coluna que tem o nome das espécies para ficar junto com as informações de dfFF
dfFE$coluna7 <- Matriz_NIR[, 7] # Peguei a coluna que que representa os individuos para ficar junto com as informações de F
dfFE$coluna4 <- Matriz_NIR[, 4] # Peguei a coluna que que representa o dimrofismo ou não

# Carregar o pacote dplyr
library(dplyr)

medias_por_especime <- dfFE %>%
  group_by(coluna7, Leaf.type) %>%  # Agrupa apenas por espécime e tipo de folha
  summarise(
    Species = first(coluna3),       # Espécie (não afeta as médias)
    Folha_Type_Info = first(coluna4),  # Nova coluna (ex: tipo de folha adicional)
    Media_PCAx = mean(x, na.rm = TRUE),
    Media_PCAy = mean(y, na.rm = TRUE),
    .groups = 'drop'                # Remove o agrupamento após o cálculo
  )

# Visualizar o resultado
print(medias_por_especime)

#Lista de espécies únicas
species_list <- unique(medias_por_especime$Species)

#Define as cores manualmente
cores_manualdfFEindiv <- c("Sienna", "DeepSkyBlue")
cores_boxplot <- c("#d3d3d3", "#d3d3d3")

# Ordem desejada para as espécies
ordem_especies <- c("M. nana", "M. reptans", "M. tecta", "M. tobagensis", "M. percussa", "M. latevagans", "M. dictyophylla", "M. piloselloides")

# Obtém os limites do eixo y a partir do gráfico g3
y_limits <- range(medias_por_especime$Media_PCAy)  # Aqui, pegamos os valores da coluna y no dataframe dfFE

# Inicializa a lista para armazenar os gráficos
plots <- list()

# Loop sobre cada espécie na ordem desejada
for (especie_desejada in ordem_especies) {
  
  # Filtra o dataframe apenas para a espécie desejada
  species_df <- medias_por_especime[medias_por_especime$Species == especie_desejada, ]
  
  # Criando o gráfico boxplot com pontos sobrepostos
  g4 <- ggplot(species_df, aes(x = Leaf.type, y = Media_PCAy, fill = Leaf.type)) +
    geom_boxplot(width = 0.7, outlier.shape = 16, fill = cores_boxplot, coef = 1.5) +  # Inclui os whiskers para máximo e mínimo
    geom_jitter(position = position_jitter(width = 0.3), aes(color = Leaf.type), size = 2.5, shape = 21, stroke = 0.9) +  # Adiciona pontos jittered com preenchimento branco
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

# 1. Criar a grade de gráficos
grade_graficos <- arrangeGrob(grobs = plots, ncol = 4, 
                              heights = c(1, 1), 
                              widths = rep(1, 4))

# 2. Adicionar os rótulos diretamente ao grob
grade_completa <- grid.arrange(
  grade_graficos,
  left = grobTree(
    textGrob("Dimorphic",
             x = unit(1, "npc"),
             y = unit(0.75, "npc"),  # Posição mais alta (em cima)
             rot = 90,
             gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "italic")
    ),
    textGrob("Monomorphic",
             x = unit(1, "npc"),
             y = unit(0.25, "npc"),  # Posição mais baixa (embaixo)
             rot = 90,
             gp = gpar(fontsize = 14, fontface = "bold", fontfamily = "italic"))))

# 3. Visualizar antes de salvar
grid.newpage()
grid.draw(grade_completa)

# 4. Salvar a figura 
png("Figuras/TESTo.png", width = 11, height = 7, units = "in", res = 300)
grid.draw(grade_completa)
dev.off()

###################################################################
#TESTANDO SE OS DADOS SEGUEM UMA DISTRIBUIÇÃO NORMAL
###################################################################
library(dplyr)

# Criar um novo dataframe com a estrutura correta
#Converter as colunas aninhadas em vetores simples
dados_corrigidos <- medias_por_especime %>%
  mutate(
    coluna7 = coluna7$`Collector and n.`,
    Species = Species$Species,
    Folha_Type_Info = Folha_Type_Info$`Functional type of leaf`
  ) %>%
  select(-contains("tibble"))  # Remove as colunas aninhadas originais

#Verificar a nova estrutura
str(dados_corrigidos)

#Converter variáveis categóricas em fatores
dados_corrigidos <- dados_corrigidos %>%
  mutate(
    Leaf.type = as.factor(Leaf.type),
    Species = as.factor(Species),
    Folha_Type_Info = as.factor(Folha_Type_Info)
  )

#Agora rodar a ANOVA three-way corretamente
modelo <- aov(Media_PCAy ~ Leaf.type * Species * Folha_Type_Info, 
              data = dados_corrigidos)

#Ver resultados
summary(modelo)

# Normalidade dos resíduos
shapiro.test(residuals(modelo))  # p > 0.05 = OK

# Teste post-hoc de Tukey
tukey_res <- TukeyHSD(modelo)

# Visualizar resultado completo
tukey_res

# Plotar comparações (gráfico de intervalos de confiança)
plot(tukey_res, las = 1)
