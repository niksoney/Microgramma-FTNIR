# Carregar pacotes necessários
library(lme4)    # Para modelos mistos se necessário
library(car)     # Para testes de significância
library(emmeans) # Para comparações múltiplas
library(ggplot2) # Para visualização

citation("car")
# Script GLM adaptado para sua estrutura de dados

# 1. Verificar a distribuição dos dados
hist(medias_por_especime$Media_PCAy, 
     main = "Distribuição dos Valores de PCA no Eixo Y",
     xlab = "Média do PCA Y",
     ylab = "Frequência")

# Adicionar constante igual ao valor absoluto do mínimo + pequeno incremento
constante <- abs(min(medias_por_especime$Media_PCAy)) + 0.001
medias_por_especime$Media_PCAy_pos <- medias_por_especime$Media_PCAy + constante

# Ajustar o GLM com os dados transformados
modelo_glm <- glm(Media_PCAy_pos ~ Species * Leaf.type, 
                  data = medias_por_especime,
                  family = Gamma(link = "log"))

# 3. Diagnóstico do modelo
summary(modelo_glm)

# Extraindo os coeficientes do modelo
coef_tabela <- summary(modelo_glm)$coefficients

# Transformando em data frame
tabela_coef <- as.data.frame(coef_tabela)

# Renomeando colunas para ficar mais claro
colnames(tabela_coef) <- c("Estimate", "Std.Error", "t.value", "p.value")

# Arredondando para melhor apresentação (opcional)
tabela_coef$Estimate <- round(tabela_coef$Estimate, 5)
tabela_coef$Std.Error <- round(tabela_coef$Std.Error, 5)
tabela_coef$t.value <- round(tabela_coef$t.value, 3)
tabela_coef$p.value <- ifelse(tabela_coef$p.value < 0.0001, "<.0001", 
                              format(round(tabela_coef$p.value, 4), nsmall = 4))

# Movendo o nome dos coeficientes para uma coluna
tabela_coef$Termo <- rownames(tabela_coef)
rownames(tabela_coef) <- NULL
tabela_coef <- tabela_coef[, c("Termo", "Estimate", "Std.Error", "t.value", "p.value")]

# Teste de significância dos termos (usando car package)
library(car)
Anova(modelo_glm, type = "III")

# 4. Comparações múltiplas com emmeans
library(emmeans)

# Comparações entre espécies dentro de cada tipo de folha
pares_species_leaf <- pairs(emmeans_species_leaf)
tabela_species_leaf <- as.data.frame(pares_species_leaf)

# Arredondar e formatar os números para imitar saída do console
tabela_species_leaf$estimate <- round(tabela_species_leaf$estimate, 5)
tabela_species_leaf$SE <- round(tabela_species_leaf$SE, 3)
tabela_species_leaf$t.ratio <- round(tabela_species_leaf$t.ratio, 3)
tabela_species_leaf$p.value <- ifelse(tabela_species_leaf$p.value < 0.0001, "<.0001",
                                      format(round(tabela_species_leaf$p.value, 4), nsmall = 4))

# Garantir que 'contrast' fique na primeira coluna
tabela_species_leaf <- tabela_species_leaf[, c("contrast", "Leaf.type", "estimate", "SE", "df", "t.ratio", "p.value")]

# Salvar em Excel
library(writexl)
write_xlsx(list(Comparacao_Especie_dentro_Folha = tabela_species_leaf), "Tabela/entre especies_formatado_final.xlsx")

# Comparações entre espécies dentro de cada tipo de folha
pares_leaf_species <- pairs(emmeans_leaf_species)
tabela_leaf_species <- as.data.frame(pares_leaf_species)

# Arredondar e formatar os números para imitar saída do console
tabela_leaf_species$estimate <- round(tabela_leaf_species$estimate, 5)
tabela_leaf_species$SE <- round(tabela_leaf_species$SE, 3)
tabela_leaf_species$t.ratio <- round(tabela_leaf_species$t.ratio, 3)
tabela_leaf_species$p.value <- ifelse(tabela_leaf_species$p.value < 0.0001, "<.0001",
                                      format(round(tabela_leaf_species$p.value, 4), nsmall = 4))

# Garantir que 'contrast' fique na primeira coluna
tabela_leaf_species <- tabela_leaf_species[, c("contrast", "Species", "estimate", "SE", "df", "t.ratio", "p.value")]

# Salvar em Excel
library(writexl)
write_xlsx(list(Comparacao_Especie_dentro_Folha = tabela_species_leaf), "Tabela/pares_formatado_final.xlsx")

# 5. Visualização consistente com seu ggplot original
library(ggplot2)

# Definindo cores consistentes com seu gráfico
cores_manual <- c("Fertile leaf" = "Sienna", "Sterile leaf" = "DeepSkyBlue")
cores_boxplot <- c("#d3d3d3", "#d3d3d3")

teste = ggplot(medias_por_especime, aes(x = Species, y = Media_PCAy, fill = Leaf.type)) +
  geom_boxplot(width = 0.7, outlier.shape = 16, coef = 1.5) +
  geom_jitter(position = position_jitter(width = 0.3), 
              aes(color = Leaf.type), 
              size = 2.5, shape = 21, stroke = 0.9) +
  scale_fill_manual(values = cores_boxplot,
                    labels = c("Fertile", "Sterile"),
                    name = "Tipo de Folha") +
  scale_color_manual(values = cores_manual) +
  scale_x_discrete(limits = ordem_especies) + # Mesma ordem que seu gráfico
  labs(title = "Análise GLM - Valores de PCA Y por Espécie e Tipo de Folha",
       x = "Espécie",
       y = "Média do PCA Y",
       color = "Tipo de Folha") +
  theme_replace() +
  theme(axis.text.x = element_text(angle = 360, hjust = 1, face = "italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "top")

plot(teste)

ggsave("Figuras/testee.png", plot = teste, width = 11, height = 7, dpi= 300, units = "in")
