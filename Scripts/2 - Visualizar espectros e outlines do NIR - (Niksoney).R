---
#title: "VISUALIZAR O GRAFICO DE ESPECTROS E VERIFICAR POSSÍVEIS OUTLINES" - CAP 1 (DISSERTAÇÃO)
#Niksoney Azevedo Mendonça
#Email: niksoneyazevedo2017@gmail.com
#Este estudo foi realizado para o complexo de espécies scaly (8 spp.) do genêro Microgramma 
---
#############################################################################################################################################
library(readxl)
library(ggplot2)
library(dplyr)
#############################################################################################################################################
####################################VISUALIZAR OS ESPECTROS MÉDIO DAS ESPÉCIE################################################################
#############################################################################################################################################

rm(list=ls()) #Limpar a lista de arquivos

#Importar a tabela média dos dados NIR
Matriz_NIR <- read.table(file='Tabela/Matriz_NIR.csv', sep="\t")

#Especificar a categoria para agrupamento
categoria <- "Species"

#Identificar as colunas com os dados
cls <- grep("X", colnames(Matriz_NIR), ignore.case = FALSE)

#Selecionar apenas as colunas com os dados
dd <- Matriz_NIR[, cls]

#Calcular a média dos dados por categoria
dt <- aggregate(dd, by = list(Matriz_NIR[, categoria]), FUN = mean)

#Extrair os números das colunas para o eixo x
xx <- colnames(dt[, -1])
xx <- as.numeric(gsub("X", "", xx))

#Cores e tipos de linha para cada espécie
cores_manualdfespecies <- c("DeepSkyBlue", "LawnGreen", "Gold", "MediumOrchid","black", "Sienna", "Tomato", "DeepPink1")
tipos_linha <- c(1,1,1,1,1,1,1,1)  # Diferentes tipos de linha

#Criar um dataframe para usar com ggplot
dados_plot <- data.frame(x = rep(xx, nrow(dt)), y = as.vector(t(dt[, -1])), group = rep(dt[, 1], each = length(xx)))

# Plotar com ggplot2
ggplot(dados_plot, aes(x = x, y = y)) +
  geom_line(na.rm = FALSE) +
  scale_color_manual(values = cores_manualdfespecies, name = NULL) +
  scale_y_continuous(limits = c(0.2, 0.9), breaks = seq(0.2, 0.8, by = 0.2)) +
  scale_x_continuous(limits = c(4000, 8000), breaks = seq(4000, 8000, by = 1000)) +
  scale_linetype_manual(values = tipos_linha, name = NULL) +
  labs(x = expression('Wavenumber (cm'^-1*')'), y = 'Absorbance') +
  theme_replace() +
  theme(legend.position = c(0.90, 0.72),
        legend.text = element_text(size = 10))

print(p1)



ggsave("Figuras/Gráficosespectros_médio.png", plot = p1, width = 7, height = 4, dpi= 300, units = "in")

#############################################################################################################################################
######################################VISUALIZAR OS ESPECTROS POR##############################################################
#############################################################################################################################################

rm(list=ls()) #Limpar a lista de arquivos

Matriz_NIR <- read.table(file='Tabela/Matriz_NIR.csv',sep="\t")

categoria = "Herbarium" #nome da coluna que identifica individuos
unique(Matriz_NIR$Herbarium)
leaf = 'Leaf.type' #nome da coluna com categoria para colorir espectros
#leitura = "leitura" # no caso das folhas

#pega colunas NIR
cls = grep("X",colnames(Matriz_NIR),ignore.case=F)

#individuos unicos
indv = Matriz_NIR[, categoria]
uindv = unique(indv)

cat = Matriz_NIR[, leaf] # Casca ou leitura p/ folhas
#cores = rainbow(n=7) # Para folha (7 foi o máx das leituras)
cores = c("red","blue") # Para casca morta e viva
cat.cor = cores[as.numeric(as.factor(cat))]
names(cat.cor) = cat

#gera um pdf
pdf(file = 'Figuras/espectros_por_individuo.pdf', width = 8.5, height = 10)
#divide a página em tres linhas e duas colunas (seis graficos por pagina)
par(mfrow=c(2,2))

#para cada individuo unico plota os spectros colorindo morta e viva diferentemente
u = 1
for (u in 1:length(uindv)) {
  # Filtra os dados para o indivíduo
  d = Matriz_NIR[indv == uindv[u], cls]
  
  faccore = cat.cor[indv == uindv[u]]
  
  # Pega eixo x do nome das variáveis NIR	
  xx = colnames(d)
  xx = gsub("X", "", xx)
  xx = as.numeric(xx)
  
  # Define a faixa desejada de reflectância
  yl = c(0.2, 0.9)
  
  # Plot uma figura vazia
  plot(xx, d[1,], type = 'n', xlab = 'Wavelength (nm)', ylab = 'Reflectance', ylim = yl)
  
  # Plot cada espectro
  for (n in 1:nrow(d)) {
    y = d[n,]
    # Ajusta os valores de reflectância para a faixa desejada
    y_adj = pmin(pmax(y, 0.2), 0.8)
    points(xx, y_adj, type = 'l', col = faccore[n])
  }
  
  legend("topleft", legend = uindv[u], bty = 'n', inset = 0.0)
  legend("topright", legend = unique(names(cat.cor)), bty = 'n', inset = 0.0, lwd = 2, col = unique(cat.cor))
}
dev.off()
