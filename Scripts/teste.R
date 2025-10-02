# Carregar pacote
library(ggplot2)

# Criar dados simples
dados <- data.frame(
  x = 1:10,
  y = c(2, 4, 3, 6, 7, 8, 7, 9, 12, 15)
)

# Fazer gráfico
ggplot(dados, aes(x = x, y = y)) +
  geom_point(color = "blue", size = 3) +
  labs(
    title = "Gráfico simples em R",
    x = "Eixo X",
    y = "Eixo Y"
  )
