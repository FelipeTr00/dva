pacman::p_load(dplyr)
options(sapien = 999)

dados <- read.csv2("D:/monografia/#monografia-github/dados/outros/pib_bcb.csv")
  dados$DATA <- as.Date(dados$DATA, format = "%d/%m/%Y")

dados <- filter(dados, dados$ANO >= 2009)
  
glimpse(head(dados,3))

# GRÁFICO PIB
# Encontre o valor máximo entre as três colunas
max_y <- max(max(dados$PIB_REAL), max(dados$PIB))

par(mgp = c(2, 0.5, 0))  # Ajuste os valores conforme necessário

# Crie o gráfico vazio usando plot() com o primeiro conjunto de dados
plot(dados$DATA,
     dados$PIB,
     type = "n",  # Defina type = "n" para criar um gráfico vazio
     xlab = "Ano", ylab = "Valorer (R$ Trilhões)",
     ylim = c(0, max_y),  # Definir limite do eixo y
     yaxt = "n",   # Não mostrar o eixo Y automaticamente
     xaxt = "n",  # Não mostrar o eixo X automaticamente
     cex.lab = 1.1)

# Adicione as linhas com diferentes símbolos
lines(dados$DATA, dados$PIB,
      type = "l", col = "red", lwd = 2)
lines(dados$DATA, dados$PIB_REAL,
      type = "l", col = "black", lwd = 3)      
http://127.0.0.1:15169/graphics/plot_zoom_png?width=1062&height=573
# Adicione uma legenda
legend("bottomright", 
       legend = c("PIB Real*", "PIB Nominal"), 
       col = c("black", "red"), 
       pch = c(NA, NA),  # Símbolos correspondentes
       lty = 1,
       lwd = 3,
       cex = 1.4,       # Tamanho da fonte
       xjust = 1,       # Ajusta a posição horizontal da legenda
       yjust = 0.5,     # Ajusta a posição vertical da legenda
       #inset = c(-0.2, 0) # Ajusta a posição da legenda em relação ao gráfico
)

# Obtenha anos únicos e suas posições correspondentes
unique_years <- unique(dados$ANO)
unique_positions <- dados$DATA[match(unique_years, dados$ANO)]

# Adicione os anos no eixo X, apenas os únicos
axis(1, at = unique_positions, labels = unique_years, las = 1, cex.axis = 1)

# Ajuste o formato do eixo Y para mostrar os valores divididos por 100
# e evite notação científica
y_ticks <- pretty(range(dados$PIB_REAL, dados$PIB))
y_labels <- format(y_ticks / 100000, scientific = FALSE)
axis(2, at = y_ticks, labels = y_labels, las = 1, cex.axis = 1)

# Adicione a nota explicativa abaixo do eixo X
mtext("* Valores ajustados pelo IGP-DI (Fundação Getúlio Vargas).",
      side = 1, line = 4, cex = 1.1, adj = 0, outer = FALSE)

