
setwd("D:/monografia/_dva")
pacman::p_load(dplyr, psych, ggplot2, tidyr, DBI, RSQLite, lubridate)
options(sapien = 999)

db_path <- "D:/monografia/_dva/db/dva.db"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
dados <- dbReadTable(con, "pib_bcb")
dados$DATA <- as.Date(dados$DATA, "%d/%m/%Y")
dados <- filter(dados, ANO >= 2009)
glimpse(dados)

# GRÁFICO PIB
# Encontre o valor máximo entre as três colunas
max_y <- max(max(dados$PIB_REAL), max(dados$PIB))



par(mgp = c(2, 0.5, 0))  # Ajuste os valores conforme necessário

# Crie o gráfico vazio usando plot() com o primeiro conjunto de dados
plot(dados$DATA,
     dados$PIB,
     type = "n",  # Defina type = "n" para criar um gráfico vazio
     xlab = "Ano", ylab = "Valores (R$ Trilhões)",
     ylim = c(0, max_y),  # Definir limite do eixo y
     yaxt = "n",   # Não mostrar o eixo Y automaticamente
     xaxt = "n",  # Não mostrar o eixo X automaticamente
     cex.lab = 1.1)

# Adicione as linhas com diferentes símbolos
lines(dados$DATA, dados$PIB,
      type = "l", col = "red", lwd = 2)
lines(dados$DATA, dados$PIB_REAL,
      type = "l", col = "black", lwd = 3)      

# Adicione uma legenda
legend("bottomright", 
       legend = c("PIB Real*", "PIB Nominal"), 
       col = c("black", "red"), 
       pch = c(NA, NA),  # Símbolos correspondentes
       lty = 1,
       lwd = 3,
       cex = 1.2,       
       box.lwd = 1   
       # inset = c(-0.001, 0) # Ajusta a posição da legenda em relação ao gráfico
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

