setwd("D:/monografia/#monografia-github")
pacman::p_load(dplyr, psych, ggplot2, tidyr, DBI, RSQLite)
options(scipen=999);

db_path <- "D:/monografia/#monografia-github/database/dva.db"

# Crie uma conexão com o banco de dados
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

dados <- dbReadTable(con, "ibovespa")

# Tratamento de datas
library(lubridate)
dados$mes_ano <- as.Date(dados$mes_ano, format = "%Y-%m-%d")
# dados$mes_ano <- dados$mes_ano
glimpse(dados)

# GRÁFICO PIB
# Encontre o valor máximo entre as três colunas
max_y <- max(max(dados$valor_em_brl), max(dados$valor_igpdi_brl))

par(mgp = c(2, 0.5, 0))  # Ajuste os valores conforme necessário

# Crie o gráfico vazio usando plot() com o primeiro conjunto de dados
plot(dados$mes_ano,
     dados$valor_em_brl,
     type = "n",  # Defina type = "n" para criar um gráfico vazio
     xlab = "Ano", ylab = "Valores (R$ Trilhões)",
     ylim = c(0, max_y),  # Definir limite do eixo y
     yaxt = "n",   # Não mostrar o eixo Y automaticamente
     xaxt = "n",  # Não mostrar o eixo X automaticamente
     cex.lab = 1.1)

# Adicione as linhas com diferentes símbolos
lines(dados$mes_ano, dados$valor_em_brl,
      type = "l", col = "red", lwd = 2)
lines(dados$mes_ano, dados$valor_igpdi_brl,
      type = "l", col = "black", lwd = 3)      

# Adicione uma legenda
legend("bottomright", 
       legend = c("Ibovespa Real*", "Ibovespa Nominal"), 
       col = c("black","red"), 
       pch = c(NA, NA),  # Símbolos correspondentes
       lty = 1,
       lwd = 4,
       cex = 1.5);

# Obtenha anos únicos e suas posições correspondentes
unique_years <- unique(dados$ano)
unique_positions <- dados$mes_ano[match(unique_years, dados$ano)]

# Adicione os anos no eixo X, apenas os únicos
axis(1, at = unique_positions, labels = unique_years, las = 1, cex.axis = 1)

# Ajuste o formato do eixo Y para mostrar os valores divididos por 100
# e evite notação científica
y_ticks <- pretty(range(dados$valor_em_brl, dados$valor_em_brl))
y_labels <- format(y_ticks /1000000000000, scientific = FALSE)
axis(2, at = y_ticks, labels = y_labels, las = 1, cex.axis = 1)

# Adicione a nota explicativa abaixo do eixo X
mtext("* Valores ajustados pelo IGP-DI (Fundação Getúlio Vargas).",
      side = 1, line = 4, cex = 1.1, adj = 0)

