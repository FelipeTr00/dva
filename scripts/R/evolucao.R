# Carregar os pacotes necessários
pacman::p_load(dplyr, tidyr, DBI, RSQLite, ggplot2)
# Conectar ao banco de dados SQLite
db <- "D:/monografia/_dva/db/dva.db"
con <- dbConnect(RSQLite::SQLite(), db)

# Definir a consulta SQL
query <- "
SELECT
    
    ANO,
    (SUM(Pessoal) / SUM(VATD)) * 100 AS Pessoal,
    (SUM(Governo) / SUM(VATD)) * 100 AS Governo,
    (SUM(RCT) / SUM(VATD)) * 100 AS Credores,
    (SUM(RCP) / SUM(VATD)) * 100 AS Acionistas

FROM dva_igpdi
WHERE SETOR_ATIV IS NOT NULL
GROUP BY ANO;
"

query2 <- "
SELECT
    
    ANO,
    SUM(Pessoal) * 0.000000001 Pessoal,
    SUM(Governo) * 0.000000001 Governo,
    SUM(RCT) * 0.000000001 Credores,
    SUM(RCP) * 0.000000001 Acionistas

FROM dva_contas
WHERE SETOR_ATIV IS NOT NULL
GROUP BY ANO;

"

# Executar a consulta SQL e coletar os resultados
df <- dbGetQuery(con, query)

# Exibir o resultado
glimpse(head(df,1))

# View(df)

#01  $ ANO        <int> 
#02  $ Pessoal    <dbl> 
#03  $ Governo    <dbl> 
#04  $ Credores   <dbl> 
#05  $ Acionistas <dbl>

# View(dados)
# Plotar o gráfico

# X-axis variable
# x <- df$ANO

# Variables to be stacked
# y <- df[, c(2, 3, 4, 5)]

# library(areaplot)
# Percentage stacked area chart
# areaplot(x, y, prop = TRUE)


# V.2

df_long <- df %>%
  pivot_longer(cols = -ANO, names_to = "Categoria", values_to = "Valor")

# Ajustar a ordem das categorias para que a ordem original seja invertida
df_long$Categoria <- factor(df_long$Categoria, levels = rev(c("Pessoal", "Governo", "Credores", "Acionistas")))

# Criar o gráfico de área empilhada com bordas e preenchimento ajustados
ggplot(df_long, aes(x = ANO, y = Valor, fill = Categoria)) +
  geom_area(position = "stack", color = "#e5e5e5", linewidth = 0.10) + # Define a cor da borda e a largura
  scale_fill_manual(values = rev(c("Pessoal" = "#343d45", "Governo" = "#4f5b66", "Credores" = "#9a9fab", "Acionistas" = "#dfdfdf"))) +
  labs(x = "Anos", y = "Valores (%)", fill = "Legenda:") + # Altera o título da legenda
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5), # Define a posição dos textos do eixo x para horizontal
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    panel.grid = element_blank(), # Remove as linhas de grade
    panel.border = element_rect(color = "black", fill = NA) # Adiciona uma borda ao redor do gráfico
  ) +
  scale_x_continuous(breaks = seq(min(df$ANO), max(df$ANO), by = 1)) # Mostra todos os anos no eixo x

