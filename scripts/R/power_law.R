setwd("D:/monografia/#monografia-github")
pacman::p_load(dplyr, psych, ggplot2, tidyr, DBI, RSQLite)
options(scipen=999);

db_path <- "D:/monografia/#monografia-github/database/dva.db"

# Crie uma conexão com o banco de dados
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

dados <- dbReadTable(con, "dva_igpdi")

glimpse(head(dados,5))

# Filtrar e ordenar os dados
dados_ordenados <- dados %>%
  filter(ANO == 2023) %>%
  arrange(desc(Receitas)) %>%
  mutate(Order = row_number()) %>%
  filter(Receitas > 0) # Remover valores zero ou negativos, se houver

# Ajustar uma power law aos dados
power_law_fit <- tryCatch({
  nls(Receitas ~ a * Order^b, data = dados_ordenados,
      start = list(a = max(dados_ordenados$Receitas), b = -1))
}, error = function(e) {
  print("Erro no ajuste:"); print(e)
  NULL
})

if (!is.null(power_law_fit)) {
  # Obter os parâmetros ajustados
  a <- coef(power_law_fit)["a"]
  b <- coef(power_law_fit)["b"]
  
  # Função para calcular a power law
  power_law <- function(x) {a * x^b}
  
  # Criar o gráfico de barras com a linha de ajuste power law
  ggplot(dados_ordenados, aes(x = Order, y = Receitas)) +
    geom_bar(stat = "identity", fill = "#222222") +
    stat_function(fun = power_law, color = "red", size = 1) +
    geom_text(aes(label = ifelse(Order %in% 1:5, as.character(DENOM_CIA), "")),
              hjust = 0, vjust = -0.5, color = "black", size = 3) +
    labs(title = " ",
         x = "Companhias (ordenadas pelas receitas)",
         y = "Receitas") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.title = element_text(hjust = 0.5)
    )
} else {
  print("Não foi possível ajustar uma power law aos dados.")
}
