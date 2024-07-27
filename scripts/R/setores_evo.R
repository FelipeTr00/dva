# Carregar os pacotes necessários
pacman::p_load(dplyr, tidyr, DBI, RSQLite, ggplot2, ggpattern, tidyverse)
# Conectar ao banco de dados SQLite
db <- "D:/monografia/_dva/db/dva.db"
con <- dbConnect(RSQLite::SQLite(), db)

# Definir a consulta SQL
query <- "
SELECT 
    d.ANO,
    s.SETOR,
    ROUND(SUM(d.VATD)) * 0.0000001 VATD
    
FROM dva_igpdi d
RIGHT JOIN setores s
ON s.SETOR_ATIV = d.SETOR_ATIV

GROUP BY ANO, SETOR;

"

query2 <- "

SELECT 
    d.ANO,
    s.SETOR,
    --ROUND(SUM(d.VATD) * 0.0000001, 2) AS VATD,
    ROUND((SUM(d.VATD) * 0.0000001) / total_vatd_ano * 100, 2) AS VATD_PERCENTUAL
FROM 
    dva_igpdi d
RIGHT JOIN 
    setores s ON s.SETOR_ATIV = d.SETOR_ATIV
LEFT JOIN 
    (SELECT 
         ANO, 
         SUM(VATD) * 0.0000001 AS total_vatd_ano
     FROM 
         dva_igpdi
     GROUP BY 
         ANO) AS total 
    ON d.ANO = total.ANO
GROUP BY 
    d.ANO, s.SETOR, total.total_vatd_ano
ORDER BY 
    d.ANO, s.SETOR;


"

# Executar a consulta SQL e coletar os resultados
df <- dbGetQuery(con, query2)

df <- df %>%
  pivot_wider(names_from = SETOR, values_from = VATD_PERCENTUAL)

glimpse(head(df,0))
View(df)
glimpse(df_long)

# Columns: 6
#  $ ANO                        <int> 
#  $ `Comércio e Serviços`      <dbl> 
#  $ Financeiro                 <dbl> 
#  $ Indústria                  <dbl> 
#  $ `Infraestrutura e Energia` <dbl> 
#  $ Outros                     <dbl> 


# V.3

ggplot(df_long, aes(x = ANO, y = Valor, fill = Categoria)) +
  geom_area(position = "stack", color = "#e5e5e5", linewidth = 0.10) + # Define a cor da borda e a largura
  scale_fill_manual(values = rev(c("Comércio e Serviços" = "#222222",
                                   "Financeiro" = "#444444", "Indústria" = "#777777",
                                   "Infraestrutura e Energia" = "#999999"))) +
  labs(x = "Anos", y = "Valores (%)", fill = "Legenda:") + # Altera o título da legenda
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5), # Define a posição dos textos do eixo x para horizontal
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    panel.grid = element_blank(), # Remove as linhas de grade
    panel.border = element_rect(color = "black", fill = NA), # Adiciona uma borda ao redor do gráfico
    legend.background = element_rect(color = "black", size = 0.5), # Adiciona uma borda ao redor da legenda
    legend.key = element_rect(fill = NA, color = NA) # Define o fundo das chaves da legenda como transparente
  ) +
  scale_x_continuous(breaks = seq(min(df_long$ANO), max(df_long$ANO), by = 1)) # Mostra todos os anos no eixo x

#
#
#
#
# V.4


ggplot(df_long, aes(x = ANO, y = Valor, fill = Categoria)) +
  geom_area_pattern(position = "stack", color = "#e5e5e5", linewidth = 0.10, 
                    pattern = "circle", pattern_fill = "white", pattern_density = 0.1) + # Define o padrão de hachura
  scale_fill_manual(values = rev(c("Comércio e Serviços" = "#222222",
                                   "Financeiro" = "#444444", "Indústria" = "#777777",
                                   "Infraestrutura e Energia" = "#999999"))) +
  labs(x = "Anos", y = "Valores (%)", fill = "Legenda:") + # Altera o título da legenda
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(hjust = 0.5), # Define a posição dos textos do eixo x para horizontal
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    panel.grid = element_blank(), # Remove as linhas de grade
    panel.border = element_rect(color = "black", fill = NA), # Adiciona uma borda ao redor do gráfico
    legend.background = element_rect(color = "black", size = 0.5), # Adiciona uma borda ao redor da legenda
    legend.key = element_rect(fill = NA, color = NA) # Define o fundo das chaves da legenda como transparente
  ) +
  scale_x_continuous(breaks = seq(min(df_long$ANO), max(df_long$ANO), by = 1)) # Mostra todos os anos no eixo x
