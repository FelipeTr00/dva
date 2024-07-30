# Gerar um conjunto de dados de exemplo
install.packages("plm")
install.packages("car") # Para MANOVA
library(plm)
library(car)



# Gerar um conjunto de dados de exemplo
set.seed(123)
dados <- data.frame(
  Y1 = rnorm(100),
  Y2 = rnorm(100),
  Y3 = rnorm(100),
  Y4 = rnorm(100),
  X1 = rnorm(100),
  C1 = as.factor(sample(c("A", "B", "C"), 100, replace = TRUE))
)

# Verificar as primeiras linhas dos dados
head(dados)

# Ajustar a MANOVA
modelo <- manova(cbind(Y1, Y2, Y3, Y4) ~ X1 + C1, data = dados)

# Resumo da MANOVA
summary(modelo)

# Resumo detalhado da MANOVA
summary(modelo, test = "Pillai")
summary.lm(modelo)

###########################
######### MANOVA ##########
###########################

pacman::p_load(dplyr, tidyr, DBI, RSQLite, plm, car)
# Conectar ao banco de dados SQLite
db <- "D:/monografia/_dva/db/dva.db"
con <- dbConnect(RSQLite::SQLite(), db);

# Definir a consulta SQL
query <- "
SELECT 
    d.ANO,
    s.SETOR,
    d.SETOR_ATIV,
    d.Receitas,
    d.VAB,
    d.VATD,
    d.Governo,
    d.Pessoal,
    d.RCT Financiadores,
    d.RCP Acionistas

FROM dva_contas d
RIGHT JOIN setores s
ON s.SETOR_ATIV = d.SETOR_ATIV
WHERE ANO IS NOT NULL;
";

df <- dbGetQuery(con, query)
dbDisconnect(con)

glimpse(head(df,10));

summary(df)


# Normalizar as colunas numéricas
df_normalized <- df %>%
  mutate(
    across(c(Receitas, VAB, VATD, Governo, Pessoal, Financiadores, Acionistas), ~ as.numeric(scale(.))),
    SETOR = as.factor(SETOR),
    SETOR_ATIV = as.factor(SETOR_ATIV)
  ) %>%
  drop_na()
# Visualizar os primeiros registros do dataframe normalizado
glimpse(head(df_normalized, 10))

# Visualizar o sumário dos dados normalizados
summary(df_normalized)


############################
######## EXEC. MANOVA ######

# Ajustar a MANOVA
modelo <- manova(cbind(Pessoal, Governo, Acionistas, Financiadores) ~ Receitas + SETOR, data = df_normalized)

# Resumo da MANOVA
summary(modelo)

# Resumo detalhado da MANOVA
summary(modelo, test = c("Wilks"))
summary.lm(modelo)
