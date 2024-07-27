pacman::p_load(dplyr, DBI, RSQLite)
# Conectar ao banco de dados SQLite
db <- "D:/monografia/_dva/db/dva.db"
con <- dbConnect(RSQLite::SQLite(), db);

# Definir a consulta SQL
query <- "
SELECT 
    d.ANO,
    s.SETOR,
    ROUND(SUM(d.Receitas)) Receitas,
    ROUND(SUM(d.VAB)) VAB,
    ROUND(SUM(d.VATD)) VATD,
    ROUND(SUM(d.Governo)) Governo,
    ROUND(SUM(d.Pessoal)) Pessoal,
    ROUND(SUM(d.RCT)) Credores,
    ROUND(SUM(d.RCP)) Acionistas
FROM dva_igpdi d
RIGHT JOIN setores s
ON s.SETOR_ATIV = d.SETOR_ATIV
WHERE ANO in ('2009','2016','2023')

GROUP BY ANO, SETOR;
";

df <- dbGetQuery(con, query)
glimpse(head(df,10));

############
## TABLE1 ##
############

