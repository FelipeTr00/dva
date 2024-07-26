# Carregar os pacotes necessários
pacman::p_load(dplyr, DBI, RSQLite)
# Conectar ao banco de dados SQLite
db <- "D:/monografia/#monografia-github/database/dva.db"
con <- dbConnect(RSQLite::SQLite(), db)

# Definir a consulta SQL
query <- "
SELECT
    ANO,
    SETOR_ATIV,
    DENOM_CIA,
    CD_CVM,

    SUM(Receitas) AS Total_Receitas,
    (SUM(VAB) / SUM(Receitas)) * 100 AS GVAR,
    (SUM(VALP) / SUM(Receitas)) * 100 AS GVALR,
    (SUM(VALP) / SUM(VATD)) * 100 AS GCPR,
    (SUM(Pessoal) / SUM(VATD)) * 100 AS PDVAP,
    (SUM(Governo) / SUM(VATD)) * 100 AS PDVAG,
    (SUM(RCT) / SUM(VATD)) * 100 AS PDVAT,
    (SUM(RCP) / SUM(VATD)) * 100 AS PDVAA,

    (SUM(Pessoal) / SUM(Receitas)) * 100 AS PesRec,
    (SUM(Governo) / SUM(Receitas)) * 100 AS GovRec,
    (SUM(RCT) / SUM(Receitas)) * 100 AS RctR,
    (SUM(RCP) / SUM(Receitas)) * 100 AS RcpR,

    (SUM(Pessoal) / SUM(VALP)) * 100 AS PesVALP,
    (SUM(Governo) / SUM(VALP)) * 100 AS GovVALP,
    (SUM(RCT) / SUM(VALP)) * 100 AS RctVALP,
    (SUM(RCP) / SUM(VALP)) * 100 AS RcpVALP

FROM dva_contas
WHERE SETOR_ATIV IS NOT NULL
  -- AND Receitas > 0
  AND ANO != 2008 -- ALTERAR SE NECESSÁRIO
GROUP BY ANO, CD_CVM
ORDER BY ANO, CD_CVM;

"

# Executar a consulta SQL e coletar os resultados
result <- dbGetQuery(con, query)

# Exibir o resultado
glimpse(result)

View(result)



## MIGRATION

# Passo 1: Excluir a tabela existente (se necessário)
dbExecute(con, "DROP TABLE IF EXISTS dva_indicadores;")

# Passo 2: Criar a nova tabela com a estrutura desejada
table_name <- "dva_indicadores"
create_table_query <- "
CREATE TABLE dva_indicadores (
    ANO INTEGER,
    SETOR_ATIV TEXT,
    DENOM_CIA TEXT,
    CD_CVM INTEGER,
    Total_Receitas REAL,
    GVAR REAL,
    GVALR REAL,
    GCPR REAL,
    PDVAP REAL,
    PDVAG REAL,
    PDVAT REAL,
    PDVAA REAL,
    PesRec REAL,
    GovRec REAL,
    RctR REAL,
    RcpR REAL,
    PesVALP REAL,
    GovVALP REAL,
    RctVALP REAL,
    RcpVALP REAL,
    PRIMARY KEY (ANO, CD_CVM)
);"

# Executar a consulta para criar a nova tabela
dbExecute(con, create_table_query)

# Passo 3: Inserir os dados na nova tabela
dbWriteTable(con, table_name, result, append = TRUE, row.names = FALSE)

# Verificar se a tabela foi criada corretamente
dbListTables(con)
dbReadTable(con, table_name)

# Desconectar do banco de dados
dbDisconnect(con)