pacman::p_load(tidyr, dplyr, DBI, RSQLite)
options(sapien = 999)

# Defina o caminho do arquivo CSV
arquivo <- "D:/monografia/#monografia-github/dados/outros/ibovespa.csv"

# Ler o arquivo CSV
dados <- read.csv2(arquivo, fileEncoding ="utf-8")

# Exibir a estrutura dos dados para entender as colunas
glimpse(dados)

# Tratamento de datas
# library(lubridate)
# dados$mes_ano <- as.Date(dados$mes_ano, format = "%Y-%m-%d")
# dados$DATA <- dados$MES_ANO

# Remover as colunas indesejadas
# dados <- dados %>%
#  select(-SETOR_ECON, -VL_CONTA, -VL_INPC, -VL_IPCA, -VL_IGPM, -VL_MEDIO, -CD_CONTA)

# Transformar de long para wide
# dados <- dados %>%
#  pivot_wider(names_from = DS_CONTA, values_from = VL_IGPDI)

# Exibir os dados transformados
# View(dados)
# glimpse(dados)


# Se desejar, salvar o resultado em um novo arquivo CSV
# write.csv(dados_wide, "D:/monografia/monografia-github/dados/dva_dados_wide.csv", row.names = FALSE)
##################################
##################################
# Conectar ao banco de dados SQLite
# Criar uma conexão com o banco de dados
glimpse(dados)
con <- dbConnect(RSQLite::SQLite(), "D:/monografia/#monografia-github/database/dva.db")

# Passo 1: Excluir a tabela existente (se necessário)
 dbExecute(con, "DROP TABLE IF EXISTS ibovespa;")

# Passo 2: Criar a nova tabela com a estrutura desejada
create_table_query <- "
CREATE TABLE ibovespa (
    ano INTEGER,
    mes_ano DATE,
    valor_em_brl REAL,
    valor_igpdi_brl REAL,
    valor_em_usd REAL,
    n_cias INTEGER
);"

# Executar a consulta para criar a nova tabela
dbExecute(con, create_table_query)

# Passo 3: Inserir os dados na nova tabela
dbWriteTable(con, "ibovespa", dados, append = TRUE, row.names = FALSE)

# Verificar se a tabela foi criada corretamente
dbListTables(con)
dbReadTable(con, "ibovespa")

# Desconectar do banco de dados
dbDisconnect(con)
