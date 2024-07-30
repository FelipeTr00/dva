# Carregar os pacotes necessários
pacman::p_load(dplyr, tidyr, DBI, RSQLite, plm)
options(sapien =999)
# Conectar ao banco de dados SQLite
db <- "D:/monografia/_dva/db/dva.db"
con <- dbConnect(RSQLite::SQLite(), db)

# Definir a consulta SQL
query <- "

SELECT * FROM dva;

"

query2 <- "

SELECT
    ANO AS Tempo,
    CD_CVM AS Individuos,
    Receitas Receitas,
    Pessoal Pessoal,
    Governo Governo,
    RCT Credores,
    RCP Acionistas
FROM
    dva_contas
WHERE
    CD_CVM IN (
        SELECT CD_CVM
        FROM dva_contas
        GROUP BY CD_CVM
        HAVING COUNT(DISTINCT ANO) = (SELECT COUNT(DISTINCT ANO) FROM dva_contas)
    );

"

# Executar a consulta SQL e coletar os resultados
df <- dbGetQuery(con, query2)


glimpse(head(df,0))
View(df)

#Columns: 7
#$ Tempo      <int> 
#  $ Individuos <chr> 
#  $ Receitas   <dbl> 
#  $ Pessoal    <dbl> 
#  $ Governo    <dbl> 
#  $ Credores   <dbl> 
#  $ Acionistas <dbl> 

##########################
##########################
##### REG PANEL DATA #####
##########################
##########################

# Converter os dados em dados em painel:
df <- na.omit(df)
dados_em_painel <- pdata.frame(df, index = c("Individuos", "Tempo"));

glimpse(head(dados_em_painel, 1))
# Modelo de Efeitos Fixos:
efeitos_fixos <- plm(Pessoal ~ Receitas,
                     data = dados_em_painel, model = "within");


summary(efeitos_fixos);


# Converter os dados para painel
dados_em_painel <- pdata.frame(df, index = c("Individuos", "Tempo"))

# Modelo de Efeitos Fixos: Receitas como variável dependente
efeitos_fixos <- plm(Receitas ~ Pessoal + Governo + Credores + Acionistas, 
                     data = dados_em_painel, model = "within")

summary(efeitos_fixos)



# V.2 MODELO SUR


# Ajustar modelos separados
modelo_pessoal <- plm(Pessoal ~ Receitas, data = dados_em_painel, model = "within")
modelo_governo <- plm(Governo ~ Receitas, data = dados_em_painel, model = "within")
modelo_acionistas <- plm(Acionistas ~ Receitas, data = dados_em_painel, model = "within")
modelo_credores <- plm(Credores ~ Receitas, data = dados_em_painel, model = "within")

# Sumário dos modelos
summary(modelo_pessoal)
summary(modelo_governo)
summary(modelo_acionistas)
summary(modelo_credores)

# Extrair resíduos
residuos_pessoal <- residuals(modelo_pessoal)
residuos_governo <- residuals(modelo_governo)
residuos_acionistas <- residuals(modelo_acionistas)
residuos_credores <- residuals(modelo_credores)

# Analisar correlação entre resíduos
residuos <- data.frame(
  Pessoal = residuos_pessoal,
  Governo = residuos_governo,
  Acionistas = residuos_acionistas,
  Credores = residuos_credores
)

cor(residuos, use = "complete.obs")



# V.3 

pacman::p_load(lavaan)

# Definindo o modelo SEM
model <- '
  Pessoal ~ Receitas
  Governo ~ Receitas
  Acionistas ~ Receitas
  Credores ~ Receitas
'

# Ajustando o modelo SEM
fit <- sem(model, data = dados_em_painel)

# Sumário do modelo SEM
summary(fit, fit.measures = TRUE)


library(lavaan)

# Definindo o modelo SEM com um fator latente para capturar variância comum
model <- '
  # Fator latente
  F1 =~ Pessoal + Governo + Acionistas + Credores
  
  # Regressões
  Pessoal ~ Receitas
  Governo ~ Receitas
  Acionistas ~ Receitas
  Credores ~ Receitas
  
  # Variância do fator latente
  F1 ~~ F1
'

# Ajustando o modelo SEM
fit <- sem(model, data = dados_em_painel)

# Sumário do modelo SEM
summary(fit, fit.measures = TRUE)

