options(scipen = 999)
# Instalar pacotes necessários
install.packages("pacman")
pacman::p_load(DBI, RSQLite)

# Conectar ao banco de dados SQLite
con <- dbConnect(RSQLite::SQLite(), "/home/mrs/Dev/dva/db/dva.db")

# Carregar dados da tabela 'dva_contas'
query <- "SELECT Receitas, Pessoal, Governo, RCT, RCP FROM dva_contas"
dados <- dbGetQuery(con, query)

# Remover linhas com valores não positivos (zeros ou negativos)
dados <- dados[dados$Receitas > 0 & dados$Pessoal > 0 & dados$Governo > 0 & dados$RCT > 0 & dados$RCP > 0, ]

# Aplicar o logaritmo natural em todas as variáveis
dados$ln_Receitas <- log(dados$Receitas)
dados$ln_Pessoal <- log(dados$Pessoal)
dados$ln_Governo <- log(dados$Governo)
dados$ln_RCT <- log(dados$RCT)
dados$ln_RCP <- log(dados$RCP)

# Realizar a MANOVA
# Definindo as variáveis dependentes (logaritmos)
dependentes <- cbind(dados$ln_Pessoal, dados$ln_Governo, dados$ln_RCT, dados$ln_RCP)

# Definindo o modelo MANOVA com a variável independente ln_Receitas
manova_modelo <- manova(dependentes ~ dados$ln_Receitas)

# Fechar a conexão com o banco de dados
dbDisconnect(con)

# ANOVA para cada variável dependente
anova_ln_pessoal <- summary.aov(manova_modelo, test = "Pillai")[[1]]
anova_ln_governo <- summary.aov(manova_modelo, test = "Pillai")[[2]]
anova_ln_rct <- summary.aov(manova_modelo, test = "Pillai")[[3]]
anova_ln_rcp <- summary.aov(manova_modelo, test = "Pillai")[[4]]


# Regressões lineares univariadas para cada variável dependente (logaritmos)
modelo_ln_pessoal <- lm(ln_Pessoal ~ ln_Receitas, data = dados)
modelo_ln_governo <- lm(ln_Governo ~ ln_Receitas, data = dados)
modelo_ln_rct <- lm(ln_RCT ~ ln_Receitas, data = dados)
modelo_ln_rcp <- lm(ln_RCP ~ ln_Receitas, data = dados)

# Ver os resultados da MANOVA
summary(manova_modelo)

# Verificar os testes de significância multivariados (Pillai, Wilks, Hotelling-Lawley, Roy)
summary(manova_modelo, test = "Pillai")
summary(manova_modelo, test = "Wilks")
summary(manova_modelo, test = "Hotelling-Lawley")
summary(manova_modelo, test = "Roy")

# Visualizando os resultados
anova_ln_pessoal
anova_ln_governo
anova_ln_rct
anova_ln_rcp

# Ver os coeficientes de regressão (slopes) para cada modelo
coef(modelo_ln_pessoal)
coef(modelo_ln_governo)
coef(modelo_ln_rct)
coef(modelo_ln_rcp)
