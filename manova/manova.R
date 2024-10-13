# Instalar pacotes necessários
install.packages("pacman")
pacman::p_load(DBI, RSQLite)

# Conectar ao banco de dados SQLite
con <- dbConnect(RSQLite::SQLite(), "/home/mrs/Dev/dva/db/dva.db")

# Carregar dados da tabela 'dva_contas'
query <- "SELECT Receitas, Pessoal, Governo, RCT, RCP FROM dva_contas"
dados <- dbGetQuery(con, query)

# Realizar a MANOVA
# Definindo as variáveis dependentes
dependentes <- cbind(dados$Pessoal, dados$Governo, dados$RCT, dados$RCP)

# Definindo o modelo MANOVA com a variável independente Receitas
manova_modelo <- manova(dependentes ~ dados$Receitas)

# Ver os resultados da MANOVA
summary(manova_modelo)

# Verificar os testes de significância multivariados (Pillai, Wilks, Hotelling-Lawley, Roy)
summary(manova_modelo, test = "Pillai")
summary(manova_modelo, test = "Wilks")
summary(manova_modelo, test = "Hotelling-Lawley")
summary(manova_modelo, test = "Roy")

# Fechar a conexão com o banco de dados
dbDisconnect(con)


# ANOVA para cada variável dependente
anova_pessoal <- summary.aov(manova_modelo, test = "Pillai")[[1]]
anova_governo <- summary.aov(manova_modelo, test = "Pillai")[[2]]
anova_rct <- summary.aov(manova_modelo, test = "Pillai")[[3]]
anova_rcp <- summary.aov(manova_modelo, test = "Pillai")[[4]]

# Visualizando os resultados
anova_pessoal
anova_governo
anova_rct
anova_rcp

# Regressões lineares univariadas para cada variável dependente
modelo_pessoal <- lm(Pessoal ~ Receitas, data = dados)
modelo_governo <- lm(Governo ~ Receitas, data = dados)
modelo_rct <- lm(RCT ~ Receitas, data = dados)
modelo_rcp <- lm(RCP ~ Receitas, data = dados)

# Ver os coeficientes de regressão (slopes) para cada modelo
coef(modelo_pessoal)
coef(modelo_governo)
coef(modelo_rct)
coef(modelo_rcp)