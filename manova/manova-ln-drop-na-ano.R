options(scipen = 999)
# Instalar pacotes necessários
install.packages("pacman")
pacman::p_load(DBI, RSQLite)

# Conectar ao banco de dados SQLite
con <- dbConnect(RSQLite::SQLite(), "/home/mrs/Dev/dva/db/dva.db")

# Carregar dados da tabela 'dva_contas' incluindo a variável ANO
query <- "SELECT Receitas, Pessoal, Governo, RCT, RCP, ANO FROM dva_contas"
dados <- dbGetQuery(con, query)

# Remover linhas com valores não positivos (zeros ou negativos)
dados <- dados[dados$Receitas > 0 & dados$Pessoal > 0 & dados$Governo > 0 & dados$RCT > 0 & dados$RCP > 0, ]

# Aplicar o logaritmo natural em todas as variáveis
dados$ln_Receitas <- log(dados$Receitas)
dados$ln_Pessoal <- log(dados$Pessoal)
dados$ln_Governo <- log(dados$Governo)
dados$ln_RCT <- log(dados$RCT)
dados$ln_RCP <- log(dados$RCP)

# Converter a variável ANO em fator (se ainda não for)
dados$ANO <- as.factor(dados$ANO)

# Definindo as variáveis dependentes (logaritmos)
dependentes <- cbind(dados$ln_Pessoal, dados$ln_Governo, dados$ln_RCT, dados$ln_RCP)

# Definindo o modelo MANOVA com as variáveis independentes ln_Receitas e ANO
manova_modelo <- manova(dependentes ~ dados$ln_Receitas + dados$ANO)

# Fechar a conexão com o banco de dados
dbDisconnect(con)

# ANOVA para cada variável dependente
anova_ln_pessoal <- summary.aov(manova_modelo)[[1]]
anova_ln_governo <- summary.aov(manova_modelo)[[2]]
anova_ln_rct <- summary.aov(manova_modelo)[[3]]
anova_ln_rcp <- summary.aov(manova_modelo)[[4]]

# Regressões lineares univariadas para cada variável dependente (logaritmos)
modelo_ln_pessoal <- lm(ln_Pessoal ~ ln_Receitas + ANO, data = dados)
modelo_ln_governo <- lm(ln_Governo ~ ln_Receitas + ANO, data = dados)
modelo_ln_rct <- lm(ln_RCT ~ ln_Receitas + ANO, data = dados)
modelo_ln_rcp <- lm(ln_RCP ~ ln_Receitas + ANO, data = dados)

# Ver os resultados da MANOVA
summary(manova_modelo)

# Verificar os testes de significância multivariados (Pillai, Wilks, Hotelling-Lawley, Roy)
summary(manova_modelo, test = "Pillai")
summary(manova_modelo, test = "Wilks")
summary(manova_modelo, test = "Hotelling-Lawley")
summary(manova_modelo, test = "Roy")

# Visualizando os resultados das ANOVAs individuais
anova_ln_pessoal
anova_ln_governo
anova_ln_rct
anova_ln_rcp

# Ver os coeficientes de regressão para cada modelo
coef(modelo_ln_pessoal)
coef(modelo_ln_governo)
coef(modelo_ln_rct)
coef(modelo_ln_rcp)


############
############
# GRÁFICOS #

library(ggplot2)


# Definir um tema simples sem grid lines e com ticks
tema_personalizado <- theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", size = 0.8),
    plot.title = element_text(hjust = 0.5)  # Centralizar o título
  )

# Gráfico de ln_Receitas vs ln_Pessoal com linha de regressão e bolinha aberta
ggplot(dados, aes(x = ln_Receitas, y = ln_Pessoal)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  labs(title = "ln_Receitas vs ln_Pessoal", x = "ln_Receitas", y = "ln_Pessoal") +
  tema_personalizado

# Gráfico de ln_Receitas vs ln_Governo com linha de regressão e bolinha aberta
ggplot(dados, aes(x = ln_Receitas, y = ln_Governo)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  labs(title = "ln_Receitas vs ln_Governo", x = "ln_Receitas", y = "ln_Governo") +
  tema_personalizado

# Gráfico de ln_Receitas vs ln_RCT com linha de regressão e bolinha aberta
ggplot(dados, aes(x = ln_Receitas, y = ln_RCT)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  labs(title = "ln_Receitas vs ln_RCT", x = "ln_Receitas", y = "ln_RCT") +
  tema_personalizado

# Gráfico de ln_Receitas vs ln_RCP com linha de regressão e bolinha aberta
ggplot(dados, aes(x = ln_Receitas, y = ln_RCP)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  labs(title = "ln_Receitas vs ln_RCP", x = "ln_Receitas", y = "ln_RCP") +
  tema_personalizado


###############


# Boxplot para ln_Pessoal por ANO
ggplot(dados, aes(x = ANO, y = ln_Pessoal)) +
  geom_boxplot() +
  labs(title = "Distribuição de ln_Pessoal por ANO", x = "ANO", y = "ln_Pessoal")

# Boxplot para ln_Governo por ANO
ggplot(dados, aes(x = ANO, y = ln_Governo)) +
  geom_boxplot() +
  labs(title = "Distribuição de ln_Governo por ANO", x = "ANO", y = "ln_Governo")

# Boxplot para ln_RCT por ANO
ggplot(dados, aes(x = ANO, y = ln_RCT)) +
  geom_boxplot() +
  labs(title = "Distribuição de ln_RCT por ANO", x = "ANO", y = "ln_RCT")

# Boxplot para ln_RCP por ANO
ggplot(dados, aes(x = ANO, y = ln_RCP)) +
  geom_boxplot() +
  labs(title = "Distribuição de ln_RCP por ANO", x = "ANO", y = "ln_RCP")





###############


# Gráfico de resíduos para o modelo ln_Pessoal
plot(modelo_ln_pessoal, which = 1, main = "Resíduos do Modelo ln_Pessoal")

# Gráfico de resíduos para o modelo ln_Governo
plot(modelo_ln_governo, which = 1, main = "Resíduos do Modelo ln_Governo")

# Gráfico de resíduos para o modelo ln_RCT
plot(modelo_ln_rct, which = 1, main = "Resíduos do Modelo ln_RCT")

# Gráfico de resíduos para o modelo ln_RCP
plot(modelo_ln_rcp, which = 1, main = "Resíduos do Modelo ln_RCP")

