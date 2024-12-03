options(scipen = 999)
# Instalar pacotes necessários
install.packages("pacman")
pacman::p_load(DBI, RSQLite)

# Conectar ao banco de dados SQLite
con <- dbConnect(RSQLite::SQLite(), "C:/Users/mrs/Documents/Dev/dva/db/dva.db")

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

# Correlação entre as Variáveis
# Remover linhas com NA
dados <- na.omit(dados[, c("ln_Receitas", "ln_Pessoal", "ln_Governo", "ln_RCT", "ln_RCP")])

cor_matrix <- cor(dados[, c("ln_Receitas", "ln_Pessoal", "ln_Governo", "ln_RCT", "ln_RCP")])
cor_matrix

pacman::p_load(car)

# Modelo linear para calcular VIF
vif_model <- lm(ln_Receitas ~ ln_Pessoal + ln_Governo + ln_RCT + ln_RCP, data = dados)
vif_values <- car::vif(vif_model)
vif_values


############
############
# GRÁFICOS #

# Carregar o pacote necessário
library(ggplot2)
library(patchwork)

# Adicionar linha vermelha deslocada aos gráficos
grafico1 <- ggplot(dados, aes(x = ln_Receitas, y = ln_Pessoal)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  geom_abline(slope = 1, intercept = -2, color = "red", linetype = "dashed", size = 0.8) +  # Linha deslocada
  labs(title = "Receitas por Pessoal", x = "ln(Receitas)", y = "ln(Pessoal)") +
  tema_personalizado

grafico2 <- ggplot(dados, aes(x = ln_Receitas, y = ln_Governo)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  geom_abline(slope = 1, intercept = -1.5, color = "red", linetype = "dashed", size = 0.8) +  # Linha deslocada
  labs(title = "Receitas por Governo", x = "ln(Receitas)", y = "ln(Governo)") +
  tema_personalizado

grafico3 <- ggplot(dados, aes(x = ln_Receitas, y = ln_RCT)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  geom_abline(slope = 1, intercept = -2, color = "red", linetype = "dashed", size = 0.8) +  # Linha deslocada
  labs(title = "Receitas por RCT", x = "ln(Receitas)", y = "ln(RCT)") +
  tema_personalizado

grafico4 <- ggplot(dados, aes(x = ln_Receitas, y = ln_RCP)) +
  geom_point(shape = 1) +  # Bolinha aberta
  geom_smooth(method = "lm", formula = y ~ x + ANO, se = FALSE, color = "blue") +
  geom_abline(slope = 1, intercept = -2, color = "red", linetype = "dashed", size = 0.8) +  # Linha deslocada
  labs(title = "Receitas por RCP", x = "ln(Receitas)", y = "ln(RCP)") +
  tema_personalizado

# Combinar os gráficos em uma única figura
(grafico1 | grafico2) /
  (grafico3 | grafico4)



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


# Configurar a janela gráfica para 2x2
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Margens ajustadas

# Gráfico de resíduos para o modelo ln_Pessoal
plot(modelo_ln_pessoal, which = 1, main = "")  # Remove título automático
title("Resíduos do Modelo ln_Pessoal")         # Adiciona título manual

# Gráfico de resíduos para o modelo ln_Governo
plot(modelo_ln_governo, which = 1, main = "")  # Remove título automático
title("Resíduos do Modelo ln_Governo")         # Adiciona título manual

# Gráfico de resíduos para o modelo ln_RCT
plot(modelo_ln_rct, which = 1, main = "")      # Remove título automático
title("Resíduos do Modelo ln_RCT")             # Adiciona título manual

# Gráfico de resíduos para o modelo ln_RCP
plot(modelo_ln_rcp, which = 1, main = "")      # Remove título automático
title("")             # Adiciona título manual

# Resetar o layout para gráficos individuais
par(mfrow = c(1, 1))



###########
##############
################
# Configurar layout para 2x2
par(mfrow = c(2, 2), mar = c(4, 4, 4, 2))  # Ajuste das margens

# Gráfico de resíduos do modelo ln_Pessoal
residuos_ln_pessoal <- resid(modelo_ln_pessoal)
ajustados_ln_pessoal <- fitted(modelo_ln_pessoal)
plot(
  ajustados_ln_pessoal, residuos_ln_pessoal,
  xlab = "Fitted values", ylab = "Residuals",
  main = "Resíduos do Modelo ln(Pessoal)",
  pch = 20, col = "black"
)
abline(h = 0, col = "red", lty = 2, lwd = 2)  # Linha horizontal

# Gráfico de resíduos do modelo ln_Governo
residuos_ln_governo <- resid(modelo_ln_governo)
ajustados_ln_governo <- fitted(modelo_ln_governo)
plot(
  ajustados_ln_governo, residuos_ln_governo,
  xlab = "Fitted values", ylab = "Residuals",
  main = "Resíduos do Modelo ln(Governo)",
  pch = 20, col = "black"
)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Gráfico de resíduos do modelo ln_RCT
residuos_ln_rct <- resid(modelo_ln_rct)
ajustados_ln_rct <- fitted(modelo_ln_rct)
plot(
  ajustados_ln_rct, residuos_ln_rct,
  xlab = "Fitted values", ylab = "Residuals",
  main = "Resíduos do Modelo ln(RCT)",
  pch = 20, col = "black"
)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Gráfico de resíduos do modelo ln_RCP
residuos_ln_rcp <- resid(modelo_ln_rcp)
ajustados_ln_rcp <- fitted(modelo_ln_rcp)
plot(
  ajustados_ln_rcp, residuos_ln_rcp,
  xlab = "Fitted values", ylab = "Residuals",
  main = "Resíduos do Modelo ln(RCP)",
  pch = 20, col = "black"
)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# Resetar layout para 1 gráfico
par(mfrow = c(1, 1))



############ 

# BOXPLOT
# Extrair coeficientes de cada modelo
coeficientes <- data.frame(
  Modelo = rep(c("ln(Pessoal)", "ln(Governo)", "ln(RCT)", "ln(RCP)"), each = 2),  # Nome dos modelos
  Termo = rep(c("Intercepto", "ln(Receitas)"), times = 4),  # Nome dos termos
  Coeficiente = c(coef(modelo_ln_pessoal),
                  coef(modelo_ln_governo),
                  coef(modelo_ln_rct),
                  coef(modelo_ln_rcp))  # Coeficientes
)

# Criar o boxplot
ggplot(coeficientes, aes(x = Termo, y = Coeficiente, fill = Modelo)) +
  geom_boxplot(outlier.shape = NA, outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot dos Coeficientes dos Modelos",
    x = "Termos do Modelo",
    y = "Coeficientes"
  ) +
  scale_fill_brewer(palette = "Pastel1") +  # Paleta de cores pastel
  tema_personalizado

# Filtrar apenas os coeficientes do Intercepto
coeficientes_intercepto <- coeficientes[coeficientes$Termo == "Intercepto", ]

# Criar o boxplot horizontal para o Intercepto sem cores
boxplot1 = ggplot(coeficientes_intercepto, aes(x = Modelo, y = Coeficiente)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +  # Sem preenchimento, apenas bordas
  labs(
    title = "Boxplot dos Interceptos por Modelo",
    x = "",
    y = "Coeficiente do Intercepto"
  ) +
  ylim(-0.3, NA) +  # Define o limite inferior como -0.7 e mantém o superior automático
  tema_personalizado +
  theme(
    legend.position = "none",  # Remover legenda
    axis.text.x = element_text(size = 14, face = "bold"),  # Aumentar e destacar a fonte dos modelos
    axis.text.y = element_text(size = 12),  # Aumentar a fonte do eixo Y
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centralizar e destacar o título
    axis.title.y = element_text(size = 14)  # Aumentar fonte do rótulo do eixo Y
  )


# Filtrar apenas os coeficientes de ln(Receitas)
coeficientes_ln_receitas <- coeficientes[coeficientes$Termo == "ln(Receitas)", ]

# Criar o boxplot horizontal para ln(Receitas)
boxplot2 = ggplot(coeficientes_ln_receitas, aes(x = Modelo, y = Coeficiente)) +
  geom_boxplot(outlier.shape = NA, fill = "white", color = "black") +  # Sem preenchimento, apenas bordas
  labs(
    title = "Boxplot dos Coeficientes por Modelo",
    x = "",
    y = "Coeficiente de ln(Receitas)"
  ) +
  ylim(-0.3, NA) +  # Ajustar o limite inferior se necessário
  tema_personalizado +
  theme(
    legend.position = "none",  # Remover legenda
    axis.text.x = element_text(size = 14, face = "bold"),  # Aumentar e destacar a fonte dos modelos
    axis.text.y = element_text(size = 12),  # Aumentar a fonte do eixo Y
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centralizar e destacar o título
    axis.title.y = element_text(size = 14)  # Aumentar fonte do rótulo do eixo Y
  )

(boxplot1 | boxplot2)






