# Bancos de dados usados ---------------------------------------------------------------------------------------

vd <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/VD.csv") # banco de dados com inputs e outputs para o DEA
vi <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/VI.csv")
geral_final <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/geral_final.csv")

## bancos por regime político ----------------------------------------------------------------------------------------
# divisao por tipo de democracia

aut_fechada <- geral[geral$catg_democracy == 0, ]

aut_eleitoral <- geral[geral$catg_democracy == 1, ]

zona_cinz <- geral[geral$catg_democracy == 2, ]

democr_eleitoral <- geral[geral$catg_democracy == 3, ]

democracia_liberal <- geral[geral$catg_democracy == 4, ]

## plot ----------------------------------------------------------------------
# grafico com todas as VIs
plot(geral$lib.democracy, vd$eff.ajust, type = "n")
text(geral$lib.democracy, vd$eff.ajust, labels = vd$pais, cex = 0.60)

plot(geral$va, vd$eff.ajust, type = "n")
text(geral$va, vd$eff.ajust, labels = vd$pais, cex = 0.60)

plot(geral$rl, vd$eff.ajust, type = "n")
text(geral$rl, vd$eff.ajust, labels = vd$pais, cex = 0.60)

plot(geral$pp.urbana, vd$eff.ajust, type = "n")
text(geral$pp.urbana, vd$eff.ajust, labels = vd$pais, cex = 0.60)

plot(geral$PS.legitimacy, vd$eff.ajust,type = "n")
text(geral$PS.legitimacy, vd$eff.ajust, labels = vd$pais, cex = 0.60)

plot(geral$F.economy, vd$eff.ajust, type = "n")
text(geral$F.economy, vd$eff.ajust, labels = vd$pais, cex = 0.60)

## Histograma das democracias
hist(geral$categ_democracy, breaks = seq(-0.5, 4.5, by = 1), col = "pink", main = "DistribuiÃ§Ã£o das democracias na amostra", xlab = "Tipo de democracia", ylim = c(0, 35))

## DEA - Mensurando a eficiencia governamental ----------------------------------------------------------------
#install.packages("Benchmarking")
library(Benchmarking)

#ordenando em matrizes
x <- as.matrix(vd[ , c(3:7)])
y <- as.matrix(vd[ , c(8:10)])

# ajustando os valores negativos
constante_cc <- abs(min(x[ , 1])) # extraindo o valor min da coluna cc
constante_rq <- abs(min(x[ , 2]))
x[ , 1] <- x[ , 1] + constante_cc # somando a costantes (advinda do valor min da coluna) com cada observaÃ§Ã£o
x[ , 2] <- x[ , 2] + constante_rq

dea_ge <- dea(x, y, RTS = "vrs", ORIENTATION = "in") #utilizando DEA nos inputs e outputs com o modelo BCC
dea_ge_bias <- dea.boot(x, y, RTS = "vrs", ORIENTATION = "in") #bootstrap das eficiencias

vd$eff.DEA <- efficiencies(dea_ge)
vd$eff.ajust <- dea_ge_bias$eff.bc
vd$bias <- dea_ge_bias$bias

## Iniciando preparacao para execucao do modelo de regressao --------------------------------------------------------------------------------------
# verificando pressuposto da multicolineariedade 
sem_vi <- vi[ , -c(1, 2, 9)]

#install.packages("ggcorrplot")
library(ggcorrplot)
library(ggplot2)
ggcorrplot(cor(sem_vi),
           lab = TRUE,              # mostra os valores
           colors = c("red", "white", "blue"),  # cores personalizadas
           title = "Matriz de CorrelaÃ§Ã£o",
           ggtheme = theme_minimal()) +
  theme(
    plot.title = element_text(
      hjust = 0.5,                 # centraliza o tÃ­tulo
      size = 16,                   # aumenta o tamanho da fonte
      face = "bold",               # deixa em negrito
      margin = margin(b = 10)      # ajusta margem inferior do tÃ­tulo
    ),
    plot.margin = unit(c(1, 1, 1, 1), "cm"))  # estilo do grÃ¡fico

## PCA & modelo de regrssao
pca <- prcomp(sem_vi, scale. = TRUE)
acumulado <- summary(pca)$importance[3, ]
n.pca <- which(cumsum(acumulado) >= 0.95)[1] 
x.pca <- pca$x[ , 1:n.pca]
modelo.pca <- lm(vd$eff.ajust ~ x.pca)
summary(modelo.pca) #ambos os pcs dÃ£o significativos para o modelo
AIC(modelo.pca)
BIC(modelo.pca)

## PESOS
pesos <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = c(0.3999395, 0.4152296, 0.4506527, 0.3463064, -0.4306246, -0.3989727),
  PC2 = c(-0.54232018, -0.50796793, -0.02774712, 0.45441115, -0.29177927, -0.39428779)
)

pesos$PC1.pesos <- pesos$PC1 * -0.027328
pesos$PC2.pesos <- pesos$PC2 * -0.010160 
pesos$peso.final <- pesos$PC1.pesos + pesos$PC2.pesos
coeficientes <- pesos[ , c(1, 6)]
coeficientes # aqui temos o impacto de cada variável no modelo
coeficientes$peso.final

## modelo por regime político  --------------------------------------------------------
#Autoracia fechada
vi_aut_fechada <- aut_fechada[ , c(3, 4, 5, 6, 7, 8)]

pca_d1 <- prcomp(vi_aut_fechada, scale. = TRUE)
summary(pca_d1)
acumulado_d1 <- summary(pca_d1)$importance[3, ]
n.pca_d1 <- which(cumsum(acumulado_d1) >= 0.90)[1]
x.pca_d1 <- pca_d1$x[ , 1:n.pca_d1]
modelo.pca_d1 <- lm(aut_fechada$eff.ajust ~ x.pca_d1)
summary(modelo.pca_d1) 
AIC(modelo.pca_d1)
BIC(modelo.pca_d1)
pca_d1$rotation 

n.pca_d1.new <- which(cumsum(acumulado_d1) >= 0.60)[1]
x.pca_d1.new <- pca_d1$x[ , 1:n.pca_d1.new]
modelo.pca_d1.new <- lm(aut_fechada$eff.ajust ~ x.pca_d1.new)
summary(modelo.pca_d1.new)

pesos2 <- c(-0.2596976, -0.2802606, -0.4877775, -0.4253547, 0.4357248, 0.4952793) * 0.025893   
coeficientes2 <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = pesos2
  )


# Autocracia eleitoral 
vi_aut_eleitoral <- aut_eleitoral[ , c(3, 4, 5, 6, 7, 8)]

pca_d2 <- prcomp(vi_aut_eleitoral, scale. = TRUE)
summary(pca_d2) 
acumulado_d2 <- summary(pca_d2)$importance[3, ]
n.pca_d2 <- which(cumsum(acumulado_d2) >= 0.90)[1]
x.pca_d2 <- pca_d2$x[ , 1:n.pca_d2]
modelo.pca_d2 <- lm(aut_eleitoral$eff.ajust ~ x.pca_d2)
summary(modelo.pca_d2)
AIC(modelo.pca_d2)
BIC(modelo.pca_d2)
pca_d2$rotation 

n.pca_d1.new2 <- which(cumsum(acumulado_d2) >= 0.40)[1]
x.pca_d1.new2 <- pca_d2$x[ , 1:n.pca_d1.new2]
modelo.pca_d1.new2 <- lm(aut_eleitoral$eff.ajust ~ x.pca_d1.new2)
summary(modelo.pca_d1.new2) 

pesos3 <- c(0.2480164, 0.2586244, 0.5081671, 0.3069439, -0.5139105, -0.5050236) * -0.021253  
coeficientes3 <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = pesos2
)

# Zona cinzenta 
vi_zona_cinz <- zona_cinz[ , c(3, 4, 5, 6, 7, 8)]

pca_d3 <- prcomp(vi_zona_cinz, scale. = TRUE)
summary(pca_d3)
acumulado_d3 <- summary(pca_d3)$importance[3, ]
n.pca_d3 <- which(cumsum(acumulado_d3) >= 0.90)[1]
x.pca_d3 <- pca_d3$x[ , 1:n.pca_d3]
modelo.pca_d3 <- lm(zona_cinz$eficiencia ~ x.pca_d3)
summary(modelo.pca_d3)
AIC(modelo.pca_d3)
BIC(modelo.pca_d3)

n.pca_d1.new3 <- which(cumsum(acumulado_d3) >= 0.50)[1]
x.pca_d1.new3 <- pca_d3$x[ , 1:n.pca_d1.new3]
modelo.pca_d1.new3 <- lm(zona_cinz$eff.ajust ~ x.pca_d1.new3)
summary(modelo.pca_d1.new3)

pesos4 <- c(-0.01774978, 0.37015104, 0.47721927, 0.38502869, -0.51993673, -0.46513828) * -0.003219
coeficientes4 <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = pesos4
)

# Democracia eleitoral
vi_democr_eleitoral <- democr_eleitoral[ , c(3, 4, 5, 6, 7, 8)]

pca_d4 <- prcomp(vi_democr_eleitoral, scale. = TRUE)
summary(pca_d4)
acumulado_d4 <- summary(pca_d4)$importance[3, ]
n.pca_d4 <- which(cumsum(acumulado_d4) >= 0.90)[1]
x.pca_d4 <- pca_d4$x[ , 1:n.pca_d4]
modelo.pca_d4 <- lm(democr_eleitoral$eficiencia ~ x.pca_d4)
summary(modelo.pca_d4) 
AIC(modelo.pca_d4)
BIC(modelo.pca_d4)
pca_d4$rotation  

n.pca_d1.new4 <- which(cumsum(acumulado_d4) >= 0.45)[1]
x.pca_d1.new4 <- pca_d4$x[ , 1:n.pca_d1.new4]
modelo.pca_d1.new4 <- lm(democr_eleitoral$eff.ajust ~ x.pca_d1.new4)
summary(modelo.pca_d1.new4) 

pesos5 <- c(-0.2592667, -0.4818556, -0.5343130, -0.1512413, 0.5356153, 0.3245736) *  0.013853  
coeficientes5 <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = pesos5
) 

# Democracia liberal 
vi_democracia_liberal <- democracia_liberal[ , c(3, 4, 5, 6, 7, 8)]

pca_d5 <- prcomp(vi_democracia_liberal, scale. = TRUE)
summary(pca_d5)
acumulado_d5 <- summary(pca_d5)$importance[3, ]
n.pca_d5 <- which(cumsum(acumulado_d4) >= 0.90)[1]
x.pca_d5 <- pca_d5$x[ , 1:n.pca_d5]
modelo.pca_d5 <- lm(democracia_liberal$eficiencia ~ x.pca_d5)
summary(modelo.pca_d5)  
AIC(modelo.pca_d5)
BIC(modelo.pca_d5)
pca_d5$rotation 

n.pca_d1.new5 <- which(cumsum(acumulado_d5) >= 0.45)[1]
x.pca_d1.new5 <- pca_d5$x[ , 1:n.pca_d1.new5]
modelo.pca_d1.new5 <- lm(democracia_liberal$eff.ajust ~ x.pca_d1.new5)
summary(modelo.pca_d1.new5) 

pesos6 <- c(-0.3298425, -0.4660928, -0.4795727, -0.1045581, 0.4771551, 0.4531690) *  0.0117337    
coeficientes6 <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = pesos6
)

##-----------------------------------------------------------------------------------
# Análise de resíduos 
residuos_person <- residuals(modelo.pca, type = "pearson")
shapiro.test(residuos_person) # p-value = 0.1067 - pressuposto alcançado 

# teste de correlação de residuos
library(lmtest)
dwtest(modelo.pca)

## Gráficos ----------------------------
residuos_simples <- residuals(modelo.pca)
residuos_padronizados <- residuos_person
valores_ajustados <- fitted(modelo.pca)
cook <- cooks.distance(modelo.pca)

dados_grafico <- data.frame(
  Fitted = valores_ajustados,
  Residuos = residuos_simples,
  StdResiduos = residuos_padronizados,
  CooksDist = cook,
  Index = 1:length(cook)
)

library(ggplot2)
library(patchwork)

# Gráfico 1: Resíduos vs. Valores Ajustados
p1 <- ggplot(dados_grafico, aes(x = Fitted, y = Residuos)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Resíduos vs. Valores Ajustados", x = "Valores Ajustados", y = "Resíduos") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# Gráfico 2: Gráfico QQ dos Resíduos
p2 <- ggplot(dados_grafico, aes(sample = Residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Gráfico QQ dos Resíduos", x = "Quantis Teóricos", y = "Quantis Amostrais") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

# Gráfico 3: Histograma dos Resíduos Padronizados
p3 <- ggplot(dados_grafico, aes(x = StdResiduos)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Hist. Resíduos Padronizados", x = "Resíduos Padronizados", y = "Frequência") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

#elementos para o grafico 4
# Diagnóstico dos pontos influentes (usando a distância de Cook)
influentes <- cooks.distance(modelo.pca)
limite_influencia <- 4 / length(vd$eff.ajust)
pontos_influentes <- which(influentes > limite_influencia)

dados_diag <- data.frame(
  Index = 1:length(cook),
  CooksDist = cook
)
dados_acima <- dados_diag[pontos_influentes, ]

# Gráfico 4: Distância de Cook
library(ggrepel)
p4 <- ggplot(dados_diag, aes(x = Index, y = CooksDist)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_point(data = dados_acima, aes(x = Index, y = CooksDist), color = "red", size = 2) +
  geom_text_repel(data = dados_acima, aes(x = Index, y = CooksDist, label = Index), 
                  size = 3, box.padding = 0.5, point.padding = 0.5, 
                  segment.color = 'grey50') +
  geom_hline(yintercept = 4 / 117, color = "red", linetype = "dashed") +
  labs(title = "Distância de Cook", x = "Observações", y = "Distância de Cook") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        title = element_text(size = 10))

plot(p4)
# Plotar todos os gráficos em uma única página
# Definindo a disposição dos gráficos em uma matriz 2x2
# Combinar os gráficos em uma única figura
combined_plot <- p1 + p2 + p3 + p4 + plot_layout(ncol = 2)
print(combined_plot)
#difícil visualização no espaço do R
#SOLUÇÃO ABAIXO:

# Save the combined plot to a file
ggsave("combined_plot.png", combined_plot, width = 10, height = 8)


#Análise mais detalhada
#pontos de alavanca
# Função para calcular a alavanca
calcular_alavanca <- function(modelo.pca) {
  X <- model.matrix(modelo.pca)
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  alavanca <- diag(H)
  return(alavanca)
}

# Calcular alavanca
alavanca <- calcular_alavanca(modelo.pca)

# Identificar pontos de alavanca (por exemplo, com base na alavanca)
pontos_alavanca <- which(alavanca > 2 * mean(alavanca))  # Usando o critério de 2 vezes a média da alavanca

#tratando a influência dos pontos de alavanca
# Ajustar modelo sem pontos de alavanca
dados_sem_alavanca <- geral[-pontos_alavanca, ]
dados_sem_alavanca_semvd <- dados_sem_alavanca[ , c(3:8)]

pca_sem_alavancas <- prcomp(dados_sem_alavanca_semvd, scale. = TRUE)
summary(pca_sem_alavancas)
acumulado_sem_alavanca <- summary(pca_sem_alavancas)$importance[3, ]
n.pca_sem_alavanca <- which(cumsum(acumulado_sem_alavanca) >= 0.90)[1]
x.pca_sem_alavanca <- pca_sem_alavancas$x[ , 1:n.pca_sem_alavanca]
modelo.pca_sem_alavanca <- lm(dados_sem_alavanca$eff.ajust ~ x.pca_sem_alavanca)

# Ajustar modelo sem pontos influentes
dados_sem_influentes <- geral[-pontos_influentes, ]
dados_sem_influentes_semvd <- dados_sem_influentes[ , c(3:8)]

pca_sem_influentes <- prcomp(dados_sem_influentes_semvd, scale. = TRUE)
summary(pca_sem_influentes)
acumulado_sem_influentes <- summary(pca_sem_influentes)$importance[3, ]
n.pca_sem_influentes <- which(cumsum(acumulado_sem_influentes) >= 0.90)[1]
x.pca_sem_influentes <- pca_sem_influentes$x[ , 1:n.pca_sem_influentes]
modelo.pca_sem_influentes <- lm(dados_sem_influentes$eff.ajust ~ x.pca_sem_influentes)

# Resumo do modelo original
summary(modelo.pca)
AIC(modelo.pca)
BIC(modelo.pca)

# Resumo do modelo ajustado sem alavancas
summary(modelo.pca_sem_alavanca)
AIC(modelo.pca_sem_alavanca)
BIC(modelo.pca_sem_alavanca)

# Resumo do modelo ajustado sem pontos influentes
summary(modelo.pca_sem_influentes)
AIC(modelo.pca_sem_influentes)
BIC(modelo.pca_sem_influentes)

# Plotar a distância de Cook para identificar visualmente os pontos influentes
plot(influentes, type = "h", main = "Distância de Cook", xlab = "Observações", ylab = "Distância de Cook")
abline(h = limite_influencia, col = "red", lty = 2)

##----RETIRANDO PONTOS INFLUÊNTES -----------------------------------------------------------------------------------
# sme 104
dados_sem_influentes2 <- geral[-104, ]
dados_sem_influentes_semvd2 <- dados_sem_influentes2[ , c(3:8)]

pca_sem_influentes2 <- prcomp(dados_sem_influentes_semvd2, scale. = TRUE)
summary(pca_sem_influentes2)
acumulado_sem_influentes2 <- summary(pca_sem_influentes2)$importance[3, ]
n.pca_sem_influentes2 <- which(cumsum(acumulado_sem_influentes2) >= 0.90)[1]
x.pca_sem_influentes2 <- pca_sem_influentes2$x[ , 1:n.pca_sem_influentes2]
modelo.pca_sem_influentes2 <- lm(dados_sem_influentes2$eff.ajust ~ x.pca_sem_influentes2)
summary(modelo.pca_sem_influentes2)
AIC(modelo.pca_sem_influentes2)
BIC(modelo.pca_sem_influentes2)

# sem 104 e 65 ------------------------------------------------------------------------
dados_sem_influentes7 <- geral[-c(104, 65) ]
dados_sem_influentes_semvd7 <- dados_sem_influentes7[ , c(3:8)]

pca_sem_influentes7 <- prcomp(dados_sem_influentes_semvd7, scale. = TRUE)
summary(pca_sem_influentes7)
acumulado_sem_influentes7 <- summary(pca_sem_influentes7)$importance[3, ]
n.pca_sem_influentes7 <- which(cumsum(acumulado_sem_influentes7) >= 0.90)[1]
x.pca_sem_influentes7 <- pca_sem_influentes7$x[ , 1:n.pca_sem_influentes7]
modelo.pca_sem_influentes7 <- lm(dados_sem_influentes7$eff.ajust ~ x.pca_sem_influentes7)
summary(modelo.pca_sem_influentes7)
AIC(modelo.pca_sem_influentes7)
BIC(modelo.pca_sem_influentes7)

# sem 104 e 82
dados_sem_influentes3 <- geral[-c(104, 82), ]
dados_sem_influentes_semvd3 <- dados_sem_influentes3[ , c(3:8)]

pca_sem_influentes3 <- prcomp(dados_sem_influentes_semvd3, scale. = TRUE)
summary(pca_sem_influentes3)
acumulado_sem_influentes3 <- summary(pca_sem_influentes3)$importance[3, ]
n.pca_sem_influentes3 <- which(cumsum(acumulado_sem_influentes3) >= 0.90)[1]
x.pca_sem_influentes3 <- pca_sem_influentes3$x[ , 1:n.pca_sem_influentes3]
modelo.pca_sem_influentes3 <- lm(dados_sem_influentes3$eff.ajust ~ x.pca_sem_influentes3)
summary(modelo.pca_sem_influentes3)
AIC(modelo.pca_sem_influentes3)
BIC(modelo.pca_sem_influentes3)

# sem 104, 82 e 41
dados_sem_influentes4 <- geral[-c(104, 82, 41), ]
dados_sem_influentes_semvd4 <- dados_sem_influentes4[ , c(3:8)]

pca_sem_influentes4 <- prcomp(dados_sem_influentes_semvd4, scale. = TRUE)
summary(pca_sem_influentes4)
acumulado_sem_influentes4 <- summary(pca_sem_influentes4)$importance[3, ]
n.pca_sem_influentes4 <- which(cumsum(acumulado_sem_influentes4) >= 0.90)[1]
x.pca_sem_influentes4 <- pca_sem_influentes4$x[ , 1:n.pca_sem_influentes4]
modelo.pca_sem_influentes4 <- lm(dados_sem_influentes4$eff.ajust ~ x.pca_sem_influentes4)
summary(modelo.pca_sem_influentes4)
AIC(modelo.pca_sem_influentes4)
BIC(modelo.pca_sem_influentes4)

# sem 104, 82, 41 e 65
dados_sem_influentes5 <- geral[-c(104, 82, 41, 65), ]
dados_sem_influentes_semvd5 <- dados_sem_influentes5[ , c(3:8)]

pca_sem_influentes5 <- prcomp(dados_sem_influentes_semvd5, scale. = TRUE)
summary(pca_sem_influentes5)
acumulado_sem_influentes5 <- summary(pca_sem_influentes5)$importance[3, ]
n.pca_sem_influentes5 <- which(cumsum(acumulado_sem_influentes5) >= 0.90)[1]
x.pca_sem_influentes5 <- pca_sem_influentes5$x[ , 1:n.pca_sem_influentes5]
modelo.pca_sem_influentes5 <- lm(dados_sem_influentes5$eff.ajust ~ x.pca_sem_influentes5)
summary(modelo.pca_sem_influentes5)
AIC(modelo.pca_sem_influentes5)
BIC(modelo.pca_sem_influentes5)

# sem 104, 82, 41, 65 e 22
dados_sem_influentes6 <- geral[-c(104, 82, 41, 65, 22), ]
dados_sem_influentes_semvd6 <- dados_sem_influentes6[ , c(3:8)]

pca_sem_influentes6 <- prcomp(dados_sem_influentes_semvd6, scale. = TRUE)
summary(pca_sem_influentes6)
acumulado_sem_influentes6 <- summary(pca_sem_influentes6)$importance[3, ]
n.pca_sem_influentes6 <- which(cumsum(acumulado_sem_influentes6) >= 0.90)[1]
x.pca_sem_influentes6 <- pca_sem_influentes6$x[ , 1:n.pca_sem_influentes6]
modelo.pca_sem_influentes6 <- lm(dados_sem_influentes6$eff.ajust ~ x.pca_sem_influentes6)
summary(modelo.pca_sem_influentes6)
AIC(modelo.pca_sem_influentes6)
BIC(modelo.pca_sem_influentes6)




