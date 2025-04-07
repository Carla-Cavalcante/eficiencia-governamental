# Bancos de dados usados ---------------------------------------------------------------------------------------

vd <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/VD.csv")
vi <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/VI.csv")
geral <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/geral.csv")

## bancos por categoria ----------------------------------------------------------------------------------------
# divisÃ£o por tipo de democracia

aut_fechada <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/autocracia_fechada.csv")

aut_eleitoral <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/aut_eleitoral.csv")

zona_cinz <-  read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/zona_cinzenta.csv")

democr_eleitoral <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/democr_eleitoral.csv")

democracia_liberal <- read.csv("C:/Users/user/Downloads/Trabalho_funcionarios/democracia_liberal.csv")

## plot ----------------------------------------------------------------------
# GrÃ¡fico das relaÃ§Ãµes
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

## DEA ----------------------------------------------------------------
#install.packages("Benchmarking")
library(Benchmarking)

#ordenando em matrizes
x <- as.matrix(vd[ , c(3:7)])
y <- as.matrix(vd[ , c(8:10)])

# ajustando os valores negativos
constante_cc <- abs(min(x[ , 1])) # extraindo o valor mÃ­nimo da coluna cc
constante_rq <- abs(min(x[ , 2]))
x[ , 1] <- x[ , 1] + constante_cc # somando a costantes (advinda do valor mÃ­nimo da coluna) com cada observaÃ§Ã£o
x[ , 2] <- x[ , 2] + constante_rq

dea_ge <- dea(x, y, RTS = "vrs", ORIENTATION = "in") #utilizando DEA nos inputs e outputs com o modelo BCC
dea_ge_bias <- dea.boot(x, y, RTS = "vrs", ORIENTATION = "in") #bootstrap das eficiÃªncias

vd$eff.DEA <- efficiencies(dea_ge)
vd$eff.ajust <- dea_ge_bias$eff.bc
vd$bias <- dea_ge_bias$bias

## CorrelaÃ§Ã£o
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

## PCA -
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

pesos$PC1.pesos <- pesos$PC1 * -0.026386
pesos$PC2.pesos <- pesos$PC2 * -0.008338
pesos$peso.final <- pesos$PC1.pesos + pesos$PC2.pesos
coeficientes <- pesos[ , c(1, 6)]
coeficientes # aqui temos o impacto de cada variÃ¡vel no modelo


## modelo por tipo de democracia  --------------------------------------------------------

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

pesos2 <- c(-0.2596976, -0.2802606, -0.4877775, -0.4253547, 0.4357248, 0.4952793) * 0.025102
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

pesos3 <- c(0.2480164, 0.2586244, 0.5081671, 0.3069439, -0.5139105, -0.5050236) * -0.020860
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
pca_d3$rotation

n.pca_d1.new3 <- which(cumsum(acumulado_d3) >= 0.50)[1]
x.pca_d1.new3 <- pca_d3$x[ , 1:n.pca_d1.new3]
modelo.pca_d1.new3 <- lm(zona_cinz$eff.ajust ~ x.pca_d1.new3)
summary(modelo.pca_d1.new3)

pesos4 <- c(-0.01774978, 0.37015104, 0.47721927, 0.38502869, -0.51993673, -0.46513828) * -0.021674
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

pesos5 <- c(-0.2592667, -0.4818556, -0.5343130, -0.1512413, 0.5356153, 0.3245736) *  0.022515
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

pesos6 <- c(-0.3298425, -0.4660928, -0.4795727, -0.1045581, 0.4771551, 0.4531690) *  0.008426
coeficientes6 <- data.frame(
  variaveis = c("lib.democracy", "va", "rl", "pp.urbana", "PS.legitimacy", "F.economy"),
  PC1 = pesos6
)

#-----------------------------------
install.packages("kableExtra")
library(knitr)
library(kableExtra)

tabela_eff <- vd[ , c(2, 11, 12, 13)]
tabela_eff$eff.DEA <- round(tabela_eff$eff.DEA, 3)
tabela_eff$bias <- round(tabela_eff$bias, 3)
tabela_eff$eff.ajust <- round(tabela_eff$eff.ajust, 3)
tabela_eff$eff.ajust

kable(tabela_eff, format = "latex", booktabs = TRUE,
      caption = "EficiÃªncia tÃ©cnica, viÃ©s estimado e eficiÃªncia ajustada dos paÃ­ses.",
      align = "lccc") %>%
  kable_styling(latex_options = c("hold_position", "striped", "scale_down"))
