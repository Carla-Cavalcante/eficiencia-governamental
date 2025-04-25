wgi <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/ge_wgi.csv")
ge_wgi <- wgi[wgi$indicator == "ge" & wgi$year == 2023, ]
va_wgi <- wgi[wgi$indicator == "va" & wgi$year == 2023, ]
rq_wgi <-  wgi[wgi$indicator == "rq" & wgi$year == 2023, ]
rl_wgi <- wgi[wgi$indicator == "rl" & wgi$year == 2023, ]
cc_wgi <- wgi[wgi$indicator == "cc" & wgi$year == 2023, ]
unique(wgi$indicator)
wgi_geral <- data.frame(
  pais = wgi$countryname,
 ge = ge_wgi$estimate, 
 va = va_wgi$estimate,
 rq = rq_wgi$estimate,
 rl = rl_wgi$estimate,
 cc = cc_wgi$estimate
)
head(wgi_geral)

## 
#install.packages("readxl")
library(readxl)
## liberal democracy index 
liberal_democracy <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/liberal_democracy.csv")
lib.democracy <- liberal_democracy[liberal_democracy$Year == 2023, -2]
lib.democracy <- lib.democracy[ , -2]
colnames(lib.democracy) <- c("pais", "lib.democracy")
head(lib.democracy)
geral <- merge(lib.democracy, wgi_geral, by = "pais", all = TRUE)
geral1 <- unique(geral)


## participatory democracy
p_democracy <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/participatory_democracy.csv")
p_democracy <- p_democracy[ , -c(2, 3, 5)]
colnames(p_democracy) <- c("pais", "p.democracy")
geral2 <- merge(geral1, p_democracy, by = "pais")


## percentual_popurbana 
percent_popUrb <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/percetual_purbana.csv")
pp_urbana <- percent_popUrb[ , c(3, 16)]
colnames(pp_urbana) <- c("pais", "pp.urbana")
geral3 <- merge(geral1, pp_urbana, by = "pais")

## state legimacy index 
legitimacy <- read_xlsx("C:/Users/carla/Downloads/Trabalho_funcionarios/state_legitimacy_index.xlsx")
PS_legitimacy <- legitimacy[legitimacy$Indicator == "Fragile States Index - Public Services", c(2, 26)]
colnames(PS_legitimacy) <- c("pais", "PS.legitimacy")
geral4 <- merge(geral3, PS_legitimacy, by = "pais")
summary(PS_legitimacy$PS.legitimacy)

## state legitimacy

SL_legitimacy <- legitimacy[legitimacy$Indicator == "Fragile States Index - State Legitimacy", c(2, 26)]
colnames(SL_legitimacy) <- c("pais", "SL.legitimacy")
geral5 <- merge(geral4, SL_legitimacy, by = "pais")

## economia fragilizada 
F_economy <- legitimacy[legitimacy$Indicator == "Fragile States Index - Economy", c(2, 26)]
colnames(F_economy) <- c("pais", "F.economy")
geral6 <- merge(geral4, F_economy, by = "pais")

## DIRECT POPULAR VOTER INDEX 
popular_vote <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/popular_vote.csv")
p_vote <- popular_vote[popular_vote$Year == 2023, -c(2, 3)]
colnames(p_vote) <- c("pais", "popular.vote")
geral7 <- merge(geral6, p_vote, by = "pais")


## gato publico com educação 
gp_education <- read_xls("C:/Users/carla/Downloads/Trabalho_funcionarios/Government_expenditure.xls")
ncol(gp_education)
gp_educ <- gp_education[ , c(1, 225)]
colnames(gp_educ) <- c('pais', "gp.educ")
gp_educ$gp.educ <- as.numeric(gp_educ$gp.educ)
gp_educ$gp.educ <- round(gp_educ$gp.educ, digits = 3) 
geral8 <- merge(geral7, gp_educ, by = "pais")

## densidade populacional - The number of people per km² of land area
pop_density <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/population_density.csv")
pop.density <- pop_density[ , c(1, 4)]
colnames(pop.density) <- c("pais", "pop.density")
pop.density$pop.density <- round(pop.density$pop.density, 2)
geral9 <- merge(geral8, pop.density, by = "pais")

## gini
gini_index <- read.csv("C:/Users/carla/Downloads/Trabalho_funcionarios/gini_index.csv")
head(gini_index)
gini.2023 <- gini_index[gini_index$Year == 2023, c(1, 4)]
colnames(gini) <- c("pais", "gini")
unique(gini.2023, incomparables = FALSE, MARGIN = 1,
       fromLast = FALSE)

head(geral9)

####------------~
geral7$va <- as.numeric(geral7$va)
geral7$rq <- as.numeric(geral7$rq)
geral7$rl <- as.numeric(geral7$rl)
geral7$cc <- as.numeric(geral7$cc)
geral7$pp.urbana <- as.numeric(geral7$pp.urbana)
###--------------------


