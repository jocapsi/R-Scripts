######################### Regrecao Linear Simples #########################

# Passo 1: Carregar os pacotes que serao usados

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpubr)

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretorio de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory

dados <- read.csv2('arquivo.csv', stringsAsFactors = T) # Carregamento do arquivo csv
View(Album_Sales_1)                                 # Visualizacao dos dados em janela separada
glimpse(Album_Sales_1)                              # Visualizacao de um resumo dos dados

# Passo 3: Verificacao dos pressupostos para a regress?o linear

## Relatio linear entre a VD e a VI:
### VD: Vendas
### VI: Publicidade

plot(Album_Sales_1$adverts, Album_Sales_1$sales)

## Construcao do modelo:
mod <- lm(sales ~ adverts, Album_Sales_1)


## Analise grafica:

par(mfrow=c(2,2))

plot(mod)

### Interpretacao: https://data.library.virginia.edu/diagnostic-plots/

par(mfrow=c(1,1))

## Normalidade dos residuos:
shapiro.test(mod$residuals)

## Outliers nos residuos:
summary(rstandard(mod))

## Independencia dos res?duos (Durbin-Watson):
durbinWatsonTest(mod)

## Homocedasticidade (Breusch-Pagan):
bptest(mod)

# Passo 4: Analise do modelo
summary(mod)

ggplot(data = Album_Sales_1, mapping = aes(x = adverts, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,
                                          sep = "*plain(\",\")~~")),
                        label.x = 0, label.y = 400) +
  theme_classic()

# https://pt.stackoverflow.com/questions/6979/como-colocar-a-equa%C3%A7%C3%A3o-da-regress%C3%A3o-em-um-gr%C3%A1fico