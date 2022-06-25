#importar dados da forma que você preferir

#mostra os dados importados
dataset
#sumario dos dados
summary(variavel1(variavel2))

######Initiate packages

#If you don't have ggplot2 installed then use:
#install.packages(c("ggplot2", "plyr"))


#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)

#teste de normalidade

shapiro.test(dataset$`variavelx`)
shapiro.test(dataset$`variavely`)
shapiro.test(dataset$`variavelz`)


#gerar um scatter simples
scatter <- ggplot(dataset, aes(variavelx), variavely))
scatter + geom_point() + labs(x = "variavel label x", y = "variavel label y") 

#saveInImageDirectory("04 Exam Scatter.png")


#correlacao de Pearson

with(dataset, cor.test(altura, peso, alternative="two.sided", method="pearson"))

#correlacao de Spearman

with(dataset, cor.test(fantasia, tempo, alternative="two.sided", method="spearman"))

