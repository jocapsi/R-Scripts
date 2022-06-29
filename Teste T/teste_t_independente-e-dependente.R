#scripts para fazer testes t

#----Instale esses pacotes caso ainda nao tenha feito
install.packages("ggplot2")
install.packages("pastecs")
install.packages("Hmisc")
install.packages("bootstrap")
install.packages("WRS2")

#------Entao carregue os pacotes-----
library(ggplot2)
library(pastecs)
library(reshape)
library(Hmisc)
# caso precise abir o R Commander execute esse codigo >> library(Rcmdr)
library(WRS2)

source("http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v21")


#------------carregue os dados na forma que mais achar melhor ou entao utilize o codigo abaixo para extrair os dados 
#lembre de alterar o caminho para o arquivo

Aranhas <- readSPSS("sua-pasta/SpiderBG.SAV",
           rownames=FALSE, stringsAsFactors=TRUE, tolower=FALSE)


#fazendo graficos
Boxplot(ANXIETY~GROUP, data=Aranhas, id=list(method="y"))

#grafico de barras mais desvio padr?o
bar <- ggplot(Aranhas, aes(GROUP, ANXIETY))
bar + stat_summary(fun = mean, geom = "bar",position="dodge") + stat_summary(fun.data = mean_sdl, geom = "errorbar",
                                                                             position = position_dodge(width=0.90), width = 0.2)
#gr?fico de barras mais IC
                   
bar + stat_summary(fun = mean, geom = "bar",position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
                                                                             position = position_dodge(width=0.90), width = 0.2)

# fazendo teste t independente e verificando homogeneidade

leveneTest(Aranhas[,"ANXIETY"], Aranhas[,"GROUP"], 
           location = "median", correction.method = "zero.correction")

ind.t.test<-t.test(ANXIETY ~ GROUP, data = Aranhas)
ind.t.test



#Teste t independente com mais argumentos na funcao
t.test(ANXIETY~GROUP, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Aranhas)

#Effect sizes

t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#Robust tests and Bootstrap

#Independent groups
yuen(ANXIETY~GROUP, Aranhas, tr=.2)
yuenbt(ANXIETY~GROUP,Aranhas, tr=.2, nboot = 2000)
#CI
pb2gen(ANXIETY~GROUP,Aranhas, nboot=2000, est="mom")


# Agora vamos fazer para amostras pareadas, 
# lembrando que agora os dados estarao organizados de outra forma na planilha

#carregue os dados
#carregue os dados da forma de sua preferencia ou entao use o codigo abaixo 

Spiders <- read.table("sua-pasta/SpiderWide.dat", header=TRUE, stringsAsFactors=TRUE, sep="",
                      na.strings="NA", dec=".", strip.white=TRUE)

# fazendo teste t dependente

dep.t.test<-t.test(Spiders$real, Spiders$picture, paired
                   = TRUE)
dep.t.test

#Effect sizes:
t<-dep.t.test$statistic[[1]]
df<-dep.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)