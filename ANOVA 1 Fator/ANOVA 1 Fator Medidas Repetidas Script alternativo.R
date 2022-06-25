########## Anova de medidas repetidas###################

#instale os pacotes necess?rios caso ainda n?o os tenha instalado

install.packages("compute.es")
install.packages("ez")
install.packages("multcomp")
install.packages("nlme")
install.packages("pastecs")
install.packages("reshape")

#Initiate packages
library(compute.es)
library(ez)
library(ggplot2)
library(multcomp)
library(nlme)
library(pastecs)
library(reshape)
library(WRS2)

#entrar com os dados (n?o se esque?a de mudar o caminho!)

bushData <- 
  read.table("C:/Users/Fl?vio Barbosa/Dropbox/UFPB/Disciplinas/aulas/Estat?stica/R/Chapter13/Bushtucker.dat",
             header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)

#convertando os dados de colunas para linhas
longBush<-melt(bushData, id = "participant", measured =
                 c("stick_insect", "kangaroo_testicle", "fish_eye",
                   "witchetty_grub"))
names(longBush)<-c("Participant", "Animal", "Retch")

#renomeando as vari?veis

longBush$Animal<-factor(longBush$Animal, labels = c("Stick
Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))

#teste de normalidade

Histogram_Insect <- ggplot(bushData, aes(stick_insect)) 
Histogram_Insect + geom_histogram(binwidth = 0.5) + labs(x = "Insect", y = "Frequency")
shapiro.test (bushData$stick_insect)

Histogram_Kangaroo <- ggplot(bushData, aes(kangaroo_testicle)) 
Histogram_Kangaroo + geom_histogram(binwidth = 0.5) + labs(x = "Kangaroo testicle", y = "Frequency")
shapiro.test(bushData$kangaroo_testicle)

Histogram_fish <- ggplot(bushData, aes(fish_eye)) 
Histogram_fish + geom_histogram(binwidth = 0.5) + labs(x = "Fish eye", y = "Frequency")
shapiro.test(bushData$fish_eye)

Histogram_witche <- ggplot(bushData, aes(witchetty_grub)) 
Histogram_witche + geom_histogram(binwidth = 0.5) + labs(x = "Wicthetty grub", y = "Frequency")
shapiro.test(bushData$witchetty_grub)



#fazendo gr?fico de barras (m?dia e Intervalo de confian?a)

bushBar <- ggplot(longBush, aes(Animal, Retch))
bushBar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Type of Animal Eaten", y = "Mean Time to Retch (Seconds)") 

#using ezAnova: perceber que essa sa?da ? a mais pr?xima do spss e do que usamos 
#na parte te?rica da aula;
# o tamanho de efeito pode ser extra?do da coluna ges (eta ao quadrado)

bushModel<-ezANOVA(data = longBush, dv = .(Retch), wid = .(Participant),  within = .(Animal), type = 3, detailed = TRUE)
bushModel

#fazendo um test post hoc (bonferroni)

pairwise.t.test(longBush$Retch, longBush$Animal, paired = TRUE, p.adjust.method = "bonferroni")

