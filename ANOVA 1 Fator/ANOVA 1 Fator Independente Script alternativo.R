# Rotina para a aula de Anova

# Primeiro para Anova independente

######Install packages, just in case
install.packages("granova")
install.packages("car")
install.packages("pastecs")
install.packages("multcomp")
install.packages("compute.es")

#Initiate packages
library(ggplot2)
library(granova)
library(car)
library(Rcmdr)
library(pastecs)
library(multcomp)
library(compute.es)
library(WRS2)

#--------Viagra data----------
viagraData <- 
  read.table("/home/yourfolderViagra.dat", 
             header=TRUE, stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
viagraData <- within(viagraData, {
  dose <- factor(dose, labels=c('placebo','low dose','high dose'))})

#Gr?fico de linha
line <- ggplot(viagraData, aes(dose, libido))
line + stat_summary(fun = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + stat_summary(fun = mean, geom = "point", size = 4, colour = "#990000") + stat_summary(fun = mean, geom = "point", size = 3, colour = "#FF6633") + labs(x = "Dose of Viagra", y = "Mean Libido")  

#gr?fico de barra (m?dia +- IC)
bar <- ggplot(viagraData, aes(dose, libido))
bar + stat_summary(fun = mean, geom = "bar",position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar",

                                                                                                                                                          position = position_dodge(width=0.90), width = 0.2)
#teste de normalidade

Histogram <- ggplot(viagraData, aes(group=1, libido)) 
Histogram + geom_histogram(binwidth = 0.5) + labs(x = "Subjects", y = "Frequency")

shapiro.test (viagraData$libido)

#verificando homogeneidade das vari?ncias com teste de Levene

leveneTest(viagraData$libido, viagraData$dose, center = median)

#Teste Anova
viagraModel<-aov(libido ~ dose, data = viagraData)
summary(viagraModel)
summary.lm(viagraModel)

#caso nao haja homogeneidade das variancias, faca correlacao de Welch nos graus de liberdade

oneway.test(libido ~ dose, data = viagraData)

#--------Post Hoc Tests----------

pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "bonferroni")
pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "BH")


postHocs<-glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)

postHocs<-glht(viagraModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(postHocs)
confint(postHocs)

lincon(viagraWide, tr = .1)

mcppb20(viagraWide, tr = .2, nboot = 2000)

lincon(viagraWide)
mcppb20(viagraWide)