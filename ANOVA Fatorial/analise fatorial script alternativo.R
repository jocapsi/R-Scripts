# Selecionar variáveis 5 a 23 da base completa
dados<-dataset_Jin_2013[c(5:23)]


#<<<<<<<<<<<<<<< Verificar pressupostos
# Normalidade multivariada
ifelse(!require(MVN),install.packages("MVN", dependencies=TRUE),1) # instalar pacote se necessário
require(MVN)
mvn(cpac_filtrados, mvnTest = "hz", desc = FALSE, multivariatePlot = "qq") # transform = "sqrt" ou "square" ; mvnTest = c("mardia", "hz", "royston", "dh", "energy")
# Ausencia de outliers
mvn(cpac_filtrados, multivariateOutlierMethod = "quan", showOutliers = T) # verificar defult datol = 1e-25
mvn(cpac_filtrados, multivariateOutlierMethod = "quan", showOutliers = T, tol = 1e-33)
# Homocedasticidade multivariada
ifelse(!require(biotools),install.packages("biotools", dependencies=TRUE),1) # instalar pacote se necessário
require(biotools)
data(iris) #exemplo
boxM(iris[, -5], iris[, 5]) #exemplo


#<<<<<<<<<<<<<<< Verificar adequação do tamanho da amostra: N >= Max {5 x k, 100}; sendo k: variáveis 
nrow(bancodedados)
ncol(bancodedados)*5


#<<<<<<<<<<<<<<< Verificar adequação da amostra para AFE
# Correlação entre variáveis
ifelse(!require(psych),install.packages("psych", dependencies=TRUE),1) # instalar pacote se necessário
require(psych)
a<-corr.test(cpac_filtrados, method = "pearson");a
ifelse(!require(corrplot),install.packages("corrplot", dependencies=TRUE),1) # instalar pacote se necessário
require(corrplot)
corrplot(a$r, p.mat = a$p, sig.level = 0.05, method = "color", type = "upper") # #method = c("circle", "square", "ellipse", "number", "shade", "color", "pie"), type = c("full", "lower", "upper")
# KMO, MSA e Teste de Esfericidade de Bartlett
ifelse(!require(stats),install.packages("stats", dependencies=TRUE),1) # instalar pacote se necessário
require(stats)
# Teste de Esfericidade de Bartlett
cortest.bartlett(a$r, 140) # default = 100 observações
# KMO e MSA
KMO(a$r)


#<<<<<<<<<<<<<<< Processar AFE
# Rodar AFE forçando  10 fatores, rotação varimax e Bartlett's weighted least-squares scores
afe <- factanal(cpac_filtrados, 10, scores="Bartlett", rotation="varimax") # scores = "regression", rotation = "promax" [oblíqua] ou "none"
# chamar resultados com duas casas decimais, ponto de corte das cargas = 0.3 e classificando as variáveis em ordem de importância das cargas
print(afe, digits=2, cutoff=.30, sort=TRUE) #com ponto de corte e ordenado
#escores fatoriais
afe$scores


#<<<<<<<<<<<<<<< Determinar o numero de fatores a serem retidos (análise gráfica)
ifelse(!require(nFactors),install.packages("nFactors", dependencies=TRUE),1) # instalar pacote se necessário
require(nFactors)
ev <- eigen(cor(cpac_filtrados)) # eigenvalues
ap <- parallel(subject=nrow(cpac_filtrados),var=ncol(cpac_filtrados), rep=1000, cent = .05, quantile =  .05, model = "factors") # model = "factors", "components"
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, cor = TRUE, model = "factors", criteria = 1)
plotnScree(nS)


#<<<<<<<<<<<<<<< Ajustes
# Rodar AFE forçando  8 fatores, rotação varimax e Bartlett's weighted least-squares scores
afe <- factanal(cpac_filtrados, 8, scores="Bartlett", rotation="varimax") 
print(loadings(afe), digits = 2, cutoff =.30, sort = TRUE)
print(loadings(afe), digits = 2, cutoff =.30, sort = TRUE)
# Rodar AFE sem as variáveis "ot5" e "de11", forçando  6 fatores, rotação varimax e Bartlett's weighted least-squares scores
afe <- factanal(cpac_filtrados, 6, scores="Bartlett", rotation="varimax") 
print(loadings(afe), digits = 2, cutoff =.30, sort = TRUE)
# Investigar variáveis
ifelse(!require(PerformanceAnalytics),install.packages("PerformanceAnalytics", dependencies=TRUE),1) # instalar pacote se necessário
require(PerformanceAnalytics)
chart.Correlation(cpac_filtrados, histogram = TRUE)


#<<<<<<<<<<<<<<< Nomear os fatores
colnames(afe$loadings)<-c("Dimensão1","Dimensão2","Dimensão3", "Dimensão4",  "Dimensão5",  "Dimensão6")
print(loadings(afe), digits = 2, cutoff =.45, sort = TRUE)
