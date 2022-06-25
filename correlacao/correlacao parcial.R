library(psych) # for descriptive statistics
library(ppcor) # this pacakge computes partial and semipartial correlations.

#x = variável 1
#Y = variável 2
#Z = variável controle

#correlacao parcial de Pearson controlado por uma unica variavel

pcor.test(banco_dados$variavel1, y=banco_dados$variavel2, z=banco_dados$variavel_controle)

#correlacao partical de Spearman controlado por uma unica variavel

pcor.test(x=banco_dados$variavel1, y=banco_dados$variavel2, z=banco_dados$variavel_controle, method = "spearman")   

#estimate a matrix of the partial correlation coefficient between two variables
#p.value a matrix of the p value of the test
#statistic a matrix of the value of the test statistic
#n the number of samples
#gn the number of given variables
#method the correlation method used

# Fazer correlacao parcial controlado por mais de uma variavel

# Metodo de Pearson

pcor.test(banco_dados$variavel1, banco_dados$variavel2, list(banco_dados$variavel_controle1, banco_dados$variavel_controle2))

# Com o metodo spearman

pcor.test(banco_dadosv$variavel1, banco_dados$variavel2, list(banco_dados$variavel_controle1, banco_dados$variavel_controle2), method = "spearman")