#Media, minimo e máximo
summary(banco_spss$idade)
#desvio padrão
sd(banco_spss$idade) # standard deviation

# tabela de proporção com porcentagem

library(questionr)

#sexo
questionr::freq(banco_spss$sexo, cum = TRUE, sort = "dec", total = TRUE)
#estado civil
questionr::freq(banco_spss$estado_civil, cum = TRUE, sort = "dec", total = TRUE)
#ocupacao
questionr::freq(banco_spss$ocupacao, cum = TRUE, sort = "dec", total = TRUE)

# outra opção

library(gmodels) #uma alternativa não obrigatória
CrossTable(banco_spss$estado_civil)

# Tabela Cruzada de frequência por grupo sexo para estado civil e ocupação

library(crosstable)
ct1 = crosstable(banco_spss, c(estado_civil, ocupacao), by=sexo, total="both", 
                 percent_pattern="{n} ({p_row}/{p_col})", percent_digits=0) %>%
  as_flextable()
ct1

#if you need these descriptive statistics by group use the by() function:

# Estatista descritiva da idade por sexo e desvio padrão da idade por sexo

by(banco_spss$idade, banco_spss$sexo, summary)

by(banco_spss$idade, banco_spss$sexo, sd)

# tabela de proporção para dados qualitativos por grupo (ex. sexo)

table(banco_spss$estado_civil, banco_spss$sexo)

table(banco_spss$ocupacao, banco_spss$sexo)


#descritivos escalas

# Modelo EFA, voce pode repetir esse codigo para outras escalas e  fatores

#Media, minimo e máximo
summary(banco_spss$efa)
#desvio padrão
sd(banco_spss$efa) # standard deviation

#Analise de confiabiliade

library(ltm)

#Alfa EFA

#enter survey responses as a data frame
EFAS <- data.frame(banco_spss$EFA1,banco_spss$EFA2,banco_spss$EFA3,banco_spss$EFA4,banco_spss$EFA5,banco_spss$EFA6,banco_spss$EFA7,banco_spss$EFA8,banco_spss$EFA9,banco_spss$EFA10,banco_spss$EFA11,banco_spss$EFA12,banco_spss$EFA13,banco_spss$EFA14,banco_spss$EFA15,banco_spss$EFA16,banco_spss$EFA17,banco_spss$EFA18,banco_spss$EFA19,banco_spss$EFA20,banco_spss$EFA21,banco_spss$EFA22,banco_spss$EFA23,banco_spss$EFA24,banco_spss$EFA25,banco_spss$EFA26,banco_spss$EFA27,banco_spss$EFA28,banco_spss$EFA29,banco_spss$EFA30,banco_spss$EFA31,banco_spss$EFA32,banco_spss$EFA33,banco_spss$EFA34,banco_spss$EFA35,banco_spss$EFA36,banco_spss$EFA37,banco_spss$EFA38,banco_spss$EFA39,banco_spss$EFA40,banco_spss$EFA41,banco_spss$EFA42)

#calculate Cronbach's Alpha
cronbach.alpha(EFAS)

# repetir o codigo para mais fatores e itens de uma outra escala