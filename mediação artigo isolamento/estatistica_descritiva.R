install.packages("dplyr")
install.packages("skimr")
install.packages("janitor")

library(dplyr)
library(skimr)
library(janitor)


# Média, Desvio Padrão e N para variáveis numéricas (ex: Age, Income)
descriptive_stats_numeric <- banco3_apenas_fatores_29_anos %>%
  summarise(
    Mean_Age = mean(idade, na.rm = TRUE),
    SD_Age = sd(idade, na.rm = TRUE),
    N_Age = n(), # Contagem total de observações
    Mean_Filhos = mean(filhos, na.rm = TRUE),
    SD_Filhos = sd(filhos, na.rm = TRUE),
    N_Filhos = n() # Contagem total de observações
  )

print(descriptive_stats_numeric)


# Se quiser para todas as variáveis numéricas de uma vez (usando skimr)
skim_summary <- skim(banco29anos)
print(skim_summary)

# Frequência e Porcentagem para Gender
gender_freq <- banco3_apenas_fatores_29_anos %>%
  tabyl(genero) %>%
  adorn_percentages("row") %>% # Adiciona porcentagens
  adorn_pct_formatting() %>%   # Formata como porcentagem
  adorn_ns()                   # Adiciona a contagem (N)
print(gender_freq)

# Frequência e Porcentagem para MaritalStatus
marital_status_freq <- banco3_apenas_fatores_29_anos %>%
  tabyl(estado_civil) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns()
print(marital_status_freq)

# Frequência e Porcentagem para Filhos
education_freq <- banco3_apenas_fatores_29_anos %>%
  tabyl(filhos) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting() %>%
  adorn_ns()
print(education_freq)