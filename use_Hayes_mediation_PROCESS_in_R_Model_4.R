#Notes:
#1. The author accepts no responsibility for the topicality, correctness, completeness, or quality of the information provided.
#2. This pdf is part of a YouTube tutorial: https://youtu.be/4Aq3RRRu0DI 
#3. This pdf is for your own personal use only. Please do not distribute further.


# Read the data
data_mediation <-
  read.csv("https://raw.githubusercontent.com/tidydatayt/mediation_analysis/m
ain/mediation_hypothetical_data.csv")

# show the first 6 row of the data
head(data_mediation)

# set seed
set.seed(123)

# key mediation statement
process(data = banco_r, y = "depressao", x = "idade", m ="autocomp_geral", model = 4)

# multiple mediatio analysis

process(data = data_mediation, y = "Y", x = "X", m =c("M1", "M2"), model = 4)
