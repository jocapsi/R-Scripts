# Exemplo de dados
dados <- c(2, 4, 4, 4, 5, 5, 7, 9)

# Calcule o desvio padrão da amostra
desvio_padrao <- sd(dados)

# Calcule a raiz quadrada do tamanho da amostra
raiz_tamanho_amostra <- sqrt(length(dados))

# Calcule o erro padrão da média (SEM)
erro_padrao_media <- desvio_padrao / raiz_tamanho_amostra

# Exiba o resultado
print(erro_padrao_media)
