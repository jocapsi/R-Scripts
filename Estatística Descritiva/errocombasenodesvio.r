# desvio padraa calculado

N-Back_controle <- c(9.656605256)
N-Back_experimental <- c(7.011877421)
olhosabertos_controle <- c(11.48273316)
olhosabertos_experimental <- c(12.57105478)
Olhosfechados_controle <- c(15.0411409)
ohosfechados_experimental <- c(19.00036445)

# Calcule a raiz quadrada do tamanho da amostra
raiz_tamanho_amostra1 <- sqrt(length(N-Back_controle))
raiz_tamanho_amostra2 <- sqrt(length(N-Back_experimental))
raiz_tamanho_amostra3 <- sqrt(length(olhosabertos_controle))
raiz_tamanho_amostra4 <- sqrt(length(olhosabertos_experimental))
raiz_tamanho_amostra5 <- sqrt(length(Olhosfechados_controle))
raiz_tamanho_amostra6 <- sqrt(length(ohosfechados_experimental))

# Calcule o erro padrão da média (SEM)
erro_padrao_media1 <- N-Back_controle / raiz_tamanho_amostra1
erro_padrao_media2 <- N-Back_experimental / raiz_tamanho_amostra2
erro_padrao_media3 <- olhosabertos_controle / raiz_tamanho_amostra3
erro_padrao_media4 <- olhosabertos_experimental / raiz_tamanho_amostra4
erro_padrao_media5 <- Olhosfechados_controle / raiz_tamanho_amostra5
erro_padrao_media6 <- ohosfechados_experimenta / raiz_tamanho_amostra6

# Exiba o resultado
print(erro_padrao_media1)
print(erro_padrao_media2)
print(erro_padrao_media3)
print(erro_padrao_media4)
print(erro_padrao_media5)
print(erro_padrao_media6)
