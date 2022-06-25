
## Ler banco de dados

read.csv ('/home/youfolder/data.csv')

summary (data.csv)
matcor <- cor(data.csv)
print(matcor, digits = 2)
require (corrplot)
corrplot(matcor, method="circle")
#install.packages("psych")
require (psych)
cortest.bartlett (data.csv)
KMO (data.csv)
fit<-princomp (data.csv,cor=TRUE)
fit
summary(fit)
screeplot(fit)
plot(fit,type="lines")
PCAdente<-principal(cpac_filtrados, nfactors=2,
                    n.obs=30,rotate="none", scores=TRUE)
PCAdente

PCAdentevarimax<-principal(data.csv, nfactors=2,
                           n.obs=30,rotate="varimax",scores=TRUE)
PCAdentevarimax

PCAdentevarimax$values
PCAdentevarimax$loadings
biplot(PCAdentevarimax)

factor.scores(cpac_filtrados,PCAdentevarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)
