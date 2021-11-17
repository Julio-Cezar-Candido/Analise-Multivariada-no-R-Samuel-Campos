#Vamos importar o banco de dados "estados.xlsx"
#Nele teremos 
#Populacao	
#Pib	
#Homicidios.100mil	
#Analfabetos.10a14anos
#Anos.de.estudos

#Vamos obter apenas os dados numéricos
#Perceba que os dados da coluna 1 a 3 não são numéricos
#Então, iremos salvar as colunas 4 a 8 em outro banco de dados
dados <- estados[,4:8]

#Criando um matriz de var-cov
#Calculando a matriz de var-cov, por meio da função var()
varcov <- var(dados)

#Vizualizando apenas parte da matriz (1ªa 3{} linha e 1ª a 3ª colunas usando o subscrito)
varcov[1:3,1:3]
#Temos uma matriz de variância e covariância. 
#Na diagonal principal temos a variância (sii) e os demais elementos são as covariâncias (sij, 
# para i diferente de j) entre as variáveis
#A magnitude da variância e covariância depende da escala dos dados!
#Valores positivos (negativos) indicam que valores xi acima (abaixo) da média mu tentem a estar 
#associados a valores de xj acima (abaixo) da média. Assim, se sij > 0, temos que à medida que os 
#valores da variável xi aumentam, também se elevam os valores da variável xj

#Podemos usar a função cov também
cov(dados)

#Podemos também, obter a matriz de correlação por meio da função cor()
#Na diagonal principal temos a correlação rii.

cor(dados)

#Esse valor é equivalente à variância das variáveis padronizadas com média 0 e 
# variância =1
#Os elementos fora da digonal principal são equivalentes à covariância entre 
# as variáveis padronizadas!

#Vamos padronizar os dados e testar empiricamente
#Instalando o pacote que contém a função necessária para normalizar os dados
# z= (x-xmédia)/(desvio padrão)
install.packages("BBmisc")
#Carregando o pacote na mémória do programa
library(BBmisc)
#Normalizando
dados.n <-normalize(dados)
#Obtendo estatísticas descritivas para os dados
summary(dados.n)
# Obtendo a matriz de variância e covariancia dos dados
cov(dados.n)
#Observe que essa matriz é igual à matriz de correlação

#Calculando a variância total, o tr(varcov)
sum(diag(varcov))

#Calculando a variância generalizada, o determinante
det(varcov)
#Fornece uma noção da dispersão global da distribuição multivariada
#É influenciada pelas covariâncias entre as variáveis


###########################################################
#Seja uma matriz de variância e covarância
M1 <- matrix(c(8,-2,-2,5), nrow=2, ncol=2)

M1 

auto<-eigen(M1)

auto

#Temos que a matriz é de ordem, então teremos dois autovalores e dois autovetores

#Os autovalores e autovetores são importantes pois eles decompoem a matriz original
#Vamos reconstruir a matriz original
#Obtendo os autovalores
autovalores <-auto$values
autovalores
#e os autovetores
autovetores <-auto$vectors
autovetores
#Tememos então, o primeiro autovalor * a primeira coluna * transposta da primeira coluna +
#               o segundo autovalor * a segunda coluna * transposta da segunda coluna +

M2 <- autovalores[1]*autovetores[1,]%*%t(autovetores[1,])+
      autovalores[2]*autovetores[2,]%*%t(autovetores[2,])


M2
#Então, pelo Teorema da Decomposição Espectral, a matriz de corelação amostral ou matriz de
# variâncias e covariâncias pode ser decomposta como a soma de "p" matrizes, cada uma 
# relacionada com um autovetor da matriz corelação amostral ou da matriz varcov.





M3 <- matrix(c("a11","a12", "a13", 
              "a21", "a22", "a23",
              "a31", "a32", "a33"), nrow=3, ncol=3,
             dimnames = list( c("lin1","lin2","lin3"),c("col1","col2","col3")))
xtable(M3)
