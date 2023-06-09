library(readr);
library(dplyr);
library(ggplot2);
library(plyr);
library(factoextra)
library(stats)

dados <- read.csv("C:\\Users\\pedro\\OneDrive\\Documentos\\Inteligencia Artificial\\analiseDeDados\\winequality-whiteAndRed.csv", header = TRUE)

str(dados) 

#1 Identificacao do atributo alvo (saida)
atributo_saida <- as.data.frame(dados$quality) 

#2 Identifica????o dos tipos de dados dos atributos de entrada (quantitativo, qualitativo)
atributo_de_entrada <- data.frame(r1=names(dados), t(dados))
tipo_de_atributos <- as.data.frame(atributo_de_entrada$r1)
tipo_de_atributos <- as.data.frame(tipo_de_atributos[-c(12), ])
colnames(tipo_de_atributos)[1] <- "Atributo"
tipo_de_atributos$Tipo <- c("Quantitativo Continuo", "Quantitativo Continuo",
                            "Quantitativo Continuo","Quantitativo Continuo","Quantitativo Continuo",
                            "Quantitativo Discreto","Quantitativo Discreto", "Quantitativo Continuo", 
                            "Quantitativo Continuo","Quantitativo Continuo","Quantitativo Continuo" ,"Quantitativo Continuo" ,"Quantitativo Continuo")
View(tipo_de_atributos)


#3 Identifica????o da escala de dados dos atributos de entrada (nominal, ordinal, intervalar, racional)
atributo_de_entrada <- data.frame(r1=names(dados) , t(dados))
escala_de_atributos <- as.data.frame(atributo_de_entrada$r1)
escala_de_atributos <- as.data.frame(escala_de_atributos[-c (12) ,])
colnames(escala_de_atributos)[1] <- "Atributo"
escala_de_atributos$Escala <- c("Racional" , "Racional" , "Racional" , "Racional" , "Racional" , "Racional" , "Racional" , "Racional" , "Racional" , "Racional" ,"Raciona" ,"Racional" ,"Racional" )
View(escala_de_atributos)



#4 Exploracao de dados atraves de medidas de localidade

ggplot(dados) +geom_point(aes(x = fixed.acidity, y = 1), color = "blue") +labs(title = "Fixed Acidity")

ggplot(dados) +geom_point(aes(x = volatile.acidity, y = 1), color = "red") +labs(title = "Volatile Acidity")

ggplot(dados) +geom_point(aes(x = citric.acid, y = 1), color = "green") +labs(title = "Citric Acid")

ggplot(dados) +geom_point(aes(x = residual.sugar, y = 1), color = "orange") +labs(title = "Residual Sugar")

ggplot(dados) +geom_point(aes(x = chlorides, y = 1), color = "purple") +labs(title = "Chlorides")

ggplot(dados) +geom_point(aes(x = free.sulfur.dioxide, y = 1), color = "brown") +labs(title = "Free Sulfur Dioxide")

ggplot(dados) +geom_point(aes(x = total.sulfur.dioxide, y = 1), color = "gray") +labs(title = "Total Sulfur Dioxide")

ggplot(dados) +geom_point(aes(x = density, y = 1), color = "pink") +labs(title = "Density")

ggplot(dados) +geom_point(aes(x = pH, y = 1), color = "darkblue") +labs(title = "pH")

ggplot(dados) +geom_point(aes(x = sulphates, y = 1), color = "darkgreen") +labs(title = "Sulphates")

ggplot(dados) +geom_point(aes(x = alcohol, y = 1), color = "magenta") +labs(title = "Alcohol")

ggplot(dados) +geom_point(aes(x = quality, y = 1), color = "cyan") +labs(title = "Quality")

#5 Exploracao de dados atraves de medidas de espalhamento

summary(dados$fixed.acidity)
summary(dados$volatile.acidity)
summary(dados$citric.acid)
summary(dados$residual.sugar)
summary(dados$chlorides)
summary(dados$free.sulfur.dioxide)
summary(dados$total.sulfur.dioxide)
summary(dados$density)
summary(dados$pH)
summary(dados$sulphates)
summary(dados$alcohol)
summary(dados$quality)

#Usando a variancia devido a presenca de poucos outliers
sd(dados$fixed.acidity, na.rm = FALSE)
sd(dados$volatile.acidity, na.rm = FALSE)
sd(dados$citric.acid, na.rm = FALSE)
sd(dados$residual.sugar, na.rm = FALSE)
sd(dados$chlorides, na.rm = FALSE)
sd(dados$free.sulfur.dioxide, na.rm = FALSE)
sd(dados$total.sulfur.dioxide, na.rm = FALSE)
sd(dados$density, na.rm = FALSE)
sd(dados$pH, na.rm = FALSE)
sd(dados$sulphates, na.rm = FALSE)
sd(dados$alcohol, na.rm = FALSE)


#Estava testando esse outro tipo de exploracao de dados, com o primeiro atributo
mean_fixed_acidity <- mean(dados$fixed.acidity)
mean_fixed_acidity
median_fixed_acidity <- median(dados$fixed.acidity)
median_fixed_acidity
quantiles_fixed_acidity <- quantile(dados$fixed.acidity, probs = c(0.25, 0.5, 0.75))
quantiles_fixed_acidity

#6 Exploracao de dados atraves de medidas de distribuicao

hist(dados$fixed.acidity, main = "Histograma de Acidez Fixa")
hist(dados$volatile.acidity, main = "Histograma de Acidez Volatil")
hist(dados$citric.acid, main = "Histograma de Acidez Citrica")
hist(dados$residual.sugar, main = "Histograma de Acucar Residual")
hist(dados$chlorides, main = "Histograma de Cloretos")
hist(dados$free.sulfur.dioxide, main = "Histograma de Dioxido de Enxofre Livre")
hist(dados$total.sulfur.dioxide, main = "Histograma de Dioxido de Enxofre Total")
hist(dados$density, main = "Histograma de Densidade")
hist(dados$pH, main = "Histograma de pH")
hist(dados$sulphates, main = "Histograma de Sulfatos")
hist(dados$alcohol, main = "Histograma de Alcool")
hist(dados$quality, main = "Histograma de Qualidade") 

plot(density(dados$pH)) #grafico comparando um atributo com o outro

#7 Identificacao e separacao do conjunto de teste e treinamento, na qual 0.2 representa 20% usado no conjunto de teste

set.seed(123)
prop_test <- 0.2
num_observacoes <- nrow(dados)
num_teste <- round(prop_test * num_observacoes)
indices_teste <- sample(1:num_observacoes, num_teste)
conjunto_teste <- dados[indices_teste, ]
conjunto_treinamento <- dados[-indices_teste, ]

#8 Identificacao e eliminacao de atributos nao necessarios, nao e necessario pois nao existem atributos altamente correlacionados


#9 Identificacao e eliminacao de exemplos nao necessarios

#Verificando dados duplicados
dados <- unique(dados) 

#10 No caso deste dataset, n??o ser?? necess??rio o uso de t??cnicas de amostragem muito complexas,
#pois o n??mero de exemplos pode ser considerado baixo, sendo poss??vel fazer a an??lise com
#efici??ncia e sem muito custo computacional, portanto, foi feita uma amostragem aleat??ria,
##pegando aleatoriamente 217 elementos da classe majorit??ria e 217 elementos da classe
#minorit??ria(para manter a base balanceada) considerando a seguinte classifica????o.

nrow(dados)

#11 Identifica????o e aplica????o de t??cnicas para minimizar problemas de desbalanceamento (caso n??o seja 
#necess??rio, analisar o porque o datasetBalanceado agora possui o mesmo n??mero de classes majorit??rias e minorit??rias


hist(dados$quality, breaks = 16)
dados <- tibble::rowid_to_column(dados)
sum(dados$quality >= 7, na.rm =TRUE)
sum(dados$quality < 6, na.rm =TRUE)
set.seed(40028922)
classeMinoritaria <- dados[sample(which(dados$quality < 6), 217), ]

set.seed(1234)
classeMajoritaria <- dados[sample(which(dados$quality >= 7), 217), ]

datasetBalanceado <- rbind(classeMinoritaria, classeMajoritaria)
View(datasetBalanceado)

sum(datasetBalanceado$quality >= 7, na.rm = TRUE)
sum(datasetBalanceado$quality < 6, na.rm = TRUE)

#12  Limpeza de dados:
#A Identifica????o e elimina????o de ru??dos ou outliers
#N??o foram utilizadas t??cnicas para elimina????o de ru??dos e outliers.

#A Identifica????o e elimina????o de dados inconsistentes
#N??o foram encontrados dados inconsistentes no dataset.

#C Identifica????o e elimina????o de dados redundantes
datasetBalanceado <- unique(datasetBalanceado)
View(datasetBalanceado)

#D Identifica????o e resolu????o de dados incompletos (ausentes) ??? utiliza????o de alguma t??cnica de preenchimento e justificar
#Como todos os resultados foram FALSE o dataset nao possui nenhum valor nulo
sapply(datasetBalanceado, function(x)all(is.na(x)))

# 13. Identifica????o e convers??o dos tipos de dados (caso n??o seja necess??rio,analisar o porqu??
#Foi considerado que neste dataset n??o seria necess??rio o uso de reescala, pois os limites dos valores dos atributos n??o s??o muito discrepantes entre si e nem mesmo uma convers??o, pois
#todos os valores s??o muito ??teis para serem utilizados da forma que est??o.


#14 An??lise e aplica????o de alguma t??cnica para redu????o de dimensionalidade ??? pesquisar alguma t??cnica utilizada na literatura e aplicar
dados <- iris[, 1:4]
pca <- prcomp(dados, scale = TRUE)
print(summary(pca))

# Plotar um gr??fico de dispers??o dos componentes principais
plot(pca$x[, 1], pca$x[, 2], col = iris$Species, pch = 16,
     xlab = "Componente Principal 1", ylab = "Componente Principal 2",
     main = "PCA - Redu????o de Dimensionalidade")


