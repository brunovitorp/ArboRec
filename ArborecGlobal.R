library(readr)
library(bnlearn)
library(e1071)
library(caret)
require(rpart)
require(rpart.plot)
require(party)
require(partykit)
require(tree)
library(adabag)

#Carga da base
ArboRec <- read_delim("ArboRec.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Transformando o tp_classificacao_final em: Descartado, Inconclusivo, Dengue
ArboRec$tp_classificacao_final = ifelse(ArboRec$tp_classificacao_final == 5, 'Descartado', ifelse(ArboRec$tp_classificacao_final == 8, 'Inconclusivo', 'Dengue'))
table(ArboRec$tp_classificacao_final)

#Modificando os tipos de dados para Factor
ArboRec$tp_classificacao_final <- as.factor(ArboRec$tp_classificacao_final)
ArboRec$co_distrito_residencia <- as.factor(ArboRec$co_distrito_residencia)
ArboRec$tp_sexo <- as.factor(ArboRec$tp_sexo)
ArboRec$tp_gestante <- as.factor(ArboRec$tp_gestante)
ArboRec$tp_raca_cor <- as.factor(ArboRec$tp_raca_cor)
ArboRec$tp_escolaridade <- as.factor(ArboRec$tp_escolaridade)
ArboRec$co_distrito_residencia <- as.factor(ArboRec$co_distrito_residencia)
ArboRec$no_bairro_residencia <- as.factor(ArboRec$no_bairro_residencia)
ArboRec$tp_zona_residencia <- as.factor(ArboRec$tp_zona_residencia)
ArboRec$tp_result_exame <- as.factor(ArboRec$tp_result_exame)

#Recortando a base para apenas as colunas usadas para o modelo
ArboRecFinal <- ArboRec[, c(14:17, 21, 23, 26, 57, 68, 74)]

#table(ArboRecFinal$tp_result_exame)
plot(ArboRecFinal$tp_classificacao_final)

#Criação da base de treinamento e de teste
particaoArbo = createDataPartition(1:nrow(ArboRecFinal), p=.7) 
treinoArbo = ArboRecFinal[particaoArbo$Resample1,] 
testeArbo = ArboRecFinal[- particaoArbo$Resample1,] 

### Análise de NaiveBayes ### 

arboNaiveBayes = naiveBayes(tp_classificacao_final ~., treinoArbo)
arboNaiveBayes

previsaoArbo = predict(arboNaiveBayes, testeArbo) 
confusionMatrix(previsaoArbo, testeArbo$tp_classificacao_final)

### Adaboost ### Demorou muito!

adabagArbo <- boosting(tp_classificacao_final ~ ., data=treinoArbo, boos=TRUE, mfinal=20, coeflearn='Breiman')
plot(errorevol(adabagArbo,treinoArbo))

previsaoArbo = predict(adabagArbo, testeArbo) # criar predição
confusionMatrix(as.factor(previsaoArbo$class), testeArbo$tp_classificacao_final) # matriz de confusão


### Criação de Arvore de Decisão ### 

arboAtree <- ctree(tp_classificacao_final ~ notificacao_ano + co_distrito_residencia + tp_result_exame, treinoArbo)

plot(arboAtree, type="simple", gp = gpar(fontsize = 5))

previsaoCtreeArbo = predict(arboAtree, testeArbo)
previsaoCtreeArbo = as.data.frame(previsaoCtreeArbo)

confusionMatrix(previsaoCtreeArbo$previsaoCtreeArbo, testeArbo$tp_classificacao_final)

save(arboAtree, file="ctreeModel.Rdata")

# Árvores de Entropia
arboRpart <- rpart(tp_classificacao_final ~ notificacao_ano + co_distrito_residencia + tp_raca_cor + tp_escolaridade + tp_zona_residencia, treinoArbo) # criar árvore
rpart.plot(arboRpart)

# plotar entropia da árvore - nível de complexidade para arvore acima (evolução)
plotcp(arboRpart) 

# podar árvore
arboRpart <- prune(arboRpart, cp=0.041) 

# plotar árvore podada
rpart.plot(arboRpart, extra = 104, branch.lty = 2,
           nn= T, tweak = 1.2)

# realizar predição
previsaoPartArbo = predict(arboRpart, testeArbo, type = "class")

# transformar predições em data frame
previsaoPartArbo = as.data.frame(previsaoPartArbo)

# Matriz de confusão
confusionMatrix(previsaoPartArbo$previsaoPartArbo, testeArbo$tp_classificacao_final)

save(arboRpart, file="arboRpart.Rdata")