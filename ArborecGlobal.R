library(readr)
library(bnlearn)
library(e1071)
library(caret)
require(rpart)
require(rpart.plot)
require(party)
require(partykit)
require(tree)

ArboRec <- read_delim("ArboRec.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#View(ArboRec)

ArboRec$tp_classificacao_final <- as.factor(ArboRec$tp_classificacao_final)

plot(ArboRec$tp_classificacao_final)

particaoArbo = createDataPartition(1:nrow(ArboRec), p=.7) 
treinoArbo = ArboRec[particaoArbo$Resample1,] 
testeArbo = ArboRec[- particaoArbo$Resample1,] 

##Análise de NaiveBayes

arboNaiveBayes = naiveBayes(tp_classificacao_final ~  notificacao_ano + co_distrito_residencia + tp_raca_cor + tp_zona_residencia, treinoArbo)

arboNaiveBayes

previsaoArbo = predict(arboNaiveBayes, testeArbo) 
confusionMatrix(previsaoArbo, testeArbo$tp_classificacao_final)

##Criação de Arvore de Decisão

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
arboRpart <- prune(arboRpart, cp=0.048) 

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

