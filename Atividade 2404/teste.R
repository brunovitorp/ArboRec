library(adabag)
library(bnlearn)
library(caret)
library(C50)
library(data.table)
library(DMwR)
library(dplyr)
library(e1071)
library(ggplot2)
library(pROC)
library(randomForest)
library(readr)
library(rpart)

t1 <- Sys.time()
eleitores <- read.csv2("eleitores_jovens.csv", sep = ';')
Sys.time()-t1

t2 <- Sys.time()
eleitores <- fread("eleitores_jovens.csv", showProgress = TRUE, encoding = 'UTF-8', stringsAsFactors = TRUE)
Sys.time()-t2

colnames(eleitores) <- c('Turno', 'UF', 'Genero', 'Estado_Civil', 'Idade', 'Escolaridade', 'Situacao')

particao = createDataPartition(1:nrow(eleitores),p=.7) 
treino = eleitores[particao$Resample1,] 
teste = eleitores[- particao$Resample1,]
testeResultados <- data.frame(Class = teste$Situacao)

treinoDown <- downSample(x = treino[, -7], y = treino$Situacao, yname = 'Situacao')

treinoUp <- upSample(x = treino[, -7], y = treino$Situacao, yname = 'Situacao')

t3 <- Sys.time()
nb = naiveBayes(Situacao ~ . -Turno - UF, treinoUp)
Sys.time()-t3

previsao = predict(nb, teste) 
confusionMatrix(previsao, teste$Situacao)

t4 <- Sys.time()
forest = randomForest(treinoUp[,3:6], treinoUp$Situacao, ntree = 10, importance = TRUE)
Sys.time()-t4

varImpPlot(forest)
forest
forest$importance
plot(forest)

previsao  = predict(forest, teste)
confusionMatrix(previsao, teste$Situacao)

costMatrix <- matrix(c(1, 0.5, 0.5,1), ncol = 2)
costMatrix
rownames(costMatrix) <- levels(treino$Situacao)
colnames(costMatrix) <- levels(treino$Situacao)
costMatrix

t6 <- Sys.time()
cost.dt <- train(x = treino[,3:6], 
                 y = treino$Situacao,
                 method = "rpart", 
                 metric = "Kappa", 
                 tuneLength = 5, 
                 parms = list(loss = costMatrix))
Sys.time()-t6

testeResultados$DT <- predict(cost.dt, teste, type = "prob")[,1]
dt.ROC.cost <- roc(testeResultados$Class, testeResultados$DT, levels = rev(levels(testeResultados$Class)))
dt.ROC.cost

dtEvalCM.cost <- confusionMatrix(predict(cost.dt, teste), testeResultados$Class)
dtEvalCM.cost

