# Usa a base Car Evaluation # 

# carregar pacotes de Naive Bayes
library(randomForest)
library(caret)

### Modelagem ###
carRdForest = randomForest(treinoCar[,-7], treinoCar$classe, ntree = 120, importance = TRUE) # criar Modelo
varImpPlot(carRdForest)
carRdForest
carRdForest$importance
plot(carRdForest)

previsaoRdCar  = predict(carRdForest, testeCar) # criar predição
confusionMatrix(previsaoRdCar, testeCar$classe) # matriz de confusão
