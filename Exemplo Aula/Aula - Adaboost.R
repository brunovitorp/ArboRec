# Usa a base Car Evaluation # 

# carregar pacotes de AdaBoost
library(adabag)
library(caret)

### Modelagem ###
adabagCar <- boosting(classe ~ ., data=treinoCar, boos=TRUE, mfinal=120, coeflearn='Breiman')
plot(errorevol(adabagCar,treinoCar))

previsaoAbCar = predict(adabagCar, testeCar) # criar predição
confusionMatrix(as.factor(previsaoAbCar$class), testeCar$classe) # matriz de confusão
