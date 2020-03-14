# Usa a base Car Evaluation # 

# carregar pacotes de Naive Bayes
library(bnlearn)
library(e1071)
library(caret)

### Modelagem ###
carNaiveBayes = naiveBayes(classe ~., treinoCar) # criar Modelo. O ~. é em função de, por exemplo, vai classificar os dados (demais colunas) em função da coluna classe.
carNaiveBayes # retorno das probabilidades do modelo

previsaoNBCar = predict(carNaiveBayes, testeCar) # criar predição
confusionMatrix(previsaoNBCar, testeCar$classe) # matriz de confusão - Performace
