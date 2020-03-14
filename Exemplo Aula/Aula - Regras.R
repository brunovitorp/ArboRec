library(arules)
library(arulesCBA)
library(arulesViz)
library(caret)

regrasCar = CBA(classe ~ ., treinoCar, supp=0.01, conf=0.01) 
inspect(regrasCar$rules)
plot(regrasCar$rules)

previsaoRegrasCar <- predict(regrasCar, testeCar)

confusionMatrix(previsaoRegrasCar, testeCar$classe)

