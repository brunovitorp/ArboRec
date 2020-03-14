library(class)
library(caret)
library(ade4)

irisNova <- iris
##[linhas, colunas] c é uma função de concatenar
treinoIrisNum <- irisNova[c(1:140),] 
testeIrisNum <- irisNova[-c(1:140),] 

irisInst = knn(treinoIrisNum[,1:4], testeIrisNum[,1:4], treinoIrisNum[,5], k=20) 
irisInst 
confusionMatrix(irisInst, testeIrisNum$Species) 


## Exercicio com o ChickWeight
dietaPinto <- ChickWeight

treinoPinto <- dietaPinto[c(1:404),] 
testePinto <- dietaPinto[-c(1:404),] 

pintoInst = knn(treinoPinto[,c(1,2)], testePinto[,c(1,2)], treinoPinto[,4], k=20) 
pintoInst 
confusionMatrix(pintoInst, testePinto$Diet)
