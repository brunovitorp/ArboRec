library(neuralnet)
library(ade4)
library(Metrics)
##Processo de ETL - Dijunção ou Binarização - Transforma uma variável categórica em uma variável numérica.
treinoCarDummyFull <- acm.disjonctif(treinoCar)
testeCarDummyFull <- acm.disjonctif(testeCar)
##Contagem de tempo iniciada
start_time <- Sys.time()
##Criando o modelo da Rede Neural
nnCar = neuralnet(classe.acc + classe.good + classe.unacc + classe.vgood ~ 
precoCompra.high + precoCompra.low + precoCompra.med +
precoCompra.vhigh + precoManutencao.high + precoManutencao.low +
precoManutencao.med + precoManutencao.vhigh + portas.2 + portas.3 +
portas.4 + portas.5more + ocupantes.2 + ocupantes.4 + ocupantes.more +  mala.big + mala.med + mala.small + seguranca.high + seguranca.low +  seguranca.med, treinoCarDummyFull, hidden=c(20,15,10,8))
##Finalizando o tempo
end_time <- Sys.time()
##Iprimindo o tempo
print(total_time <- end_time - start_time)

plot(nnCar) 

net.result <- compute(nnCar, testeCarDummyFull[,1:21])
predicoesRN <- as.data.frame(round(net.result[["net.result"]])) 
colnames(predicoesRN) <- c('acc', 'good', 'unacc', 'vgood') 

Metrics::rmse(testeCarDummyFull[,22], predicoesRN[,1])
rmse(testeCarDummyFull[,23], predicoesRN[,2])
rmse(testeCarDummyFull[,24], predicoesRN[,3])
rmse(testeCarDummyFull[,25], predicoesRN[,4])
