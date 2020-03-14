# Usa a base Car Evaluation # 

# carregar pacotes de Redes Bayesianas
library(bnlearn)
library(caret)

carRedeBayes <- hc(treinoCar) # estruturar a rede bayseana
plot(carRedeBayes)
#Criando o Modelo
carModRedeBayes <- bn.fit(carRedeBayes, data = treinoCar)
bn.fit.barchart(carModRedeBayes$classe)
bn.fit.dotplot(carModRedeBayes$classe)
coefficients(carModRedeBayes)

cpquery(carModRedeBayes, 
        event =(classe=="unacc"), 
        evidence=(precoCompra=="vhigh" & precoManutencao=="vhigh" & seguranca == "low"))

str(carModRedeBayes)

###################### Students ################
