# instalação
install.packages('adabag', dependencies = T) # transformação de matrizes
install.packages('ade4', dependencies = T) # transformação de matrizes
install.packages('ape', dependencies = T) # clustering
install.packages('arules', dependencies = T) # mineração de regras
install.packages('arulesCBA', dependencies = T) # mineração de regras
install.packages('arulesViz', dependencies = T) # mineração de regras
install.packages('bnlearn', dependencies = T) # estatística bayesiana
install.packages('caret', dependencies = T) # geral de machine learning
install.packages('class', dependencies = T) # geral de machine learning
install.packages('cluster', dependencies = T) # clustering
install.packages('e1071', dependencies = T) # geral de machine learning
install.packages('expert', dependencies = T) # sistemas especialistas
install.packages('fclust', dependencies = T) # clustering
install.packages('GA', dependencies = T) # algoritmos genéticos
install.packages("GenSA", dependencies=T) # algoritmos genéticos
install.packages('lfl', dependencies = T) # fuzzy
install.packages('mgcv', dependencies = T)
install.packages('Metrics', dependencies = T)
install.packages('neuralnet', dependencies = T) # redes neurais
install.packages('openNLP', dependencies = T) # processamento de linguagem natural
install.packages('popbio', dependencies = T)
install.packages('randomForest', dependencies = T) # random forest 
install.packages('rpart', dependencies = T)
install.packages('rpart.plot', dependencies = T)
install.packages('party', dependencies = T)
install.packages('partykit', dependencies = T)
install.packages('RWeka', dependencies = T) # processamento de linguagem natural
install.packages("sets", dependencies=T)
install.packages('tabuSearch', dependencies = T)
install.packages('tree', dependencies = T)

#### Car Evaluation ####
# Entrada Car Evaluation
carEvaluation <- read.table("car.data", sep=",") #Criando o obj e carrega de dados
colnames(carEvaluation) <- c('precoCompra', 'precoManutencao', 'portas', 'ocupantes', 'mala', 'seguranca', 'classe')
str(carEvaluation) #Estrutura os dados
#carEvaluation$precoCompra <- as.numeric(precoCompra) #mudando o tipo da coluna
summary(carEvaluation) #Categoriza os dados
plot(carEvaluation$classe) #plotar gráfico
table(carEvaluation$classe) #plotar tabela
testeCar = carEvaluation[- particaoCar$Resample1, ]
table(testeCar$classe)

library(caret) #Caret é a biblioteca básica de IA - Comando de carregar a biblioteca

# Partição Car Evaluation 
particaoCar = createDataPartition(1:nrow(carEvaluation),p=.7) #Criação da partição, onde a função createDataPartition vai da primeira linha até a ultima linha (mede o total de linhas --> nrow(carEvaluation)) partindo 70% (p=.7) da base.
treinoCar = carEvaluation[particaoCar$Resample1,] #Incluindo a primeira amostra referente aos 70% da base no objeto referente ao treinamento da IA. Resample1 garante a aleatoridade dos dados.
testeCar = carEvaluation[- particaoCar$Resample1,] #Incluindo os demais casos da amostra referente aos outros 30% da base no objeto referente ao teste da IA. O - serve para pegar exatamente os dados não usados no treinamento.

# Saída Car Evaluation
write.csv(carEvaluation, file = "carEvaluation.csv")

#### Student Performance ####
# Entrada Student Performance
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

studentsPerformance=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

particaoStudents = createDataPartition(1:nrow(studentsPerformance),p=.7)
treinoStudents = studentsPerformance[particaoStudents$Resample1,]
testeStudents = studentsPerformance[- particaoStudents$Resample1,]

# Saída Student Performance
write.csv(studentsPerformance, file = "studentsPerformance.csv")

#### Bank Evaluation ####
# Entrada Bank Evaluation
bankEvaluation <- read.csv2('bank-additional-full.csv', sep = ";")
bankEvaluation$euribor3m <- as.numeric(bankEvaluation$euribor3m)

library(caret)
# Partição Bank Evaluation 
particaoBank = createDataPartition(1:nrow(bankEvaluation),p=.7)
treinoBank = bankEvaluation[particaoBank$Resample1,]
testeBank = bankEvaluation[- particaoBank$Resample1,]

#### Facebook ####
# Entrada Facebook # 
facebook <- read.table("dataset_Facebook.csv", sep=";", header = T)
str(facebook)
for(i in 3:6) {
  facebook[,i] <- as.factor(facebook[,i]) } 
str(facebook)

inteirosFacebook <- unlist(lapply(facebook, is.integer))  
facebookInteiros <- facebook[, inteirosFacebook]
str(facebookInteiros)

factorsFacebook <- unlist(lapply(facebook, is.factor))  
facebookFactor <- facebook[, factorsFacebook]
str(facebookFactor)

library(caret)
# Partição Bank Evaluation 
particaoFacebook = createDataPartition(1:nrow(facebook),p=.7)
treinoFacebook = facebook[particaoFacebook$Resample1,]
testeFacebook = facebook[- particaoFacebook$Resample1,]

library(caret)
# Partição Bank Evaluation 
particaoFacebookFactors = createDataPartition(1:nrow(facebookFactor),p=.7)
treinoFacebookFactor = facebookFactor[particaoFacebookFactors$Resample1,]
testeFacebookFactor = facebookFactor[- particaoFacebookFactors$Resample1,]