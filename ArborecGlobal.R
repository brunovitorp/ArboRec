##GERAIS
library(dplyr) # comandos pip
library(DT) # tabelas dinâmicas
library(flexdashboard) # dashboard
library(formattable) # formatações
library(gplots) # gráficos
library(lubridate) # datas
library(plotly) # gráficos
library(ggplot2) # gráficos
library(reshape2) # transformação de dados
library(data.table) # leitura de tabelas
###ML
library(bnlearn)
library(caret)
library(C50)
library(data.table)
library(e1071)
library(pROC)
library(randomForest)
library(rpart)
library(readr)
library(bnlearn)
require(rpart.plot)
require(party)
require(partykit)
require(tree)
library(adabag)

#Carga da base
ArboRec <- read_delim("ArboRec.csv", ";", escape_double = FALSE, trim_ws = TRUE)

## Transformando os dados

# Transformando o tp_classificacao_final em: Descartado, Inconclusivo, Dengue
ArboRec$tp_classificacao_final = ifelse(ArboRec$tp_classificacao_final == 5, 'Descartado', ifelse(ArboRec$tp_classificacao_final == 8, 'Inconclusivo', 'Dengue'))

# Transformando o tp_gestante nos dados descritivos
ArboRec$tp_gestante = ifelse(ArboRec$tp_gestante == 1, '1º Trimestre', ifelse(ArboRec$tp_gestante == 2, '2º Trimestre', ifelse(ArboRec$tp_gestante == 3, '3º Trimestre',ifelse(ArboRec$tp_gestante == 4, 'Idade gestacional ignorada',ifelse(ArboRec$tp_gestante == 6, 'Não se aplica - Homem', 'Não está Gravida')))))

# Transformando o tp_raca_cor em dados descritivos
ArboRec$tp_raca_cor = ifelse(ArboRec$tp_raca_cor == 1, 'Branca', ifelse(ArboRec$tp_raca_cor == 2, 'Preta', ifelse(ArboRec$tp_raca_cor == 3, 'Amarela', ifelse(ArboRec$tp_raca_cor == 4, 'Parda', ifelse(ArboRec$tp_raca_cor == 5, 'Indígena', ifelse(is.null(ArboRec$tp_raca_cor), 'Ignorado', 'Ignorado'))))))

# Transformando o co_distrito_residencia em dados descritivos
ArboRec$co_distrito_residencia = ifelse(ArboRec$co_distrito_residencia == 117, 'CENTRO EXPANDIDO', ifelse(ArboRec$co_distrito_residencia == 118, 'INCRUZILHADA-BEBERIBE', ifelse(ArboRec$co_distrito_residencia == 119, 'CASA AMARELA - DOIS IRMAOS',ifelse(ArboRec$co_distrito_residencia == 120, 'CAXANGA - VARZEA',ifelse(ArboRec$co_distrito_residencia == 121, 'AFOGADOS - TEJIPIO',ifelse(ArboRec$co_distrito_residencia == 122, 'IBURA - BOA VIAGEM',ifelse(ArboRec$co_distrito_residencia == 546, 'MACAXEIRA - VASCO DA GAMA','COHAB - JORDAO')))))))

# Transformando o tp_zona_residencia em dados descritivos
ArboRec$tp_zona_residencia = ifelse(ArboRec$tp_zona_residencia == 1, 'Urbano', ifelse(ArboRec$tp_zona_residencia == 2, 'Rural', ifelse(ArboRec$tp_zona_residencia == 3, 'Periurbano','Ignorado')))

# Transformando o tp_autoctone_residencia em dados descritivos
ArboRec$tp_autoctone_residencia = ifelse(ArboRec$tp_autoctone_residencia == 1, 'Sim', ifelse(ArboRec$tp_autoctone_residencia == 2, 'Não', 'Indeterminado'))

#Modificando os tipos de dados para Factor
ArboRec$tp_classificacao_final <- as.factor(ArboRec$tp_classificacao_final)
ArboRec$ds_semana_notificacao <- as.factor(ArboRec$ds_semana_notificacao)
ArboRec$notificacao_ano <- as.factor(ArboRec$notificacao_ano)
ArboRec$co_distrito_residencia <- as.factor(ArboRec$co_distrito_residencia)
ArboRec$tp_sexo <- as.factor(ArboRec$tp_sexo)
ArboRec$tp_gestante <- as.factor(ArboRec$tp_gestante)
ArboRec$tp_raca_cor <- as.factor(ArboRec$tp_raca_cor)
ArboRec$tp_escolaridade <- as.factor(ArboRec$tp_escolaridade)
ArboRec$no_bairro_residencia <- as.factor(ArboRec$no_bairro_residencia)
ArboRec$tp_zona_residencia <- as.factor(ArboRec$tp_zona_residencia)
ArboRec$tp_result_exame <- as.factor(ArboRec$tp_result_exame)
ArboRec$tp_autoctone_residencia <- as.factor(ArboRec$tp_autoctone_residencia)

#Recortando a base para apenas as colunas usadas para o modelo
ArboRecFinal <- ArboRec[, c(5,6, 14:17, 21, 23, 26, 57, 68, 74)]

#Renomeando o nome das colunas da base final tratada - ArboRecFinal
colnames(ArboRecFinal) <- c('Semana', 'Ano', 'Sexo', 'SeGestante', 'Raca', 'Escolaridade', 'DistritoSanitario', 'Bairro', 'ZonaResidencial', 'ResultadoExame', 'AutoctoneMunicipio', 'ClassificacaoFinal')


#Analisando os dados
table(ArboRecFinal$ClassificacaoFinal, exclude = NULL)
plot(ArboRecFinal$ClassificacaoFinal)

#Criação da base de treinamento e de teste
# particaoArbo = createDataPartition(1:nrow(ArboRecFinal), p=.7) 
# treinoArbo = ArboRecFinal[particaoArbo$Resample1,] 
# testeArbo = ArboRecFinal[- particaoArbo$Resample1,] 

### Análise de NaiveBayes ### 

# arboNaiveBayes = naiveBayes(tp_classificacao_final ~., treinoArbo)
# arboNaiveBayes

# previsaoArbo = predict(arboNaiveBayes, testeArbo) 
# confusionMatrix(previsaoArbo, testeArbo$tp_classificacao_final)

# save(arboNaiveBayes, file="arboNaiveBayesModel.Rdata")

### Criação de Arvore de Decisão ### 31 Bairros inviabiliza o uso de árvore

# arboatree <- ctree(tp_classificacao_final ~ ., treinoarbo)
# 
# plot(arboatree, type="simple", gp = gpar(fontsize = 5))
# 
# previsaoctreearbo = predict(arboatree, testearbo)
# previsaoctreearbo = as.data.frame(previsaoctreearbo)
# 
# confusionMatrix(previsaoCtreeArbo$previsaoCtreeArbo, testeArbo$tp_classificacao_final)
# 
# save(arboAtree, file="ctreeModel.Rdata")
# 
# # Árvores de Entropia
# arboRpart <- rpart(tp_classificacao_final ~ notificacao_ano + co_distrito_residencia + tp_raca_cor + tp_escolaridade + tp_zona_residencia, treinoArbo) # criar árvore
# rpart.plot(arboRpart)
# 
# # plotar entropia da árvore - nível de complexidade para arvore acima (evolução)
# plotcp(arboRpart) 
# 
# # podar árvore
# arboRpart <- prune(arboRpart, cp=0.041) 
# 
# # plotar árvore podada
# rpart.plot(arboRpart, extra = 104, branch.lty = 2,
#            nn= T, tweak = 1.2)
# 
# # realizar predição
# previsaoPartArbo = predict(arboRpart, testeArbo, type = "class")
# 
# # transformar predições em data frame
# previsaoPartArbo = as.data.frame(previsaoPartArbo)
# 
# # Matriz de confusão
# confusionMatrix(previsaoPartArbo$previsaoPartArbo, testeArbo$tp_classificacao_final)

# save(arboRpart, file="arboRpart.Rdata")