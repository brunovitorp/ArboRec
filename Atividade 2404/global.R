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

###################### base 1 #########################
eleitores <- fread("eleitores_jovens.csv", showProgress = TRUE, encoding = 'UTF-8', stringsAsFactors = TRUE)

# deixar os nomes das colunas mais amigáveis para o R
colnames(eleitores) <- c('Turno', 'UF', 'Genero', 'Estado_Civil', 'Idade', 'Escolaridade', 'Situacao')
