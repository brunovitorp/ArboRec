library(cluster)

irisNumerica <- iris[, -5]
str(irisNumerica)

set.seed(1)
sample(1:10,5)
kIris = kmeans(irisNumerica[,1:4], centers = 3)

cIris = kIris$cluster
irisNumerica$cluster <- as.factor(kIris$cluster)
irisNumerica$Species <- iris$Species

clusplot(irisNumerica, cIris, xlab = 'Fator1', ylab = 'Fator2', main = 'Agrupamento Especies', lines = 0, shade = F, color = TRUE, labels = 2)
plot(irisNumerica$Species, irisNumerica$cluster)
