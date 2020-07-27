#Load appropriate Libraries
library(tidyverse)
library(sparklyr)
#spark.install(version = '3.0.0') #only run once
sc<- spark_connect(master = "local", version = '3.0.0')

#Load the dataset
library(rattle) 
wine <- rattle.data::wine
wine[-1] <- scale(wine[-1])

#Copy data to spark cluster
wine_tbl <- copy_to(sc, wine, "wine")

#PCA in Spark
pca <- tbl(sc, "wine") %>% select(-Type) %>% na.omit() %>%
  ml_pca()

#Plot Pca
D <- as.matrix(wine2[1:13])
E <- as.matrix(pca$pc)
P <- D %*% E
PCs <- as.data.frame(P)
PCs$Type <- wine$Type
ggplot(PCs, aes(PC1, PC2)) + geom_point(aes(color = Type))

#Kmeans Clustering + plot
kmeans <- wine_tbl %>% ml_kmeans(formula = ~ Nonflavanoids + Hue, k = 3)
ml_predict(kmeans) %>% collect() %>% 
  ggplot(aes(Nonflavanoids, Hue)) + geom_point(aes(Nonflavanoids, Hue,col = factor(prediction+1)), 
             size = 2, alpha = 0.5) + geom_point(data = kmeans$centers, aes(Nonflavanoids, Hue), pch = 'x', size = 12) + scale_color_discrete(name = "Predicted cluster")


