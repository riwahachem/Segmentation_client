library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)
library(tidyr)

set.seed(123)

load("Data/data_loc_avec.RData")
# FAMD
res.famd <- FAMD(data_loc_avec, graph = FALSE, ncp= 39)
eigen = res.famd$eig
# chute + max = 8
# gap statistic / coude = 4
coords <- res.famd$ind$coord
################################################
# Silhouette pour plusieurs k
k_values <- 2:20

# Vecteur pour stocker les silhouettes moyennes
sil_means <- numeric(length(k_values))

# Boucle sur les k
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Clustering K-means (ou autre)
  cl <- kmeans(coords, centers = k, nstart = 20)
  
  # Silhouette
  sil <- silhouette(cl$cluster, dist(coords))
  
  # Silhouette moyenne
  sil_means[i] <- mean(sil[, 3])
}

# Résultats
data.frame(k = k_values, silhouette = sil_means)
################################################
# Tester le k optimal avec la silhouette
km <- kmeans(coords, centers = 4, nstart = 20)
cluster = km$cluster
# Silhouette
d = dist(coords)
objet = silhouette(cluster, d)
summary(objet, FUN = mean)

fviz_nbclust(coords, kmeans, method = "gap_stat")
fviz_nbclust(coords, kmeans, method = "wss")
################################################
# 3. Clustering final K-means avec K_choisi
set.seed(123)
km_final <- kmeans(coords, centers = 8, nstart = 25)
data_loc_avec$cluster <- factor(km_final$cluster)

save(data_loc_avec, file = "Data/cluster_loc_avec.RData")
