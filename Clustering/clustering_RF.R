load("Data/data_loc_avec.RData")
load("Data/data_loc_sans.RData")
load("Data/data_prop_avec.RData")
load("Data/data_prop_sans.RData")

library(randomForest)
library(cluster)

set.seed(123)

# Locataire avec coemprunteur 
rf <- randomForest(data_loc_avec, 
                   ntree = 500,
                   proximity = TRUE,
                   unsupervised = TRUE)

load("Data/rf_data_prop_avec")

library(umap)
umap_res <- umap(1 - rf$proximity)

coords <- umap_res$layout

km <- kmeans(coords, centers = 8)

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
km <- kmeans(coords, centers = 8, nstart = 20)
cluster = km$cluster
# Silhouette
d = dist(coords)
objet = silhouette(cluster, d)
summary(objet, FUN = mean)
################################################
# 3. Clustering final K-means avec K_choisi
set.seed(123)
km_final <- kmeans(coords, centers = 8, nstart = 25)
data_loc_avec$cluster <- factor(km_final$cluster)

save(data_loc_avec, file = "Data/cluster_loc_avec.RData")
