load("Data/data_loc_avec.RData")
load("Data/data_loc_sans.RData")
load("Data/data_prop_avec.RData")
load("Data/data_prop_sans.RData")

library(randomForest)
library(cluster)
library(umap)

set.seed(123)

# Locataire avec coemprunteur 
rf <- randomForest(data_loc_avec, 
                   ntree = 500,
                   proximity = TRUE,
                   unsupervised = TRUE)

# PAM
prox <- rf$proximity

#Matrice de dissimilarité
dissRF <- sqrt(1 - prox)
umap_res <- umap(1 - prox)
coords <- umap_res$layout

################################################
# Silhouette pour plusieurs k
k_values <- 2:10

# Vecteur pour stocker les silhouettes moyennes
sil_means <- numeric(length(k_values))

# Boucle sur les k
for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  # Clustering K-means (ou autre)
  cl <- pam(dissRF, k = k , diss = TRUE)
  
  # Silhouette
  sil <- silhouette(cl$clustering, dist(coords))
  
  # Silhouette moyenne
  sil_means[i] <- mean(sil[, 3])
}

# Résultats
data.frame(k = k_values, silhouette = sil_means)
################################################

library(cluster)
pam_RF <- pam(dissRF, k = 3, diss = TRUE)
clusters <- pam_RF$clustering

umap_res <- umap(1 - rf$proximity)

coords <- umap_res$layout
d = dist(coords)
objet = silhouette(clusters, d)
summary(objet, FUN = mean)
