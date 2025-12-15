load("data/data_loc_avec.RData")
load("data/data_loc_sans.RData")
load("data/data_prop_avec.RData")
load("data/data_prop_sans.RData")

library(randomForest)
library(cluster)
library(umap)

datas <- list(
  data_loc_avec  = data_loc_avec
  #data_loc_sans  = data_loc_sans,
  #data_prop_avec = data_prop_avec,
  #data_prop_sans = data_prop_sans
)

for(name in names(datas)){
  data <- datas[[name]]
  set.seed(123)
  
  rf <- randomForest(data, 
                     ntree = 500,
                     proximity = TRUE,
                     unsupervised = TRUE)
  
  umap_res <- umap(1 - rf$proximity)
  coords <- umap_res$layout
  ################################################
  # Silhouette pour plusieurs k
  k_values <- 2:20

  # Vecteur pour stocker les silhouettes moyennes
  sil_means <- numeric(length(k_values))

  # Boucle sur les k
  for (i in seq_along(k_values)) {
    k <- k_values[i]
  
    # Clustering K-means (ou autre)
    set.seed(123)
    cl <- kmeans(coords, centers = k, nstart = 20)
  
    # Silhouette
    sil <- silhouette(cl$cluster, dist(coords))
  
    # Silhouette moyenne
    sil_means[i] <- mean(sil[, 3])
  }

  # Résultats
  k_silhouette = data.frame(k = k_values, silhouette = sil_means)

  ################################################
  # 3. Clustering final K-means avec K_choisi
  k = k_silhouette$k[which.max(k_silhouette$silhouette)]
  if(k > 4){
    k = k
  }else{
    k = k_silhouette$k[which.max(k_silhouette$silhouette[k_silhouette$k > 4])]
  }
  set.seed(123)
  km_final <- kmeans(coords, centers = k, nstart = 25)
  data$cluster <- factor(km_final$cluster)
  
  save(
    data,
    file = paste0("data/cluster_", name, ".RData")
  )
}