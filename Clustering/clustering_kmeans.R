library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)

set.seed(123)

load("Data/data_loc_avec.RData")

df = subset(data_loc_avec, select = -agence)
res.famd <- FAMD(data_loc_avec, graph = FALSE, 49)
eigen = res.famd$eig
# chute + max = 8
# gap statistic / coude = 4

km <- kmeans(coords, centers = 8, nstart = 20)
coords <- res.famd$ind$coord[,1:25]
cluster = km$cluster
# Silhouette
d = dist(coords)
objet = silhouette(cluster, d)

summary(objet, FUN = mean)

fviz_nbclust(coords, kmeans, method = "gap statistic")
fviz_nbclust(coords, kmeans, method = "wss")

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


# 3. Clustering final K-means avec K_choisi
km_final <- kmeans(coords, centers = 8, nstart = 25)
data_loc_avec$cluster <- factor(km_final$cluster)

# 4. Description des clusters
data   <- data_loc_avec

qual_vars <- names(data)[sapply(data, is.factor)]

results_qual <- lapply(qual_vars, function(v) {
  tab <- table(data$cluster, data[[v]])
  prop <- prop.table(tab, margin = 1) * 100  # % ligne = cluster
  round(prop, 2)
})

names(results_qual) <- qual_vars

num_vars <- names(data_loc_avec)[sapply(data_loc_avec, is.numeric)]

results_num <- data_loc_avec %>%
  group_by(cluster) %>%
  summarise(across(all_of(num_vars), median, na.rm = TRUE))



global_props <- lapply(qual_vars, function(v) {
  prop.table(table(data[[v]])) * 100
})
names(global_props) <- qual_vars

cluster_descriptors <- list()

for (v in qual_vars) {
  local <- results_qual[[v]]         # pourcentages par cluster
  global <- global_props[[v]]        # pourcentages globaux
  
  # différence cluster - global
  diff <- sweep(local, 2, global, "-")
  
  cluster_descriptors[[v]] <- diff
}

best_modalities <- list()

for (k in levels(data$cluster)) {
  all_scores <- do.call(rbind, lapply(cluster_descriptors, function(x) x[k, ]))
  all_scores <- sort(all_scores, decreasing = TRUE)
  best_modalities[[k]] <- head(all_scores, 10)
}

global_median_num <- apply(data[num_vars], 2, median, na.rm = TRUE)

num_descriptors <- results_num
for (v in num_vars) {
  num_descriptors[[v]] <- results_num[[v]] - global_median_num[v]
}
best_num <- list()

for (k in num_descriptors$cluster) {
  diffs <- num_descriptors[num_descriptors$cluster==k, num_vars]
  diffs <- sort(abs(unlist(diffs)), decreasing = TRUE)
  best_num[[k]] <- head(diffs, 3)
}

