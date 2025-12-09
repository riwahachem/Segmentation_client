library(FactoMineR)
library(factoextra)
library(dplyr)

set.seed(123)

load("Data_cleaning/data_finale.RData")

df = subset(data_impute, select = -agence)
res.famd <- FAMD(df, graph = FALSE, 49)
eigen = res.famd$eig
coords <- res.famd$ind$coord[,1:25]

# Recherche du k optimal
K_max <- 10
wcss  <- numeric(K_max)
for (k in 2:K_max) {
  km_tmp <- kmeans(coords, centers = k, nstart = 25)
  wcss[k] <- km_tmp$tot.withinss
}
plot(2:K_max, wcss[2:K_max], type = "b", pch = 19, frame = FALSE,
     xlab = "Nombre de clusters K",
     ylab = "WCSS (inertie intra)",
     main = "Méthode du coude")
K_choisi <- 6

library(cluster)
Ks <- 2:K_choisi
sil_values <- numeric(length(Ks))

for (i in seq_along(Ks)) {
  K <- Ks[i]
  km <- kmeans(coords, centers = K, nstart = 20)
  
  idx <- sample(1:nrow(coords), 1000)
  d_sample <- dist(coords[idx, ])
  clusters_sample <- km$cluster[idx]
  
  sil_values[i] <- mean(silhouette(clusters_sample, d_sample)[, 3])
}
sil_values

# 3. Clustering final K-means avec K_choisi
km_final <- kmeans(coords, centers = 3, nstart = 25)
data_impute$cluster <- factor(km_final$cluster)


# 4. Visualisation 
p <- fviz_cluster(
  list(data = coords, cluster = km_final$cluster),
  geom = "point"
)
print(p)


# 5. Description des clusters
df_desc   <- data_impute
qual_vars <- names(df_desc)[sapply(df_desc, is.factor)]

for (v in qual_vars) {
  cat("\nVariable :", v, "\n")
  print(round(prop.table(table(df_desc[[v]], df_desc$cluster), margin = 2) * 100, 2))
}