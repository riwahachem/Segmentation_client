library(FactoMineR)
library(factoextra)
library(cluster)
load("Data_cleaning/data_finale.RData")

res_famd <- FAMD(data_impute, ncp = 20, graph = FALSE) 
famd_coords <- res_famd$ind$coord
set.seed(123)
sub_idx <- sample(1:nrow(famd_coords), 2000)
famd_sub <- famd_coords[sub_idx, ]
dist_sub <- dist(famd_sub)
sil <- numeric(20)
cat("Recherche du k optimal\n")

for(k in 2:20){
  clara_k <- clara(famd_coords, k = k, samples = 100) 
  s <- silhouette(clara_k$cluster[sub_idx], dist_sub)
  sil[k] <- mean(s[,3])
  cat("k =", k, " : Silhouette =", round(sil[k], 4), "\n")
}
k_opt <- which.max(sil)
cat("k optimal =", k_opt, "\n")
clara_final <- clara(famd_coords, k = k_opt, samples = 200)
data_impute$cluster_clara <- as.factor(clara_final$cluster)
print(table(data_impute$cluster_clara))
sample_size <- 5000 
sample_id <- sample(seq_len(nrow(famd_coords)), sample_size)

p <- fviz_cluster(
  list(data = famd_coords[sample_id, 1:2],  
       cluster = clara_final$cluster[sample_id]),
  geom = "point"
)

print(p)

# A creuser