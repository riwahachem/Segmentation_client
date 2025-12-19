library(FactoMineR)
library(cluster)

set.seed(123)

load("data/data_loc_avec.RData")

data <- data_loc_avec
  
# FAMD sur toute la base
res_famd <- FAMD(data, ncp = 20, graph = FALSE)
coords <- res_famd$ind$coord
  
# Sous échantillon pour la silhouette
sub_size <- min(10000, nrow(coords))
sub_idx <- sample(seq_len(nrow(coords)), sub_size)
coords_sub <- coords[sub_idx, ]
dist_sub <- dist(coords_sub)
  
# Silhouette pour différents k
k_max <- 20
sil <- numeric(k_max)
  
for(k in 2:k_max){
    clara_k <- clara(coords, k = k, samples = 50)
    s <- silhouette(clara_k$cluster[sub_idx], dist_sub)
    sil[k] <- mean(s[, 3])
    cat("k =", k, "| silhouette =", round(sil[k], 4), "\n")
}
  
# Choix du k optimal
k_opt <- which.max(sil)
  
  
# CLARA final sur toute la base
clara_final <- clara(coords, k = k_opt, samples = 200)
data$cluster <- factor(clara_final$cluster)
  
print(table(data$cluster))
  