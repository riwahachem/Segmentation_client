library(FactoMineR)
library(factoextra)
library(dplyr)
load("Data_cleaning/data_finale.RData")
df_sample <- data_impute %>% sample_n(20000)
run_clustering_apres_imputation<- function(df) {
  res.famd <- FAMD(df, graph = FALSE, ncp = 5)
  eigen <- res.famd$eig
  print(eigen)
  res.hcpc <- HCPC(res.famd, nb.clust = -1, graph = FALSE)
  # Plan 1-2
  p12 <- fviz_cluster(res.hcpc, geom = "point", axes = c(1,2))
  print(p12)
  
  # Plan 1-3
  p13 <- fviz_cluster(res.hcpc, geom = "point", axes = c(1,3))
  print(p13)
  
  # Plan 2-3
  p23 <- fviz_cluster(res.hcpc, geom = "point", axes = c(2,3))
  print(p23)
  
  # Plan 3-4
  p34 <- fviz_cluster(res.hcpc, geom = "point", axes = c(3,4))
  print(p34)
  print(res.hcpc$desc.var$category)
  return(res.hcpc)
}

res <- run_clustering_apres_imputation(df_sample)
# Tester Bootstrap et Monte-Carlo et Théorie des grands nombres


