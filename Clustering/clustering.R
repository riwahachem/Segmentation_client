library(FactoMineR)
library(factoextra)
library(dplyr)
load("data_finale(1).RData")
df_sample <- data_impute %>% sample_n(5000)
run_clustering_apres_imputation<- function(df) {
  #df_active <- df %>% mutate_if(is.character, as.factor)
  res.famd <- FAMD(df, graph = FALSE, ncp = )
  eigen <- res.famd$eig
  print(eigen)
  res.hcpc <- HCPC(res.famd, nb.clust = -1, graph = FALSE)
  #print(fviz_cluster(res.hcpc, geom = "point"))
  p <- fviz_cluster(res.hcpc, geom = "point")
  print(p)
  plot(p)
  print(res.hcpc$desc.var$category)
  return(res.hcpc)
}

res <- run_clustering_apres_imputation(df_sample)
