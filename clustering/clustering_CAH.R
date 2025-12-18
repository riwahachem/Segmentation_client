library(FactoMineR)
library(factoextra)
library(dplyr)
library(class)
library(tidyr)

run_hcpc <- function(df){
  res.famd <- FAMD(df, ncp = 39, graph = FALSE)
  coords <- as.data.frame(res.famd$ind$coord)
  n <- nrow(df)
  
  if(n < 20000){
    res.hcpc <- HCPC(res.famd, nb.clust = -1, graph = FALSE)
    df$cluster <- factor(res.hcpc$data.clust$clust)
    viz_data <- coords
    viz_clust <- df$cluster
    
  } else {
    set.seed(123)
    idx_train <- sample(1:n, 20000)
    coords_train <- coords[idx_train, ]
    coords_test  <- coords[-idx_train, ]
    
    res.hcpc <- HCPC(coords_train, nb.clust = -1, graph = FALSE)
    cl_train <- factor(res.hcpc$data.clust$clust)
    
    cl_test <- knn(train = coords_train, test = coords_test, cl = cl_train, k = 1)
    
    cl_final <- factor(rep(NA, n), levels = levels(cl_train))
    cl_final[idx_train] <- cl_train
    cl_final[-idx_train] <- cl_test
    df$cluster <- cl_final
    
    viz_data <- coords_train
    viz_clust <- cl_train
  }
  
  print(table(df$cluster))
  
  desc <- catdes(df, num.var = which(names(df) == "cluster"))
  
  if(nrow(viz_data) > 5000){
    idx <- sample(1:nrow(viz_data), 5000)
    p <- fviz_cluster(list(data = viz_data[idx,1:2], cluster = viz_clust[idx]), geom = "point")
  } else {
    p <- fviz_cluster(list(data = viz_data[,1:2], cluster = viz_clust), geom = "point")
  }
  print(p)
  
  return(list(df=df, desc=desc))
}

load("data/data_loc_avec.RData")
load("data/data_loc_sans.RData")
load("data/data_prop_avec.RData")
load("data/data_prop_sans.RData")

res_loc_avec  <- run_hcpc(data_loc_avec)
res_loc_sans  <- run_hcpc(data_loc_sans)
res_prop_avec <- run_hcpc(data_prop_avec)
res_prop_sans <- run_hcpc(data_prop_sans)