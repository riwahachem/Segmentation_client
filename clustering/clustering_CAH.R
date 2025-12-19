library(FactoMineR)
library(cluster)
library(class)

# Chargement des données
load("data/data_loc_avec.RData")
load("data/data_loc_sans.RData")
load("data/data_prop_avec.RData")
load("data/data_prop_sans.RData")

# Liste des bases
datas <- list(
  data_loc_avec  = data_loc_avec,
  data_loc_sans  = data_loc_sans,
  data_prop_avec = data_prop_avec,
  data_prop_sans = data_prop_sans
)


run_hcpc <- function(df, ncp = 20, max_hcpc = 20000, seed = 123){
  
  set.seed(seed)
  
  # FAMD
  res_famd <- FAMD(df, ncp = ncp, graph = FALSE)
  coords <- as.data.frame(res_famd$ind$coord)
  n <- nrow(df)
  
  # HCPC
  if(n <= max_hcpc){
    
    res_hcpc <- HCPC(res_famd, nb.clust = -1, graph = FALSE)
    data <- df
    data$cluster <- factor(res_hcpc$data.clust$clust)
    
  } else {
    
    idx_train <- sample(seq_len(n), max_hcpc)
    coords_train <- coords[idx_train, ]
    coords_test  <- coords[-idx_train, ]
    
    res_hcpc <- HCPC(coords_train, nb.clust = -1, graph = FALSE)
    cl_train <- factor(res_hcpc$data.clust$clust)
    
    cl_test <- knn(
      train = coords_train,
      test  = coords_test,
      cl    = cl_train,
      k     = 1
    )
    
    cl_final <- factor(rep(NA, n), levels = levels(cl_train))
    cl_final[idx_train]  <- cl_train
    cl_final[-idx_train] <- cl_test
    
    data <- df
    data$cluster <- cl_final
  }
  cat("Répartition des clusters :\n")
  print(table(data$cluster))
  return(list(
    data = data,
    famd = res_famd,
    hcpc = res_hcpc
  ))
}

for(name in names(datas)){
  
  cat("\nHCPC :", name, "\n")
  
  res <- run_hcpc(datas[[name]])
  
  data <- res$data
  
  save(
    data,
    file = paste0("data/cluster_hcpc_", name, ".RData")
  )
}


