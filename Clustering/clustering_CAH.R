library(FactoMineR)
library(factoextra)
library(dplyr)
library(class)
library(tidyr)

run_hcpc_adaptatif <- function(df){
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



analyse_clusters <- function(df){
  
  med_num <- df %>% 
    group_by(cluster) %>% 
    summarise(across(where(is.numeric), median, na.rm = TRUE))
  
  med_global <- df %>% 
    summarise(across(where(is.numeric), median, na.rm = TRUE))
  
  med_compare <- med_num %>%
    mutate(cluster = as.character(cluster)) %>%
    pivot_longer(-cluster, names_to = "variable", values_to = "median_cluster") %>%
    left_join(
      med_global %>% 
        pivot_longer(everything(), names_to = "variable", values_to = "median_global"),
      by = "variable"
    ) %>%
    mutate(diff = median_cluster - median_global)
  
  profil_modalites <- function(df){
    quali <- df %>% select(where(is.factor))
    res <- list()
    for(v in names(quali)){
      tab_clust <- prop.table(table(df$cluster, df[[v]]), 1) * 100
      tab_global <- prop.table(table(df[[v]])) * 100
      ratio <- sweep(tab_clust, 2, tab_global, "-")
      res[[v]] <- ratio
    }
    return(res)
  }
  
  modalites <- profil_modalites(df)
  
  variable_importance <- function(df){
    num <- df %>% select(where(is.numeric))
    fac <- df %>% select(where(is.factor))
    p_num <- sapply(num, function(v) kruskal.test(v ~ df$cluster)$p.value)
    p_fac <- sapply(fac, function(v) chisq.test(table(v, df$cluster))$p.value)
    sort(c(p_num, p_fac))
  }
  
  important_vars <- variable_importance(df)
  
  resume_cluster <- function(df, cl){
    dfc <- df[df$cluster == cl, ]
    
    cat("Taille :", nrow(dfc), " (", round(100*nrow(dfc)/nrow(df),2), "%)\n\n")
    
    cat("---- Médianes numériques ----\n")
    print(dfc %>% summarise(across(where(is.numeric), median, na.rm=TRUE)))
    
    cat("\n---- Top modalités ----\n")
    quali <- df %>% select(where(is.factor))
    for(v in names(quali)){
      tab <- prop.table(table(dfc[[v]])) * 100
      top <- sort(tab, decreasing = TRUE)[1:min(3, length(tab))]
      cat("\n", v, ":\n")
      print(round(top, 2))
    }
  }
  
  for(cl in levels(df$cluster)){
    resume_cluster(df, cl)
  }
  
  return(list(
    med_num = med_num,
    med_global = med_global,
    med_compare = med_compare,
    modalites = modalites,
    important_vars = important_vars
  ))
}



load("Data/data_loc_avec.RData")
load("Data/data_loc_sans.RData")
load("Data/data_prop_avec.RData")
load("Data/data_prop_sans.RData")

res_loc_avec  <- run_hcpc_adaptatif(data_loc_avec)
res_loc_sans  <- run_hcpc_adaptatif(data_loc_sans)
res_prop_avec <- run_hcpc_adaptatif(data_prop_avec)
res_prop_sans <- run_hcpc_adaptatif(data_prop_sans)

a <- analyse_clusters(res_loc_avec$df)
b <- analyse_clusters(res_loc_sans$df)
c <- analyse_clusters(res_prop_avec$df)
d <- analyse_clusters(res_prop_sans$df)
