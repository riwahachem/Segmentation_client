run_clustering <- function(df, ncp = 39, sub_size = 10000, k_max = 20){
  library(FactoMineR)
  library(cluster)
  library(factoextra)
  res_famd <- FAMD(df, ncp = ncp, graph = FALSE)
  coords <- res_famd$ind$coord
  sub_idx <- sample(1:nrow(df), sub_size)
  coords_sub <- coords[sub_idx, ]
  dist_sub <- dist(coords_sub)
  
  sil <- numeric(k_max)
  
  for(k in 2:k_max){
    clara_k <- clara(coords, k = k, samples = 50)
    s <- silhouette(clara_k$cluster[sub_idx], dist_sub)
    sil[k] <- mean(s[,3])
    cat("k =", k, " | silhouette =", round(sil[k], 4), "\n")
  }
  
  k_opt <- which.max(sil)
  cat("\n>>> k optimal =", k_opt, "\n\n")
  clara_final <- clara(coords, k = k_opt, samples = 200)
  
  df$cluster <- as.factor(clara_final$cluster)
  
  cat("Répartition des clusters :\n")
  print(table(df$cluster))
  
  sample_size <- min(5000, nrow(coords))
  sample_id <- sample(seq_len(nrow(coords)), sample_size)
  
  p <- fviz_cluster(
    list(
      data = coords[sample_id, 1:2],
      cluster = clara_final$cluster[sample_id]
    ),
    geom = "point"
  )
  print(p)
  
  return(list(
    df = df,
    famd = res_famd,
    coords = coords,
    clara = clara_final,
    k_opt = k_opt,
    plot = p,
    silhouette = sil
  ))
}

analyze_clusters <- function(df){
  library(dplyr)
  library(tidyr)
  
  results <- list()
  
  # Médianes numériques par cluster
  med_num <- df %>% 
    group_by(cluster) %>% 
    summarise(across(where(is.numeric), median, na.rm = TRUE))
  
  # Médiane globale
  med_global <- df %>%
    summarise(across(where(is.numeric), median, na.rm = TRUE))
  
  # Tableau comparatif
  compare_median <- med_num %>%
    mutate(cluster = as.character(cluster)) %>%
    pivot_longer(-cluster, names_to = "variable", values_to = "median_cluster") %>%
    left_join(
      med_global %>%
        pivot_longer(everything(), names_to = "variable", values_to = "median_global"),
      by = "variable"
    ) %>%
    mutate(diff = median_cluster - median_global)
  
  results$median_num <- med_num
  results$median_global <- med_global
  results$median_compare <- compare_median
  
  
  # Modalités sur-représentées
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
  
  results$modalites <- profil_modalites(df)
  
  
  # Variables discriminantes
  variable_importance <- function(df){
    num <- df %>% select(where(is.numeric))
    fac <- df %>% select(where(is.factor))
    
    res_num <- sapply(num, function(v){
      kruskal.test(v ~ df$cluster)$p.value
    })
    
    res_fac <- sapply(fac, function(v){
      chisq.test(table(v, df$cluster))$p.value
    })
    
    sort(c(res_num, res_fac))
  }
  
  results$important_vars <- variable_importance(df)
  
  
  # Résumé par cluster
  resume_cluster <- function(df, cl){
    dfc <- df[df$cluster == cl, ]
    
    cat("Taille :", nrow(dfc), " (", 
        round(100 * nrow(dfc) / nrow(df), 2), "% de la base)\n\n")
    
    # Médianes
    print(dfc %>% summarise(across(where(is.numeric), median, na.rm=TRUE)))
    
    # Modalités dominantes
    quali <- df %>% select(where(is.factor))
    for(v in names(quali)){
      tab <- prop.table(table(dfc[[v]])) * 100
      top <- sort(tab, decreasing = TRUE)[1:min(3, length(tab))]
      cat("\n", v, ":\n")
      print(round(top, 2))
    }
  }
  
  results$resume <- function(){
    for(cl in levels(df$cluster)){
      resume_cluster(df, cl)
    }
  }
  
  return(results)
}


full_clustering_pipeline <- function(df){
  res_cluster <- run_clustering(df)
  res_analysis <- analyze_clusters(res_cluster$df)
  
  return(list(
    clustering = res_cluster,
    analysis = res_analysis
  ))
}

load("data/data_loc_sans.RData")
result_loc_sans <- full_clustering_pipeline(data_loc_sans)
# Afficher la médiane numérique par cluster
print(result_loc_sans$analysis$median_num)
# Afficher la médiane globale
print(result_loc_sans$analysis$median_global)
# Afficher la comparaison des médianes (différence cluster vs global)
print(result_loc_sans$analysis$median_compare)
# Afficher les modalités sur-représentées
print(result_loc_sans$analysis$modalites)
# Afficher les variables discriminantes (p-values)
print(result_loc_sans$analysis$important_vars)
# Afficher un résumé complet et détaillé par cluster (avec médianes + top modalités)
result_loc_sans$analysis$resume()


load("data/data_loc_avec.RData")
result_loc_avec <- full_clustering_pipeline(data_loc_avec)
print(result_loc_avec$analysis$median_num)
print(result_loc_avec$analysis$median_global)
print(result_loc_avec$analysis$median_compare)
print(result_loc_avec$analysis$modalites)
print(result_loc_avec$analysis$important_vars)
result_loc_avec$analysis$resume()


load("data/data_prop_sans.RData")
result_prop_sans <- full_clustering_pipeline(data_prop_sans)
print(result_prop_sans$analysis$median_num)
print(result_prop_sans$analysis$median_global)
print(result_prop_sans$analysis$median_compare)
print(result_prop_sans$analysis$modalites)
print(result_prop_sans$analysis$important_vars)
result_prop_sans$analysis$resume()

load("data/data_prop_avec.RData")
result_prop_avec <- full_clustering_pipeline(data_prop_avec)
print(result_prop_avec$analysis$median_num)
print(result_prop_avec$analysis$median_global)
print(result_prop_avec$analysis$median_compare)
print(result_prop_avec$analysis$modalites)
print(result_prop_avec$analysis$important_vars)
result_prop_avec$analysis$resume()