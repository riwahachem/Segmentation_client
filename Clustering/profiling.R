# Profils
load("Data/cluster_loc_avec.RData")
data = data_loc_avec
data$cluster = factor(data$cluster)

# --- 1. identifier variables qualitatives et numériques (en excluant 'cluster')
qual_vars <- names(data)[sapply(data, is.factor) & names(data) != "cluster"]
num_vars  <- names(data)[sapply(data, is.numeric) & names(data) != "cluster"]

# --- 2. Table des pourcentages des modalités de toutes les variables qualitatives
results_qual <- data %>%
  select(cluster, all_of(qual_vars)) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "modality") %>%
  group_by(cluster, variable, modality) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, variable) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  arrange(cluster, variable, desc(pct))

# --- 3. Proportions globales par modalité (pour comparaison)
global_props <- data %>%
  select(all_of(qual_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "modality") %>%
  group_by(variable, modality) %>%
  summarise(n_global = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(pct_global = 100 * n_global / sum(n_global)) %>%
  ungroup()

# --- 4. Calculer la différence (pct_local - pct_global)
cluster_descriptors <- results_qual %>%
  left_join(global_props %>% select(variable, modality, pct_global),
            by = c("variable", "modality")) %>%
  mutate(pct_global = ifelse(is.na(pct_global), 0, pct_global),
         diff = pct - pct_global) %>%
  arrange(cluster, desc(diff))


# QUALITATIF
# --- 5. Extraire les top modalités par cluster (ici top 10, modifie si tu veux top 3)
best_modalities <- cluster_descriptors %>%
  group_by(cluster) %>%
  slice_max(order_by = diff, n = 336, with_ties = FALSE) %>%
  arrange(cluster, desc(diff)) %>%
  ungroup()
best_modalities = subset(best_modalities, select=-c(n,pct_global))


# NUMÉRIQUES
# --- 6. Médianes numériques par cluster (dplyr >= 1.1.0 syntaxe)
results_num_long <- data %>%
  group_by(cluster) %>%
  summarise(across(all_of(num_vars), \(x) median(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "median_cluster")

# --- 7. Médianes globales et écarts types
global_median_num <- data %>%
  select(all_of(num_vars)) %>%
  summarise(across(everything(), \(x) median(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "median_global")

global_sd <- data %>%
  select(all_of(num_vars)) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "sd_global")

num_descriptors_long <- results_num_long %>%
  left_join(global_median_num, by = "variable") %>%
  left_join(global_sd, by = "variable") %>%
  mutate(diff_norm = median_cluster - median_global,
         abs_diff_norm = abs(diff_norm / sd_global)) %>%
  arrange(cluster, desc(abs_diff_norm))

# --- 8. Top variables numériques par cluster (top 10 par défaut)
best_num <- num_descriptors_long %>%
  group_by(cluster) %>%
  slice_max(order_by = abs_diff_norm, n = 336, with_ties = FALSE) %>%
  ungroup()

best_num = subset(best_num, select=-c(median_global,sd_global))

# QUALITATIF
qual_scores <- cluster_descriptors %>%
  group_by(cluster, variable) %>%
  summarise(score_qual = max(abs(diff)), .groups = "drop")

qual_scores <- qual_scores %>%
  rename(score = score_qual)

# QUANTITATIF
num_scores <- num_descriptors_long %>%
  select(cluster, variable, diff_norm) %>%
  mutate(score = abs(diff_norm)) %>%
  select(cluster, variable, score)

# TABLE FINALE
all_scores <- bind_rows(num_scores, qual_scores) %>%
  arrange(cluster, desc(score))

all_scores <- all_scores %>%
  mutate(type = ifelse(variable %in% qual_scores$variable, "Qualitative", "Numerical"))

# initialiser la colonne
all_scores$modalite <- NA

# remplir pour les variables qualitatives
for (i in 1:nrow(all_scores)) {
  # ne traiter que les qualitatives
  if (all_scores$type[i] == "Qualitative") {
    # chercher la ligne correspondante dans best_modalities
    sel <- best_modalities$cluster == all_scores$cluster[i] &
      best_modalities$variable == all_scores$variable[i]
    # assigner la modalité
    if (any(sel)) {
      all_scores$modalite[i] <- as.character(best_modalities$modality[sel])
    }
  }
}

for (i in 1:nrow(all_scores)) {
  # ne traiter que les qualitatives
  if (all_scores$type[i] == "Numerical") {
    # chercher la ligne correspondante dans best_modalities
    sel <- best_num$cluster == all_scores$cluster[i] &
      best_num$variable == all_scores$variable[i]
    # assigner la modalité
    if (any(sel)) {
      all_scores$modalite[i] <- best_num$median_cluster[sel]
    }
  }
}

