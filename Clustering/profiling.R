library(dplyr)
library(tidyr)
library(scales)

# Profils
load("data/cluster_data_loc_avec.RData")
data$cluster = factor(data$cluster)

# QUALITATIF
qual_vars <- names(data)[sapply(data, is.factor) & names(data) != "cluster"]

# Pourcentages des modalités du cluster
results_qual <- data %>%
  select(cluster, all_of(qual_vars)) %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "modality") %>%
  group_by(cluster, variable, modality) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(cluster, variable) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  arrange(cluster, variable, desc(pct))

# Pourcentages des modalités de la table totale
global_props <- data %>%
  select(all_of(qual_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "modality") %>%
  group_by(variable, modality) %>%
  summarise(n_global = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(pct_global = 100 * n_global / sum(n_global)) %>%
  ungroup()

# Différence entre les pourcentages 
cluster_descriptors <- results_qual %>%
  left_join(global_props %>% select(variable, modality, pct_global),
            by = c("variable", "modality")) %>%
  mutate(pct_global = ifelse(is.na(pct_global), 0, pct_global),
         diff = pct - pct_global) %>%
  arrange(cluster, desc(diff))

# Ranger les variables par cluster
best_modalities <- cluster_descriptors %>%
  group_by(cluster) %>%
  slice_max(order_by = diff, n = 336, with_ties = FALSE) %>%
  arrange(cluster, desc(diff)) %>%
  ungroup()
best_modalities = subset(best_modalities, select=-c(n,pct_global))

# QUANTITATIF
num_vars  <- names(data)[sapply(data, is.numeric) & names(data) != "cluster"]

# Médianes par cluster
results_num_long <- data %>%
  group_by(cluster) %>%
  summarise(across(all_of(num_vars), \(x) median(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-cluster, names_to = "variable", values_to = "median_cluster")

# Médianes globales totale
global_median_num <- data %>%
  select(all_of(num_vars)) %>%
  summarise(across(everything(), \(x) median(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "median_global")

# Standardisation
global_sd <- data %>%
  select(all_of(num_vars)) %>%
  summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "sd_global")

# Différence entre les médianes
num_descriptors_long <- results_num_long %>%
  left_join(global_median_num, by = "variable") %>%
  left_join(global_sd, by = "variable") %>%
  mutate(diff_norm = median_cluster - median_global,
         abs_diff_norm = abs(diff_norm / sd_global)) %>%
  arrange(cluster, desc(abs_diff_norm))

# Ranger les variables par cluster
best_num <- num_descriptors_long %>%
  group_by(cluster) %>%
  slice_max(order_by = abs_diff_norm, n = 336, with_ties = FALSE) %>%
  ungroup()

best_num = subset(best_num, select=-c(median_global,sd_global))

best_num <- best_num %>%
  mutate(median_cluster = format(median_cluster, scientific = FALSE, digits = 10),
         diff_norm = format(diff_norm, scientific = FALSE, digits = 10))

# COMPARAISON QUALITATIF - QUANTITATIF
# QUALITATIF
cluster_counts <- data %>%
  group_by(cluster) %>%
  summarise(n_cluster = n(), .groups = "drop")

cluster_h <- cluster_descriptors %>%
  left_join(cluster_counts, by = "cluster") %>%
  mutate(p_hat = pct / 100,
         p_global = pct_global / 100,
         # arcsine-sqrt transformation
         asin1 = 2 * asin(pmin(pmax(sqrt(p_hat), 0), 1)),
         asin0 = 2 * asin(pmin(pmax(sqrt(p_global), 0), 1)),
         h = asin1 - asin0)  # signed

qual_scores <- cluster_h %>%
  group_by(cluster, variable) %>%
  summarise(score_qual = max(abs(h), na.rm = TRUE), .groups = "drop")

qual_scores <- qual_scores %>%
  rename(score = score_qual)

# QUANTITATIF
num_scores <- num_descriptors_long %>%
  select(cluster, variable, abs_diff_norm) %>%
  mutate(score = abs_diff_norm) %>%
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

all_scores <- all_scores %>%
  mutate(score = format(score, scientific = FALSE, digits = 10))
