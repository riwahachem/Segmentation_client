library(dplyr)
library(tidyr)
library(ggplot2)

# VISU QUANTITATIVES
plot_quanti_intervals <- function(data, quanti_vars, cluster_id) {
  
  stats_quanti <- data %>%
    filter(cluster == cluster_id) %>% 
    select(all_of(quanti_vars)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>%
    summarise(
      Q1 = quantile(value, 0.25, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      Q3 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  ggplot(stats_quanti, aes(x = variable, y = median)) +
    geom_linerange(aes(ymin = Q1, ymax = Q3),
                   size = 2, color = "skyblue") +
    geom_point(size = 3, color = "blue") +
    labs(title = paste("Cluster", cluster_id,
                       ": Médiane & Intervalle [Q1 ; Q3]"),
         y = "Valeurs",
         x = "Variables") +
    theme_minimal(base_size = 13)
}

plot_quali_hist <- function(data, quali_vars, cluster_id) {
  
  df_quali <- data %>%
    filter(cluster == cluster_id) %>% 
    select(cluster, all_of(quali_vars)) %>%
    pivot_longer(-cluster, names_to = "variable", values_to = "modality") %>%
    mutate(modality = as.factor(modality))
  
  ggplot(df_quali, aes(x = modality, fill = modality)) +
    geom_bar() +
    facet_wrap(~ variable) +
    labs(title = paste("Cluster", cluster_id, ": distribution des modalités"),
         y = "Effectif",
         x = "Modalité") +
    theme_minimal(base_size = 13)
}

load("data/cluster_data_loc_avec.RData")
data$cluster <- factor(data$cluster)
num_vars  <- names(data)[sapply(data, is.numeric) & names(data) != "cluster"]
qual_vars <- names(data)[sapply(data, is.factor)  & names(data) != "cluster"]
# Cluster 1
plot_quanti_intervals(data, "taux_offre", 1)
plot_quali_hist(data, "hebergement_gratuit", 1)
plot_quali_hist(data, "situation_familliale_coemprunteur", 1)
plot_quali_hist(data, "situation_familliale_emprunteur", 1)

# Cluster 2
plot_quali_hist(data, "dette", 2)

# Cluster 3
plot_quali_hist(data, "nb_rdv_fait", 3)
plot_quali_hist(data, "etude_partagee", 3)
plot_quali_hist(data, "reserve_levee", 3)
plot_quali_hist(data, "premier_rdv_realise", 3)
plot_quali_hist(data, "status", 3)

# Cluster 4
plot_quali_hist(data, "hebergement_gratuit", 4)

# Cluster 5
plot_quali_hist(data, "nb_rdv_pas_fait", 5)
plot_quali_hist(data, "status", 5)
plot_quali_hist(data, "premier_rdv_realise", 5)

# Cluster 6
plot_quali_hist(data, "nb_rdv_fait", 6)
plot_quali_hist(data, "espace_client_est_valide", 6)
plot_quali_hist(data, "signature_electronique", 6)
plot_quali_hist(data, "etude_partagee", 6)
plot_quali_hist(data, "reserve_levee", 6)
plot_quali_hist(data, "premier_rdv_realise", 6)
plot_quali_hist(data, "retour_bfc", 6)
plot_quali_hist(data, "hebergement_gratuit", 6)
plot_quali_hist(data, "status", 6)

# Cluster 7
plot_quanti_intervals(data, "age_emprunteur", 7)
plot_quanti_intervals(data, "age_coemprunteur", 7)
plot_quanti_intervals(data, "taux_offre", 7)
plot_quali_hist(data, "type_contrat_coemprunteur", 7)

# Cluster 8
plot_quanti_intervals(data, "taux_offre", 8)
plot_quali_hist(data, "hebergement_gratuit", 8)
plot_quali_hist(data, "premier_rdv_realise", 8)
plot_quali_hist(data, "status", 8)
plot_quali_hist(data, "nb_pret_conso", 8)
plot_quali_hist(data, "etude_partagee", 8)
plot_quali_hist(data, "reserve_levee", 8)
plot_quali_hist(data, "signature_electronique", 8)
plot_quali_hist(data, "retour_bfc", 8)

# Cluster 9
plot_quali_hist(data, "hebergement_gratuit", 9)
plot_quali_hist(data, "situation_familliale_emprunteur", 9)
plot_quali_hist(data, "situation_familliale_coemprunteur", 9)
plot_quali_hist(data, "premier_rdv_realise", 9)
plot_quali_hist(data, "status", 9)
plot_quali_hist(data, "etude_partagee", 9)
plot_quali_hist(data, "reserve_levee", 9)
plot_quali_hist(data, "signature_electronique", 9)
plot_quali_hist(data, "retour_bfc", 9)

# Cluster 10
plot_quanti_intervals(data, "taux_offre", 10)
plot_quali_hist(data, "est_conteste", 10)
plot_quali_hist(data, "situation_familliale_emprunteur", 10)
plot_quali_hist(data, "situation_familliale_coemprunteur", 10)

# Cluster 11
plot_quanti_intervals(data, "taux_offre", 11)
plot_quali_hist(data, "situation_familliale_emprunteur", 11)
plot_quali_hist(data, "situation_familliale_coemprunteur", 11)
plot_quali_hist(data, "hebergement_gratuit", 11)
