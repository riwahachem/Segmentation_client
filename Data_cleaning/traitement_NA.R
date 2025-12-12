library(mice)
library(missForest)

int_vars <- c("nb_epargne", "nb_pret_immo", "nb_pret_conso","etude_partagee","nb_rdv_fait","nb_rdv_pas_fait","age_emprunteur")

# Table data_loc_avec
load("Data/data_loc_avec_NA.RData")

miss_forest_l1 = missForest(data_loc_avec, variablewise = TRUE)

perf_l1 <- data.frame(
  variable = colnames(data_loc_avec),
  OOBerror = miss_forest_l1$OOBerror
)
data_loc_avec <- miss_forest_l1$ximp
data_loc_avec = subset(data_loc_avec, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, apl, nb_doc_espace_client))

data_loc_avec$signature_electronique = as.factor(data_loc_avec$signature_electronique)

int_vars_loc_avec <- intersect(int_vars, colnames(data_loc_avec))

data_loc_avec[int_vars_loc_avec] <- lapply(data_loc_avec[int_vars_loc_avec], as.integer)

# Table data_loc_sans
load("Data/data_loc_sans_NA.RData")

miss_forest_l2 = missForest(data_loc_sans, variablewise = TRUE)
perf_l2 <- data.frame(
  variable = colnames(data_loc_sans),
  OOBerror = miss_forest_l2$OOBerror
)
data_loc_sans <- miss_forest_l2$ximp
data_loc_sans = subset(data_loc_sans, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, apl, nb_doc_espace_client))
int_vars_loc_sans <- intersect(int_vars, colnames(data_loc_sans))
data_loc_sans[int_vars_loc_sans] <- lapply(data_loc_sans[int_vars_loc_sans], as.integer)

# Table data_prop_avec
load("Data/data_prop_avec_NA.RData")

miss_forest_p1 = missForest(data_prop_avec, variablewise = TRUE)
perf_p1 <- data.frame(
  variable = colnames(data_prop_avec),
  OOBerror = miss_forest_p1$OOBerror
)
data_prop_avec <- miss_forest_p1$ximp
data_prop_avec = subset(data_prop_avec, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, nb_doc_espace_client))

int_vars_prop_avec <- intersect(int_vars, colnames(data_prop_avec))
data_prop_avec[int_vars_prop_avec] <- lapply(data_prop_avec[int_vars_prop_avec], as.integer)

# Table data_prop_sans
load("Data/data_prop_sans_NA.RData")

miss_forest_p2 = missForest(data_prop_sans, variablewise = TRUE)
perf_p2 <- data.frame(
  variable = colnames(data_prop_sans),
  OOBerror = miss_forest_p2$OOBerror
)
data_prop_sans <- miss_forest_p2$ximp
data_prop_sans = subset(data_prop_sans, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, nb_doc_espace_client))

int_vars_prop_sans <- intersect(int_vars, colnames(data_prop_sans))
data_prop_sans[int_vars_prop_sans] <- lapply(data_prop_sans[int_vars_prop_sans], as.integer)

save(data_loc_avec, file = "Data/data_loc_avec.RData")
save(data_loc_sans, file = "Data/data_loc_sans.RData")
save(data_prop_avec, file = "Data/data_prop_avec.RData")
save(data_prop_sans, file = "Data/data_prop_sans.RData")
