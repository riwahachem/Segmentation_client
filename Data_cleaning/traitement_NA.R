library(mice)
library(missForest)

# Table data
load("Data/data.RData")
donnees_manquantes <- colSums(is.na(data)) / nrow(data) * 100

miss_forest = missForest(data, variablewise = TRUE)
perf <- data.frame(
  variable = colnames(data),
  OOBerror = miss_forest$OOBerror
)
data_impute <- miss_forest$ximp

# Suppression des variables avec plus de 90% de NA et mauvaise perf
data_impute = subset(data_impute, select = -c(montant, duree, taux_offre, montant_versement_annuel, 
                                              allocation_familiale, apl, nb_doc_espace_client,
                                              banque, agence, type_contrat_coemprunteur))
save(data_impute, file = "Data_Cleaning/data_finale.RData")

# Table data_loc_avec
load("Data/data_loc_avec.RData")

miss_forest_l1 = missForest(data_loc_avec, variablewise = TRUE)

perf_l1 <- data.frame(
  variable = colnames(data_loc_avec),
  OOBerror = miss_forest_l1$OOBerror
)
data_loc_avec <- miss_forest_l1$ximp
data_loc_avec = subset(data_loc_avec, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, apl, nb_doc_espace_client))

# Table data_loc_sans
load("Data/data_loc_sans.RData")

miss_forest_l2 = missForest(data_loc_sans, variablewise = TRUE)
perf_l2 <- data.frame(
  variable = colnames(data_loc_sans),
  OOBerror = miss_forest_l2$OOBerror
)
data_loc_sans <- miss_forest_l2$ximp
data_loc_sans = subset(data_loc_sans, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, apl, nb_doc_espace_client))

# Table data_prop_avec
load("Data/data_prop_avec.RData")

miss_forest_p1 = missForest(data_prop_avec, variablewise = TRUE)
perf_p1 <- data.frame(
  variable = colnames(data_prop_avec),
  OOBerror = miss_forest_p1$OOBerror
)
data_prop_avec <- miss_forest_p1$ximp
data_prop_avec = subset(data_prop_avec, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, nb_doc_espace_client))

# Table data_prop_sans
load("Data/data_prop_sans.RData")

miss_forest_p2 = missForest(data_prop_sans, variablewise = TRUE)
perf_p2 <- data.frame(
  variable = colnames(data_prop_sans),
  OOBerror = miss_forest_p2$OOBerror
)
data_prop_sans <- miss_forest_p2$ximp
data_prop_sans = subset(data_prop_sans, select = -c(montant, duree, banque, montant_versement_annuel,
                                                    allocation_familiale, nb_doc_espace_client))


library(writexl)
write_xlsx(perf_p1, "performance_missforest_p1.xlsx")
write_xlsx(perf_p2, "performance_missforest_p2.xlsx")
write_xlsx(perf_l1, "performance_missforest_l1.xlsx")
write_xlsx(perf_l2, "performance_missforest_l2.xlsx")

donnees_manquantes_l1 <- colSums(is.na(data_loc_avec)) / nrow(data_loc_avec) * 100
tbl_manquants_l1 <- data.frame(
  variable = names(donnees_manquantes_l1),
  pourcentage_NA = donnees_manquantes_l1
)
write_xlsx(tbl_manquants_l1, "donnees_manquantes_l1.xlsx")

donnees_manquantes_l2 <- colSums(is.na(data_loc_sans)) / nrow(data_loc_sans) * 100
tbl_manquants_l2 <- data.frame(
  variable = names(donnees_manquantes_l2),
  pourcentage_NA = donnees_manquantes_l2
)
write_xlsx(tbl_manquants_l2, "donnees_manquantes_l2.xlsx")

donnees_manquantes_p1 <- colSums(is.na(data_prop_avec)) / nrow(data_prop_avec) * 100
tbl_manquants_p1 <- data.frame(
  variable = names(donnees_manquantes_p1),
  pourcentage_NA = donnees_manquantes_p1
)
write_xlsx(tbl_manquants_p1, "donnees_manquantes_p1.xlsx")

donnees_manquantes_p2 <- colSums(is.na(data_prop_sans)) / nrow(data_prop_sans) * 100
tbl_manquants_p2 <- data.frame(
  variable = names(donnees_manquantes_p2),
  pourcentage_NA = donnees_manquantes_p2
)
write_xlsx(tbl_manquants_p2, "donnees_manquantes_p2.xlsx")

save(data_loc_avec, file = "Data/data_loc_avec.RData")
save(data_loc_sans, file = "Data/data_loc_sans.RData")
save(data_prop_avec, file = "Data/data_prop_avec.RData")
save(data_prop_sans, file = "Data/data_prop_sans.RData")