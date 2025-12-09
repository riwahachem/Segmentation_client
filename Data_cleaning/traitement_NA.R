library(mice)
library(missForest)

load("Data_Cleaning/data_nouveau.RData")
donnees_manquantes <- colSums(is.na(data)) / nrow(data) * 100

miss_forest = missForest(data, variablewise = TRUE)
data_impute <- miss_forest$ximp

perf <- data.frame(
  variable = colnames(data),
  OOBerror = miss_forest$OOBerror
)

# Suppression des variables avec plus de 90% de NA et mauvaise perf

data_impute = subset(data_impute, select = -c(montant, duree, taux_offre, montant_versement_annuel, 
                                              allocation_familiale, apl, nb_doc_espace_client,
                                              banque, agence, type_contrat_coemprunteur))

save(data_impute, file = "Data_Cleaning/data_finale.RData")
