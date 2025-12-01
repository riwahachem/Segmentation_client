library(dplyr)
load("Data_Cleaning/data_nouveau.RData")

data_loc = data[data$type_dossier == "locataire",]
donnees_manquantes_l <- colSums(is.na(data_loc)) / nrow(data_loc) * 100
lapply(data_loc, unique)
# reserve_levee : NA ou N
# type_dossier : locataire
# risque_delai : NA ou N

data_prop = data[data$type_dossier == "proprietaire",]
donnees_manquantes_p <- colSums(is.na(data_prop)) / nrow(data_prop) * 100
lapply(data_prop[,1:57], unique)
# type_dossier : proprietaire
# hebergement_gratuit : NA