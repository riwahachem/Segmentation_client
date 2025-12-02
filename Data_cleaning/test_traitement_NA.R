library(mice)

load("Data_Cleaning/data_accord.RData")
load("Data_Cleaning/data_frigo.RData")
load("Data_Cleaning/data_refus_dette.RData")
load("Data_Cleaning/data_refus_sans_dette.RData")

#ACCORD

# Tableau du pattern de valeurs manquantes
#md.pattern(data_accord)

# Stripplot des NA pour quelques variables
#mice_plot <- mice(data_accord[, c("montant_versement_annuel", "produit_epargne", "pret_conso_conserve",
#                                  "pret_immo_conserver","type_contrat_emprunteur")], m = 1, maxit = 0)
#stripplot(mice_plot)

data_accord = subset(data_accord, select = -c(agence,prochain_rdv, type_prochain_rdv, prochain_rdv_etat))
mice = mice(data = data_accord, m=5, method = "pmm", maxit = 5)
data_accord_impute <- complete(mice, 1)

#FRIGO

data_frigo = subset(data_frigo, select = -c(agence,prochain_rdv, type_prochain_rdv, prochain_rdv_etat))
mice = mice(data = data_frigo, m=5, method = "pmm", maxit = 5)
data_frigo_impute <- complete(mice, 1)

#REFUS DETTE


#REFUS SANS DETTE
