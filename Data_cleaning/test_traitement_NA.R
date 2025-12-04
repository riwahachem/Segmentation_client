library(mice)

load("Data_Cleaning/data_accord.RData")
load("Data_Cleaning/data_frigo.RData")
load("Data_Cleaning/data_refus_rdv.RData")
load("Data_Cleaning/data_refus_sans_rdv.RData")

#ACCORD

# Stripplot des NA pour quelques variables
mice_plot <- mice(data_accord[, c("montant_versement_annuel", "produit_epargne", "pret_conso_conserve",
                                  "pret_immo_conserver","type_contrat_emprunteur")], m = 1, maxit = 0)
stripplot(mice_plot)

mice_a = mice(data = data_accord, m=5, method = "rf", maxit = 5)
data_accord_impute <- complete(mice_a, 1)

# pas traiter la variable situation_familliale_coemprunteur (54% NA)

#FRIGO

data_frigo = subset(data_frigo, select = -c(apl))
mice_f = mice(data = data_frigo, m=5, method = "rf", maxit = 5)
data_frigo_impute <- complete(mice_f, 1)

# transfert_paa_status (93% NA), nb_rdv_pas_fait (95 %NA), situation_familliale_coemprunteur (46% NA), apl (99% NA)

#REFUS RDV

mice_rdv = mice(data = data_refus_rdv, m=5, method = "rf", maxit = 5)
data_refus_rdv_impute <- complete(mice_rdv, 1)

#REFUS SANS RDV
data_refus_sans_rdv = subset(data_refus_sans_rdv, select = -c(reserve_levee, signature_electronique, etude_partagee, dossier_a_risque, nb_rdv_pas_fait))
mice_sans_rdv = mice(data = data_refus_sans_rdv, m=5, method = "rf", maxit = 5)
data_refus_sans_rdv_impute <- complete(mice_sans_rdv, 1)
