library(mice)

load("Data_Cleaning/data_accord.RData")
load("Data_Cleaning/data_frigo.RData")
load("Data_Cleaning/data_refus_dette.RData")
load("Data_Cleaning/data_refus_sans_dette.RData")

#ACCORD

# Tableau du pattern de valeurs manquantes
md.pattern(data_accord)

# Stripplot des NA pour quelques variables
mice_plot <- mice(data_accord[, c("montant", "type_garantie", "scoring", 
                                  "type_dossier", "allocation_familiale", 
                                  "dossier_a_risque", "age_emprunteur")],
                  m = 1, maxit = 0)
stripplot(mice_plot)

#FRIGO


#REFUS DETTE


#REFUS SANS DETTE