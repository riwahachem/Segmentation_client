library(dplyr)
load("Data/data.RData")

# Locataire
data_loc = data[data$type_dossier == "locataire",]
donnees_manquantes_l <- colSums(is.na(data_loc)) / nrow(data_loc) * 100
lapply(data_loc, unique)

data_loc = subset(data_loc, select = -c(nb_pret_immo, revenu_foncier, pret_immo_conserver,
                                        part_immo, type_garantie, transfert_adp_status,
                                        type_dossier, rdv_notaire))

data_loc$risque_delai = ifelse(is.na(data_loc$risque_delai), 0, 1)
data_loc$risque_delai = as.factor(data_loc$risque_delai)
data_loc$reserve_levee = ifelse(is.na(data_loc$reserve_levee), 0, 1)
data_loc$reserve_levee = as.factor(data_loc$reserve_levee)

# rdv_notaire : 0 
# risque_delai : NA ou 0
# reserve_levee : NA ou 0

data_loc_avec = data_loc[data_loc$coemprunteur == 1,]
donnees_manquantes_l1 <- colSums(is.na(data_loc_avec)) / nrow(data_loc_avec) * 100
lapply(data_loc_avec, unique)
data_loc_avec$etude_partagee = ifelse(is.na(data_loc_avec$etude_partagee), 0, 1)
data_loc$etude_partagee = as.factor(data_loc$etude_partagee)

data_loc_avec$signature_electronique = ifelse(is.na(data_loc_avec$signature_electronique), 0, 1)
data_loc$signature_electronique = as.factor(data_loc$signature_electronique)

# etude_partagee : 0 ou NA
# signature_electronique : 0 ou NA
data_loc_avec = subset(data_loc_avec, select = -c(coemprunteur))

data_loc_sans = data_loc[data_loc$coemprunteur == 0,]
donnees_manquantes_l2 <- colSums(is.na(data_loc_sans)) / nrow(data_loc_sans) * 100
lapply(data_loc_sans, unique)
data_loc_sans = subset(data_loc_sans, select = -c(age_coemprunteur, situation_familliale_coemprunteur, type_contrat_coemprunteur,coemprunteur))

# Propriétaire
data_prop = data[data$type_dossier == "proprietaire",]
donnees_manquantes_p <- colSums(is.na(data_prop)) / nrow(data_prop) * 100
lapply(data_prop, unique)
data_prop = subset(data_prop, select = -c(hebergement_gratuit, type_dossier, apl))
# type_dossier : proprietaire
# hebergement_gratuit : NA

# Propriétaire avec coemprunteur
data_prop_avec = data_prop[data_prop$coemprunteur == 1,]
donnees_manquantes_p1 <- colSums(is.na(data_prop_avec)) / nrow(data_prop_avec) * 100
lapply(data_prop_avec, unique)
data_prop_avec = subset(data_prop_avec, select = -c(coemprunteur))

data_prop_sans = data_prop[data_prop$coemprunteur == 0,]
donnees_manquantes_p2 <- colSums(is.na(data_prop_sans)) / nrow(data_prop_sans) * 100
lapply(data_prop_sans, unique)
data_prop_sans = subset(data_prop_sans, select = -c(age_coemprunteur, situation_familliale_coemprunteur, type_contrat_coemprunteur, coemprunteur))

save(data_loc_avec, file = "Data/data_loc_avec_NA.RData")
save(data_loc_sans, file = "Data/data_loc_sans_NA.RData")
save(data_prop_avec, file = "Data/data_prop_avec_NA.RData")
save(data_prop_sans, file = "Data/data_prop_sans_NA.RData")
