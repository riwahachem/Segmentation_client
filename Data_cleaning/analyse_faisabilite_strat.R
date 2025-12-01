library(dplyr)
load("Data_Cleaning/data_nouveau.RData")

data <- data %>%
  mutate(
    stratification_dette = ifelse(
      is.na(dette_famille_ami) &
        is.na(dette_decouvert) &
        is.na(dette_autre) &
        is.na(dette_retard_loyer) &
        is.na(dette_retard_impot) &
        is.na(dette_saisie_sur_salire) &
        is.na(avis_a_tiers_detenteurs),
      "sans_dette",
      "avec_dette"
    )
  )

data_locataires <- data %>% filter(type_dossier == "locataire")
data_proprietaires <- data %>% filter(type_dossier == "proprietaire")

# Sous-groupes Locataires
loc_sans_dette <- data_locataires %>% filter(stratification_dette == "sans_dette")
loc_avec_dette <- data_locataires %>% filter(stratification_dette == "avec_dette")

# Sous-groupes Propriétaires
prop_sans_dette <- data_proprietaires %>% filter(stratification_dette == "sans_dette")
prop_avec_dette <- data_proprietaires %>% filter(stratification_dette == "avec_dette")

cat("Taux de valeurs manquantes par sous groupe")

cat("locataires sans dette")
# Résultat observé : Montant et Durée sont vides à > 99%
print(colSums(is.na(loc_sans_dette)) / nrow(loc_sans_dette) * 100)

cat("locataires avec dette")
# Montant vide à 95% idem pour la durée
print(colSums(is.na(loc_avec_dette)) / nrow(loc_avec_dette) * 100)

cat("prop sans dette")
# Résultat observé : Beaucoup de variables quasiment vides 
print(colSums(is.na(prop_sans_dette)) / nrow(prop_sans_dette) * 100)

cat("prop avec dette")
# Résultat observé : Quelques variables sont peu renseignées 
print(colSums(is.na(prop_avec_dette)) / nrow(prop_avec_dette) * 100)
