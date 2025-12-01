library(dplyr)
library(VIM)

load("Data_Cleaning/data_accord.RData")
load("Data_Cleaning/data_frigo.RData")
load("Data_Cleaning/data_refus_dette.RData")
load("Data_Cleaning/data_refus_sans_dette.RData")

#ACCORD

vars_knn_a = c("pret_conso_conserve","pret_immo_conserver","type_contrat_emprunteur",
               "espace_client_est_valide","est_conteste", "retour_bfc")

knn_a = list(
  "pret_conso_conserve" = c("age_emprunteur","situation_familliale_emprunteur","allocation_familiale",
                            "type_dossier","apl","pension_alimentaire","nb_pret_conso","scoring"),
  
  "pret_immo_conserver" = c("part_immo","type_dossier","revenu_foncier","age_emprunteur", "situation_familliale_emprunteur",
                            "allocation_familiale", "apl","pension_alimentaire","nb_pret_immo","scoring"),
  
  "type_contrat_emprunteur" = c("age_emprunteur", "situation_familliale_emprunteur", "type_dossier", 
                                "allocation_familiale", "apl"),
  
  "espace_client_est_valide" = c("age_emprunteur","situation_familliale_emprunteur",
                                 "nb_doc_espace_client","type_dossier"),
  
  "est_conteste" = c("age_emprunteur","situation_familliale_emprunteur","dossier_a_risque", "rdv_notaire", "type_dossier"),
  
  "retour_bfc" = c("age_emprunteur","situation_familliale_emprunteur","type_dossier","dossier_a_risque")
)

for (var in names(knn_a)) {
  
  predictors <- knn_a[[var]]
  
  # Créer le sous-dataframe avec la variable + ses prédicteurs
  sub_df <- data_accord %>%
    select(all_of(c(var, predictors)))
  
  # Appliquer le KNN pour cette variable
  imp_df <- kNN(
    sub_df,
    variable = var,
    k = 5,
    imp_var = FALSE
  )
  
  # Remplacer la variable imputée dans le dataset d'origine
  data_accord[[var]] <- imp_df[[var]]
}

vars_mediane_a = c("dette_famille_amis","dette_retard_impot","dette_decouvert","dette_autre",
                   "dette_saisie_sur_salire","avis_a_tiers_detenteurs", "tresorerie_neorac")

for (var in vars_mediane_a) {
  med <- median(data_accord[[var]], na.rm = TRUE)
  data_accord[[var]][is.na(data_accord[[var]])] <- med
}

#FRIGO

vars_knn_f = c("pret_immo_conserver","type_contrat_emprunteur",
               "espace_client_est_valide","est_conteste")
knn_f = list(
  "pret_immo_conserver" = c("type_dossier","revenu_foncier","age_emprunteur", "situation_familliale_emprunteur",
                            "allocation_familiale","pension_alimentaire","nb_pret_immo"),
  
  "type_contrat_emprunteur" = c("age_emprunteur", "situation_familliale_emprunteur", "type_dossier", 
                                "allocation_familiale"),
  
  "espace_client_est_valide" = c("age_emprunteur","situation_familliale_emprunteur","nb_doc_espace_client",
                                 "type_dossier"),
  
  "est_conteste" = c("age_emprunteur","situation_familliale_emprunteur", "type_dossier","dossier_a_risque",
                     "signature_electronique","banque_accord")
)

for (var in names(knn_f)) {
  
  predictors <- knn_f[[var]]
  
  # Créer le sous-dataframe avec la variable + ses prédicteurs
  sub_df <- data_frigo %>%
    select(all_of(c(var, predictors)))
  
  # Appliquer le KNN pour cette variable
  imp_df <- kNN(
    sub_df,
    variable = var,
    k = 5,
    imp_var = FALSE
  )
  
  # Remplacer la variable imputée dans le dataset d'origine
  data_frigo[[var]] <- imp_df[[var]]
}


vars_mediane_f = c("tresorerie_neorac")

for (var in vars_mediane_f) {
  med <- median(data_frigo[[var]], na.rm = TRUE)
  data_frigo[[var]][is.na(data_frigo[[var]])] <- med
}

#REFUS DETTE (variables à rajouter age_emprunteur, age_coemprunteur)

vars_knn_rd = c("motif_refus", "pret_conso_conserve","type_contrat_emprunteur","age_emprunteur",
                "espace_client_est_valide","situation_familliale_emprunteur")

knn_rd = list(
  "motif_refus" = c("status", "nombre_rejets", "type_dossier", "nombre_commissions_intervention",
                    "banque_accord"),
  
  "pret_conso_conserve" = c("allocation_familiale","type_dossier","apl","pension_alimentaire",
                            "nb_pret_conso","nb_epargne"),
  
  "type_contrat_emprunteur" = c("type_dossier","allocation_familiale","apl","pension_alimentaire"),
  
  "espace_client_est_valide" = c("nb_doc_espace_client","type_dossier","status"),
  
  #"est_conteste" = c("type_dossier","dossier_a_risque", "signature_electronique","banque_accord"),
  
  "situation_familliale_emprunteur" = c("type_dossier","allocation_familiale","apl","pension_alimentaire","revenu_foncier")
)

for (var in names(knn_rd)) {
  
  predictors <- knn_rd[[var]]
  
  # Créer le sous-dataframe avec la variable + ses prédicteurs
  sub_df <- data_refus_dette %>%
    select(all_of(c(var, predictors)))
  
  # Appliquer le KNN pour cette variable
  imp_df <- kNN(
    sub_df,
    variable = var,
    k = 5,
    imp_var = FALSE
  )
  
  # Remplacer la variable imputée dans le dataset d'origine
  data_refus_dette[[var]] <- imp_df[[var]]
}


vars_mediane_rd = c("tresorerie_neorac")

for (var in vars_mediane_rd) {
  med <- median(data_refus_dette[[var]], na.rm = TRUE)
  data_refus_dette[[var]][is.na(data_refus_dette[[var]])] <- med
}

#REFUS SANS DETTE (voir pour la variable âge)

vars_knn_rsd = c("motif_refus","pret_conso_conserve")

knn_rsd = list(
  "motif_refus" = c("status", "nombre_rejets", "type_dossier", "nombre_commissions_intervention",
                    "banque_accord"),
  
  "pret_conso_conserve" = c("allocation_familiale","type_dossier","apl","pension_alimentaire",
                            "nb_pret_conso","nb_epargne"
  )
)

for (var in names(knn_rsd)) {
  
  predictors <- knn_rsd[[var]]
  
  # Créer le sous-dataframe avec la variable + ses prédicteurs
  sub_df <- data_refus_sans_dette %>%
    select(all_of(c(var, predictors)))
  
  # Appliquer le KNN pour cette variable
  imp_df <- kNN(
    sub_df,
    variable = var,
    k = 5,
    imp_var = FALSE
  )
  
  # Remplacer la variable imputée dans le dataset d'origine
  data_refus_sans_dette[[var]] <- imp_df[[var]]
}

vars_mediane_rsd = c("tresorerie_neorac")

for (var in vars_mediane_rsd) {
  med <- median(data_refus_sans_dette[[var]], na.rm = TRUE)
  data_refus_sans_dette[[var]][is.na(data_refus_dette[[var]])] <- med
}