library(dplyr)
library(lubridate)

load("data/new_data")
data = data_to_umontpellier
rm(data_to_umontpellier)

load("data/data_to_umontpellier_new1")
data_de_base = data_to_umontpellier
rm(data_to_umontpellier)

data_de_base <- data_de_base[, colnames(data_de_base) %in% colnames(data)]

data_non_relancer <- data_de_base %>% 
  filter(a_ete_relance == FALSE)

data_non_relancer = subset(data_non_relancer, select = -a_ete_relance)

prop_non_relance = nrow(data_non_relancer)/nrow(data_de_base) * 100

data = data_non_relancer

# Retirer status = en cours
status_en_cours <- c(
  "encours_azur",
  "encours_commercial",
  "encours_depot_banque",
  "en_attente_azur",
  "en_contestation_banque",
  "attente_signature_client",
  "envoi_bfc",
  "potentiel",
  "signature_offre_de_pret",
  "accord_banque",
  "fin_azur"
)

data <- data %>% 
  filter(!status %in% status_en_cours)

data <- data %>% 
  filter(prochain_rdv <= as.Date("2025-11-01") | is.na(prochain_rdv))


# Colonnes d'identifiants
cols_id <- c(
  "id_utilisateur", "id_rendez_vous", "premier_id_rendez_vous", "id_conso", 
  "id_immo", "id_dossier","reference_pret_offre"
)

# Colonnes info personnelles 
cols_perso <- c("profession_complement")

# Colonnes datesuni
cols_dates <- c(
  "date_derniere_modification_dossier","date_premier_rdv_prevu", 
  "date_creation_premier_rdv_prevu","date_heure_premier_rdv","date_heure_depot_banque", 
  "date_heure_suivi_max", "date_rdv_en_cours_annulation", "date_heure_accord_banque",
  "date_heure_contestation", "date_derniere_relation", "date_last_rdv_pas_fait",
  "date_heure_min_depot_banque", "date_heure_min_refus_banque","coemprunteur_date_renonciation", 
  "emprunteur_date_renonciation",  "date_rdv_en_cours_annulation", 
  "consultation_doc_espace_client", "mise_a_dispo_espace_client", "date_suivi_max", "date_suivi_J1",
  "date_validation_suivi_J1", "date_dossier_risque", "date_derniere_relation_banque", 
  "date_last_rdv_realise", "date_derniere_relation_emise_client", "date_min_depot_banque",
  "date_signature_op", "date_depot_banque", "prochain_rdv", "prochain_rdv_etat"
)

# Colonnes textuelles et autres 
cols_autres <- c(
  "libelle_support", "type_refus","mensualite_nouvelle", "montant_commission",
  "risque_remarque", "risque_delai_remarque", "envoie_adp", "envoie_paa",
  "epargne_refuse","honoraire", "garanties_assurance_coemprunteur", "garanties_assurance_emprunteur",
  "attente_status", "montant_hib", "montant_hib_fictif", "montant_a_financer", "status_virtuel",
  "pret_conso_montant_inital", "pret_conso_crd_all", "pret_conso_echeance_all", "pret_conso_echeance",
  "pret_conso_crd", "pret_immo_montant_inital", "pret_immo_echeance_all", "pret_immo_echeance",
  "pret_immo_crd_all", "pret_immo_crd", "etat_premier_rdv","categorie_relance", "date_heure_accord_banque",
  "emprunteur_status_renonciation", "coemprunteur_status_renonciation", "emprunteur_quotite_renonciation",
  "coemprunteur_quotite_renonciation", "last_etat_depot", "last_etat_depot_notaire", "etat_epargne",
  "etat_rdv_signature_op", "etat_epargne_refus", "type_support","type_prochain_rdv"
)

# Colonnes assurances
cols_assurance <- c(
  "assurance_groupe_coemprunteur", "assurance_groupe_emprunteur", "status_assurance_coemprunteur",
  "status_assurance_emprunteur", "mandat_assurance_coemprunteur", "mandat_assurance_emprunteur",
  "prime_assurance_coemprunteur", "prime_assurance_emprunteur", "quotite_assurance_coemprunteur",
  "quotite_assurance_emprunteur", "compagnie_assurance_coemprunteur", "compagnie_assurance_emprunteur"
)

# Colonnes à supprimer 
cols_a_supprimer <- c(
  cols_id,
  cols_perso,
  cols_dates,
  cols_autres,
  cols_assurance
)

data <- data %>% select(-all_of(cols_a_supprimer))

save(data, file = "data/data.RData")
save(data_de_base, file = "data/data_de_base.RData")