library(dplyr)
library(lubridate)

load("Data/data.RData")

# VARIABLES QUANTITATIVES
data$est_aggregation <- ifelse(is.na(data$est_aggregation), 0, 1)
data$est_aggregation = as.factor(data$est_aggregation)

data$montant[data$montant < 12000] <- NA
data$taux_offre[data$taux_offre > 300] <- NA
data$apl[data$apl < 5] <- NA
data$allocation_familiale[data$allocation_familiale < 10] <- NA

data <- data %>% 
  mutate(
    age_emprunteur = as.numeric(floor((date_creation - emprunteur_date_naissance) / 365.25)),
    age_coemprunteur = as.numeric(floor((date_creation - coemprunteur_date_naissance) / 365.25))
  ) %>%
  mutate(
    age_emprunteur = ifelse(age_emprunteur < 18 | age_emprunteur > 90, NA, age_emprunteur),
    age_coemprunteur = ifelse(age_coemprunteur < 18 | age_coemprunteur > 90, NA, age_coemprunteur)
  )%>%
  select(-date_creation, -emprunteur_date_naissance, -coemprunteur_date_naissance)
data$age_emprunteur = as.integer(data$age_emprunteur)
data$age_coemprunteur = as.integer(data$age_coemprunteur)

data$nb_epargne = as.integer(data$nb_epargne)
data$nb_pret_conso = as.integer(data$nb_pret_conso)
data$nb_pret_immo = as.integer(data$nb_pret_immo)
data$nb_doc_espace_client = as.integer(data$nb_doc_espace_client)
data$nb_rdv_fait = as.integer(data$nb_rdv_fait)
data$nb_rdv_pas_fait = as.integer(data$nb_rdv_pas_fait)

# VARIABLES FACTORIELLES

data$signature_electronique <- ifelse(
  data$signature_electronique == "Y", 1,
  ifelse(data$signature_electronique == "N", 0, NA))

data$est_refuse <- ifelse(is.na(data$date_refus), 0, 1)
data$date_refus <- NULL
data$est_refuse = as.factor(data$est_refuse)

data$rdv_notaire <- ifelse(is.na(data$date_rdv_notaire), 0, 1)
data$date_rdv_notaire <- NULL
data$rdv_notaire = as.factor(data$rdv_notaire)

data$banque_accord <- ifelse(is.na(data$date_premier_accord), 0, 1)
data$date_premier_accord <- NULL
data$banque_accord = as.factor(data$banque_accord)

data$est_encaisse <- ifelse(is.na(data$date_encaissement_commercial), 0, 1)
data$date_encaissement_commercial <- NULL
data$est_encaisse = as.factor(data$est_encaisse)

data$premier_rdv_realise <- ifelse(is.na(data$date_realisation_premier_rdv), 0, 1)
data$date_realisation_premier_rdv <- NULL
data$premier_rdv_realise = as.factor(data$premier_rdv_realise)

data$est_frigo <- ifelse(
  is.na(data$date_fin_previsible_frigo), 0,
  ifelse(data$date_fin_previsible_frigo < as.Date("2025-11-01"), 0, 1)
)
data$date_fin_previsible_frigo <- NULL
data$est_frigo = as.factor(data$est_frigo)

data$pret_conso_conserve <- ifelse(
  data$pret_conso_conserve == "Y", 1,
  ifelse(data$pret_conso_conserve == "N", 0, NA))
data$pret_conso_conserve = as.factor(data$pret_conso_conserve)

data$pret_immo_conserver <- ifelse(
  data$pret_immo_conserver == "Y", 1,
  ifelse(data$pret_immo_conserver == "N", 0, NA))
data$pret_immo_conserver = as.factor(data$pret_immo_conserver)

data$reserve_levee <- ifelse(
  data$reserve_levee == "Y", 1,
  ifelse(data$reserve_levee == "N", 0, NA))
data$reserve_levee = as.factor(data$reserve_levee)

data$etude_partagee <- ifelse(
  data$etude_partagee == "Y", 1,
  ifelse(data$etude_partagee == "N", 0, NA))
data$etude_partagee = as.factor(data$etude_partagee)

data$dossier_a_risque <- ifelse(
  data$dossier_a_risque == "Y", 1,
  ifelse(data$dossier_a_risque == "N", 0, NA))
data$dossier_a_risque= as.factor(data$dossier_a_risque)

data$risque_delai <- ifelse(
  data$risque_delai == "Y", 1,
  ifelse(data$risque_delai == "N", 0, NA))
data$risque_delai= as.factor(data$risque_delai)

data$espace_client_est_valide = as.factor(data$espace_client_est_valide)

data$signature_electronique <- ifelse(data$banque_accord == 0 & data$signature_electronique == 1, NA, data$signature_electronique)
data$signature_electronique = as.factor(data$signature_electronique)

data <- data %>%
  mutate(
    est_conteste = case_when(
      # deux dates présentes ET contestation après refus
      !is.na(date_min_refus_banque) & !is.na(date_contestation) &
        date_contestation > date_min_refus_banque ~ 1,
      
      # refus présent mais pas de contestation
      !is.na(date_min_refus_banque) & is.na(date_contestation) ~ 0,
      
      # aucune des deux dates
      is.na(date_min_refus_banque) & is.na(date_contestation) ~ NA,
      
      # contestation présente mais pas de refus
      is.na(date_min_refus_banque) & !is.na(date_contestation) ~ NA
    )
  )
data <- data %>% select(-date_min_refus_banque, -date_contestation)
data$est_conteste = as.factor(data$est_conteste)

# VARIABLES QUALITATIVES

data <- data %>%
  mutate(
    retour_bfc = case_when(
      (!is.na(date_envoi_bfc) & !is.na(date_reception_bfc)) | (!is.na(date_envoi_bfc) & !is.na(date_potentiel)) ~ "envoi et retour",
      
      !is.na(date_envoi_bfc) &  is.na(date_reception_bfc) & is.na(date_potentiel) ~ "envoi et pas de retour",
      
      is.na(date_envoi_bfc)  &  is.na(date_reception_bfc) & is.na(date_potentiel) ~ "pas d'envoi",
      
      is.na(date_envoi_bfc)  & (is.na(date_reception_bfc) | is.na(date_potentiel)) ~ NA
    )
  )
data <- data %>% select(-date_envoi_bfc, -date_reception_bfc, -date_potentiel)

data$dette = rowSums(data[,c("dette_autre","dette_decouvert", "dette_famille_ami", "dette_retard_impot",
                             "dette_retard_loyer", "dette_saisie_sur_salire", "avis_a_tiers_detenteurs")])
data = subset(data, select = -c(dette_autre,dette_decouvert, dette_famille_ami, dette_retard_impot,
                                dette_retard_loyer, dette_saisie_sur_salire, avis_a_tiers_detenteurs))

data$dette <- ifelse(
  is.na(data$dette), "dette inconnue",
  ifelse(data$dette==0, "dette a zero", "dette existante")
)

data$coemprunteur = ifelse(is.na(data$age_coemprunteur), 0, 1)
data$coemprunteur = as.factor(data$coemprunteur)

data <- data %>%
  mutate(across(where(is.character), as.factor))

data <- data %>% distinct()
data = subset(data, select = -c(cle_unique_mail, cle_unique_tel))

save(data, file = "Data/data.RData")