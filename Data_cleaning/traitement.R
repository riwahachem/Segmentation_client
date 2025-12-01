library(dplyr)
library(lubridate)

load("Data_Cleaning/data.RData")

data$montant[data$montant < 12000] <- "inconnu"

data$taux_offre[data$taux_offre > 300] <- "inconnu"

data$date_realisation_premier_rdv <- ifelse(is.na(data$date_realisation_premier_rdv), 0, 1)
data$premier_rdv_realise <- data$date_realisation_premier_rdv
data$date_realisation_premier_rdv <- NULL

data$signature_electronique <- ifelse(
  data$signature_electronique == "Y", 1,
  ifelse(data$signature_electronique == "N", 0, "inconnu"))

data <- data %>% 
  mutate(
    age_emprunteur = as.numeric(floor((date_creation - emprunteur_date_naissance) / 365.25)),
    age_coemprunteur = as.numeric(floor((date_creation - coemprunteur_date_naissance) / 365.25))
  ) %>%
  mutate(
    age_emprunteur = ifelse(age_emprunteur < 18 | age_emprunteur > 90, "inconnu", age_emprunteur),
    age_coemprunteur = ifelse(age_coemprunteur < 18 | age_coemprunteur > 90, "inconnu", age_coemprunteur)
  )%>%
  select(-date_creation, -emprunteur_date_naissance, -coemprunteur_date_naissance)
data$age_emprunteur = as.integer(data$age_emprunteur)
data$age_coemprunteur = as.integer(data$age_coemprunteur)

data$est_refuse <- ifelse(is.na(data$date_refus), 0, 1)
data$date_refus <- NULL

data$rdv_notaire <- ifelse(is.na(data$date_rdv_notaire), 0, 1)
data$date_rdv_notaire <- NULL

data$banque_accord <- ifelse(is.na(data$date_premier_accord), 0, 1)
data$date_premier_accord <- NULL

data <- data %>%
  mutate(
    est_conteste = case_when(
      # deux dates présentes ET contestation après refus
      !is.na(date_min_refus_banque) & !is.na(date_contestation) &
        date_contestation > date_min_refus_banque ~ "conteste",
      
      # refus présent mais pas de contestation
      !is.na(date_min_refus_banque) & is.na(date_contestation) ~ "pas conteste",
      
      # aucune des deux dates
      is.na(date_min_refus_banque) & is.na(date_contestation) ~ NA_character_,
      
      # contestation présente mais pas de refus
      is.na(date_min_refus_banque) & !is.na(date_contestation) ~ "inconnu"
    )
  )
data <- data %>% select(-date_min_refus_banque, -date_contestation)

data$est_encaisse <- ifelse(is.na(data$date_encaissement_commercial), 0, 1)
data$date_encaissement_commercial <- NULL

data <- data %>%
  mutate(
    retour_bfc = case_when(
      (!is.na(date_envoi_bfc) & !is.na(date_reception_bfc)) | (!is.na(date_envoi_bfc) & !is.na(date_potentiel)) ~ "envoi et retour",
      
      !is.na(date_envoi_bfc) &  is.na(date_reception_bfc) & is.na(date_potentiel) ~ "envoi et pas de retour",
      
      is.na(date_envoi_bfc)  &  is.na(date_reception_bfc) & is.na(date_potentiel) ~ "pas d'envoi",
      
      is.na(date_envoi_bfc)  & (is.na(date_reception_bfc) | is.na(date_potentiel)) ~ "inconnu"
    )
  )
data <- data %>% select(-date_envoi_bfc, -date_reception_bfc, -date_potentiel)

data$est_frigo <- ifelse(is.na(data$date_fin_previsible_frigo), 0, 1)
data$date_fin_previsible_frigo <- NULL

data$apl[data$apl < 5] <- "inconnu"

data$allocation_familiale[data$allocation_familiale < 10] <- "inconnu"

save(data, file = "Data_Cleaning/data_nouveau.RData")