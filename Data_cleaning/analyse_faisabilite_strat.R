library(dplyr)
load("Data_Cleaning/data_nouveau.RData")

data_accord <- data[data$status == "encaisse",]
lapply(data_accord, function(x) head(unique(x,7)))
data_accord <- data_accord %>%
  select(-c(status, est_refuse, banque_accord, est_encaisse, est_frigo, motif_refus))
# retour_bfc 
data_frigo <- data[data$status == "frigo",]
lapply(data_frigo, function(x) head(unique(x,7)))
data_frigo <- data_frigo %>% select(-c(status,reserve_levee,etude_partagee,rdv_notaire,est_encaisse))
data_frigo <- data_frigo %>%
  mutate(
    signature_electronique = as.factor(ifelse(is.na(signature_electronique), 0, 1)),
    
    scoring = as.factor(ifelse(is.na(scoring), "Non Defini", as.character(scoring))),
    
    motif_refus = as.factor(ifelse(is.na(motif_refus), "Pas de motif", as.character(motif_refus))),
    
    risque_delai = as.factor(ifelse(is.na(risque_delai), 0, as.character(risque_delai)))
  )
# Problème : est_frigo : 0/1, banque_accord 0/1, est_refuse 0/1
data_refus_base <- data %>% filter(!status %in% c("encaisse", "frigo"))

data_refus_prop <- data_refus_base %>% filter(type_dossier == "proprietaire")
data_refus_prop <- data_refus_prop %>% distinct()
data_refus_prop$type_dossier <- NULL
lapply(data_refus_prop, function(x) head(unique(x,7)))
data_refus_prop <- data_refus_prop %>% select(-c(est_encaisse,hebergement_gratuit))
# dans hebergement gratuit y a que des NA 

data_refus_loc <- data_refus_base %>% filter(type_dossier != "proprietaire")
data_refus_loc <- data_refus_loc %>% distinct()
data_refus_loc$type_dossier <- NULL
lapply(data_refus_loc, function(x) head(unique(x,7)))
data_refus_loc <- data_refus_loc %>% select(-c(rdv_notaire,est_encaisse))
data_refus_loc$type_garantie <- as.factor(ifelse(is.na(data_refus_loc$type_garantie), 1, 0))
data_refus_loc$part_immo <- as.factor(ifelse(is.na(data_refus_loc$part_immo), 1, 0))
data_refus_loc$reserve_levee <- as.factor(ifelse(is.na(data_refus_loc$reserve_levee), 1, 0))
data_refus_loc$etude_partagee <- as.factor(ifelse(is.na(data_refus_loc$etude_partagee), 1, 0))
data_refus_loc$risque_delai <- as.factor(ifelse(is.na(data_refus_loc$risque_delai), 1, 0))
data_refus_loc$signature_electronique <- as.factor(ifelse(is.na(data_refus_loc$signature_electronique), 1, 0))

cat("Taux de valeurs manquantes par sous groupe")

cat("accord")
print(colSums(is.na(data_accord)) / nrow(data_accord) * 100)

cat("frigo")
print(colSums(is.na(data_frigo)) / nrow(data_frigo) * 100)

cat("refus proprietaire")
print(colSums(is.na(data_refus_prop)) / nrow(data_refus_prop) * 100)

cat("refus locataire")
print(colSums(is.na(data_refus_loc)) / nrow(data_refus_loc) * 100)


save(data_accord, file = "Data_Cleaning/data_accord.RData")
save(data_frigo, file = "Data_Cleaning/data_frigo.RData")
save(data_refus_prop, file = "Data_Cleaning/data_refus_prop.RData")
save(data_refus_loc, file = "Data_Cleaning/data_refus_loc.RData")
