library(dplyr)
load("Data_Cleaning/data_nouveau.RData")

data_accord <- data[data$status == "encaisse",]
data_accord <- data_accord %>%
  select(-c(status, est_refuse, banque_accord, est_encaisse, est_frigo, motif_refus, premier_rdv_realise))
data_accord <- data_accord %>%
  mutate(retour_bfc = as.factor(ifelse(is.na(retour_bfc), "Non Defini", as.character(retour_bfc))))
lapply(data_accord, function(x) head(unique(x,7)))
data_accord <- data_accord %>% distinct()
data_accord = subset(data_accord, select=-c(cle_unique_tel,cle_unique_mail))

data_frigo <- data[data$status == "frigo",]
data_frigo <- data_frigo %>% select(-c(status,reserve_levee,etude_partagee,rdv_notaire,est_encaisse,est_frigo, banque_accord, est_refuse,motif_refus,premier_rdv_realise))

lapply(data_frigo, function(x) head(unique(x,7)))
data_frigo <- data_frigo %>% distinct()
data_frigo = subset(data_frigo, select=-c(cle_unique_tel,cle_unique_mail))
data_frigo$risque_delai <- as.factor(ifelse(is.na(data_frigo$risque_delai), 0, 1))
data_frigo$scoring <- as.factor(ifelse(is.na(data_frigo$scoring), 0, 1))

data_refus_base <- data %>% filter(!status %in% c("encaisse", "frigo"))

data_refus_prop <- data_refus_base %>% filter(type_dossier == "proprietaire")
data_refus_prop <- data_refus_prop %>% distinct()
data_refus_prop$type_dossier <- NULL
data_refus_prop <- data_refus_prop %>% select(-c(est_encaisse,hebergement_gratuit))
lapply(data_refus_prop, function(x) head(unique(x,7)))
data_refus_prop <- data_refus_prop %>% distinct()
data_refus_prop = subset(data_refus_prop, select=-c(cle_unique_tel,cle_unique_mail))

data_refus_loc <- data_refus_base %>% filter(type_dossier != "proprietaire")
data_refus_loc <- data_refus_loc %>% distinct()
data_refus_loc$type_dossier <- NULL
data_refus_loc <- data_refus_loc %>% select(-c(rdv_notaire,est_encaisse))
data_refus_loc$type_garantie <- as.factor(ifelse(is.na(data_refus_loc$type_garantie), 1, 0))
data_refus_loc$part_immo <- as.factor(ifelse(is.na(data_refus_loc$part_immo), 1, 0))
data_refus_loc$reserve_levee <- as.factor(ifelse(is.na(data_refus_loc$reserve_levee), 1, 0))
data_refus_loc$etude_partagee <- as.factor(ifelse(is.na(data_refus_loc$etude_partagee), 1, 0))
data_refus_loc$risque_delai <- as.factor(ifelse(is.na(data_refus_loc$risque_delai), 1, 0))
data_refus_loc$signature_electronique <- as.factor(ifelse(is.na(data_refus_loc$signature_electronique), 1, 0))
lapply(data_refus_loc, function(x) head(unique(x,7)))
data_refus_loc <- data_refus_loc %>% distinct()
data_refus_loc = subset(data_refus_loc, select=-c(cle_unique_tel,cle_unique_mail))

cat("Taux de valeurs manquantes par sous groupe")

cat("accord")
a <- print(colSums(is.na(data_accord)) / nrow(data_accord) * 100)
sort(a)

cat("frigo")
b <- print(colSums(is.na(data_frigo)) / nrow(data_frigo) * 100)
sort(b)

cat("refus proprietaire")
c <- print(colSums(is.na(data_refus_prop)) / nrow(data_refus_prop) * 100)
sort(c)

cat("refus locataire")
d <- print(colSums(is.na(data_refus_loc)) / nrow(data_refus_loc) * 100)
sort(d)

save(data_accord, file = "Data_Cleaning/data_accord.RData")
save(data_frigo, file = "Data_Cleaning/data_frigo.RData")
save(data_refus_prop, file = "Data_Cleaning/data_refus_prop.RData")
save(data_refus_loc, file = "Data_Cleaning/data_refus_loc.RData")
