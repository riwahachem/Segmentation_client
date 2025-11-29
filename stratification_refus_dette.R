library(dplyr)
load("data_nouveau.RData")

data_accord = data[data$status == "encaisse",]
donnees_manquantes_a <- colSums(is.na(data_accord)) / nrow(data_accord) * 100
lapply(data_accord, unique)
data_accord = subset(data_accord, select=-c(status,motif_refus,type_support,premier_rdv_realise,est_refuse,banque_accord,est_encaisse))
# status : encaisse
# motif_refus : NA
# type_support = Internet
# dette_retard_loyer : que 1 : 1996 et que des : 0 ou NA
# premier_rdv_realise : 1
# est_refuse : 0
# banque_accord : 1
# est_encaisse : 1

data_frigo = data[data$status == "frigo",]
donnees_manquantes_f <- colSums(is.na(data_frigo)) / nrow(data_frigo) * 100
lapply(data_frigo, unique)
data_frigo = subset(data_frigo, select=-c(status,type_support,reserve_levee,dette_retard_loyer,etude_partagee,premier_rdv_realise,rdv_notaire,est_encaisse))
# status : frigo
# type_support : Internet
# motif_refus : Refus CF après 1e rendez vous ou NA
# scoring : FRIGO ou NA
# reserve_levee : N
# apl : que 1 : 53, que 1 : 290 et que des inconnu
# dette_retard_loyer : 0
# etude_partagee : N
# risque_delai : NA ou N
# premier_rdv_realise : 1
# rdv_notaire : 0
# est_encaisse : 0


data_refus <- data %>% filter(!status %in% c("encaisse", "frigo"))
donnees_manquantes_r <- colSums(is.na(data_refus)) / nrow(data_refus) * 100
lapply(data_refus, unique)
data_refus = subset(data_refus, select=-c(type_support,est_encaisse))
# type_support : Internet
# est_encaisse : 0

data_refus_dette <- data_refus[!is.na(data_refus$dette_famille_ami), ]
donnees_manquantes_r1 <- colSums(is.na(data_refus_dette)) / nrow(data_refus_dette) * 100
lapply(data_refus_dette, unique)

data_refus_sans_dette <- data_refus[is.na(data_refus$dette_famille_ami), ]
donnees_manquantes_r2 <- colSums(is.na(data_refus_sans_dette)) / nrow(data_refus_sans_dette) * 100
lapply(data_refus_sans_dette[,1:56], unique)
data_refus_sans_dette = subset(data_refus_sans_dette, select=-c(dette_retard_loyer,dette_famille_ami,
                                                                dette_retard_impot,dette_decouvert,
                                                                dette_autre, dette_saisie_sur_salire,
                                                                avis_a_tiers_detenteurs, risque_delai,
                                                                transfert_paa_status))
# type_garantie : NA ou SGAR
# part_immo : NA ou 0
# reserve_levee : NA ou N
# toutes les dettes : NA
# avis_a_tiers_detenteurs : NA
# etude_partagee : NA ou N
# risque delai : NA
# transfert_adp_status : NA ou transmis - transfert : ok
# transfert_paa_status : NA

# DONNÉES MANQUANTES TABLE ACCORD 

# DONNÉES MANQUANTES TABLE FRIGO 

# DONNÉES MANQUANTES TABLE REFUS AVEC DETTES

# DONNÉES MANQUANTES TABLE REFUS SANS DETTES


