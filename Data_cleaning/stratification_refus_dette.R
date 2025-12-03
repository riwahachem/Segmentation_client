library(dplyr)
load("Data_Cleaning/data_nouveau.RData")

# est conteste : à re encoder en 0 / 1
# risuqe_delai à mettre en facteur
# ACCORD
data_accord = data[data$status == "encaisse",]
lapply(data_accord, unique)
data_accord = subset(data_accord, select=-c(status,motif_refus,est_frigo,est_encaisse,banque_accord,est_refuse,premier_rdv_realise))
# status : encaisse
# motif_refus : NA
# retour_bfc : "envoi et retour" et NA
# est_frigo : 0
# est_encaisse : 1
# banque_accord : 1
# est_refuse : 0
# premier_rdv_realise : 1
data_accord <- data_accord %>% distinct()
data_accord = subset(data_accord, select=-c(cle_unique_tel,cle_unique_mail))
donnees_manquantes_a <- colSums(is.na(data_accord)) / nrow(data_accord) * 100

data_accord$retour_bfc <- ifelse(is.na(data_accord$retour_bfc), 0, 1)

# FRIGO
data_frigo = data[data$status == "frigo",]
lapply(data_frigo, unique)
data_frigo = subset(data_frigo, select=-c(status,reserve_levee,etude_partagee,premier_rdv_realise,rdv_notaire,est_encaisse,est_refuse,banque_accord,est_frigo,motif_refus))
# status : frigo
# motif_refus : juste 1 motif
# apl : 1 fois : 26,53 et 290 puis NA
# scoring : FRIGO ou NA
# reserve_levee : 0
# signature_electrionique : 1 ou NA
# etude_partagee : 0
# risque_delai : 0 ou NA
# rdv_notaire : 0
# est_encaisse : 0
# est_frigo : 0 (4) et 1
# est_refuse : juste 1 fois 1 et 0
# banque_accord : 1 (6) et 0
# premier_rdv_realise : 1
data_frigo <- data_frigo %>% distinct()
data_frigo = subset(data_frigo, select=-c(cle_unique_tel,cle_unique_mail))
donnees_manquantes_f <- colSums(is.na(data_frigo)) / nrow(data_frigo) * 100

data_frigo$scoring <- ifelse(is.na(data_frigo$scoring), 0, 1)
data_frigo$signature_electronique <- ifelse(is.na(data_frigo$signature_electronique), 0, 1)
data_frigo$risque_delai <- ifelse(is.na(data_frigo$risque_delai), 0, 1)

# REFUS
data_refus <- data %>% filter(!status %in% c("encaisse", "frigo"))
lapply(data_refus, unique)
data_refus = subset(data_refus, select=-c(est_encaisse))
# est_encaisse : 0
donnees_manquantes_r <- colSums(is.na(data_refus)) / nrow(data_refus) * 100

data_refus_rdv <- data_refus[data_refus$premier_rdv_realise == "1", ]
lapply(data_refus_rdv, unique)
data_refus_rdv = subset(data_refus_rdv, select=-c(est_refuse, premier_rdv_realise))
# est_refuse : 1
# premier_rdv_realise : 1
data_refus_rdv <- data_refus_rdv %>% distinct()
data_refus_rdv = subset(data_refus_rdv, select=-c(cle_unique_tel,cle_unique_mail))
donnees_manquantes_r1 <- colSums(is.na(data_refus_rdv)) / nrow(data_refus_rdv) * 100

data_refus_sans_rdv <- data_refus[data_refus$premier_rdv_realise == "0", ]
lapply(data_refus_sans_rdv, unique)
data_refus_sans_rdv = subset(data_refus_sans_rdv, select=-c(type_premier_rendez_vous, montant,duree,type_garantie,part_immo,
                                                            taux_offre,banque,scoring,nb_epargne,montant_versement_annuel,
                                                            produit_epargne,risque_delai,nb_rdv_fait,rdv_notaire,
                                                            banque_accord,premier_rdv_realise,est_frigo,
                                                            est_conteste, retour_bfc))
# type_premier_rdv : NA
# montant : NA
# duree : NA
# type_garantie : NA
# part_immo : NA
# taux_offre : NA
# banque : NA
# scoring : NA
# reserve_levee : 0 ou NA
# signature_electronique : NA ou 1
# nb_epargne : NA
# montant_versement_annuel : NA
# produit_epargne : NA
# etude_partagee : 0 ou NA
# dossier_a_risque : 0 ou NA
# risque_delai : NA
# nb_rdv_fait : 0
# nb_rdv_pas_fait : 0 ou NA
# est_ refuse : ?
# rdv_notaire : 0
# banque_accord : 0
# premier_rdv_realise : 0
# est_frigo : 0
# est_conteste : NA
# retour_bfc : pas d'envoi
# reserve_levee / dossier_a_risque / etude_partagee : juste deux 0 et NA
# signature_electronique : juste deux 1 et NA
data_refus_sans_rdv <- data_refus_sans_rdv %>% distinct()
data_refus_sans_rdv = subset(data_refus_sans_rdv, select=-c(cle_unique_tel,cle_unique_mail))
donnees_manquantes_r2 <- colSums(is.na(data_refus_sans_rdv)) / nrow(data_refus_sans_rdv) * 100


data_refus_sans_dette$type_garantie <- ifelse(is.na(data_refus_sans_dette$type_garantie), 0, 1)
data_refus_sans_dette$part_immo <- ifelse(is.na(data_refus_sans_dette$part_immo), 0, 1)
data_refus_sans_dette$reserve_levee <- ifelse(is.na(data_refus_sans_dette$reserve_levee), 0, 1)
data_refus_sans_dette$etude_partagee <- ifelse(is.na(data_refus_sans_dette$etude_partagee), 0, 1)
data_refus_sans_dette$transfert_adp_status <- ifelse(is.na(data_refus_sans_dette$transfert_adp_status), 0, 1)


save(data_accord, file = "Data_Cleaning/data_accord.RData")
save(data_frigo, file = "Data_Cleaning/data_frigo.RData")
save(data_refus_dette, file = "Data_Cleaning/data_refus_rdv.RData")
save(data_refus_sans_dette, file = "Data_Cleaning/data_refus_sans_rdv.RData")