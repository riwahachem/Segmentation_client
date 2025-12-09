load("Data_Cleaning/data_nouveau.RData")

library(visdat)
library(naniar)
library(UpSetR)
library(ggplot2)

gg_miss_fct(x = data, fct = type_dossier)
gg_miss_fct(x = data, fct = status)
gg_miss_fct(x = data, fct = premier_rdv_realise)

#ACCORD 

vis_miss(data_accord, warn_large_data = FALSE)
gg_miss_var(data_accord)
ggplot(data = data_accord) + aes(x = nb_pret_conso, y = type_dossier ) + geom_miss_point()
gg_miss_upset(data_accord, nsets = 18)

#FRIGO 

vis_miss(data_frigo, warn_large_data = FALSE)
gg_miss_var(data_frigo)
ggplot(data = data_frigo) + aes(x = scoring, y = type_dossier ) + geom_miss_point()
gg_miss_upset(data_frigo, nsets = 15)

#REFUS

gg_miss_var(data_refus)
gg_miss_fct(x = data_refus, fct = premier_rdv_realise)
gg_miss_fct(x = data_refus, fct = type_dossier)

#REFUS AVEC RDV

vis_miss(data_refus_rdv, warn_large_data = FALSE)
gg_miss_var(data_refus_rdv)

ggplot(data = data_refus_rdv) + aes(x = nb_epargne, y = produit_epargne ) + geom_miss_point()
ggplot(data = data_refus_rdv) + aes(x = allocation_familiale, y = apl) + geom_miss_point()
ggplot(data = data_refus_rdv) + aes(x = hebergement_gratuit, y = est_conteste) + geom_miss_point()

gg_miss_upset(data_refus_rdv, nsets = 20)

#REFUS SANS RDV

vis_miss(data_refus_sans_rdv, warn_large_data = FALSE)
gg_miss_var(data_refus_sans_rdv)

ggplot(data = data_refus_sans_rdv) + aes(x = agence, y = type_utilisateur ) + geom_miss_point()
ggplot(data = data_refus_sans_rdv) + aes(x = allocation_familiale, y = apl ) + geom_miss_point()
ggplot(data = data_refus_sans_rdv) + aes(x = type_contrat_emprunteur, y = nb_doc_espace_client ) + geom_miss_point()

gg_miss_upset(data_refus_sans_rdv, nsets = 23)
