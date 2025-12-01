## Questions

# Concernant les NA dans les dettes, est ce que NA signifie 0 dette ou c'est juste que la personne n'a rien renseigné ?
# Concernant les variables montant, apl, taux_offre etc, le fait d'imputer une modalité inconnu, transforme les variables quanti en quali.
# Comment découper ces variables en tranches pour créer des classes logiques ? Ou doit on trouver un moyen de les garder en quanti ?

library(dplyr)
library(FactoMineR)
library(factoextra)
library(missMDA)

load("data_nouveau.RData")
# Préparation
data_prop <- data %>% 
  filter(type_dossier == "proprietaire")

# Variables actives
vars_actives <- c(
  "age_emprunteur", "situation_familliale_emprunteur",
  "type_contrat_emprunteur", "type_contrat_coemprunteur",
  "revenus", "allocation_familiale", "apl", "revenu_foncier",
  "montant", "duree", "dette_famille_ami", "dette_decouvert",
  "nombre_rejets", "nombre_commissions_intervention",
  "signature_electronique", "type_support", "nb_doc_espace_client", 
  "nb_rdv_fait", "nb_rdv_pas_fait", "nb_epargne", 
  "nb_pret_conso", "nb_pret_immo", "taux_offre", "taux_ev", "est_aggregation"
)

# Variables supplémentaires
vars_supp <- c("est_refuse", "status", "banque", "motif_refus", "retour_bfc")

df_actives <- data_prop %>% 
  select(any_of(vars_actives)) %>% 
  mutate_if(is.character, as.factor)

df_supp <- data_prop %>% 
  select(any_of(vars_supp)) %>% 
  mutate_if(is.character, as.factor)

# Imputation
res.impute <- imputeFAMD(df_actives, ncp = 2)

# Reconstruction du jeu de données
df_final <- cbind(res.impute$completeObs, df_supp)

# Les variables supplémentaires sont à la fin du tableau (plus simple a gerer)
index_sup <- (ncol(res.impute$completeObs) + 1):ncol(df_final)

# Analyse factorielle
res.famd <- FAMD(df_final, 
                 sup.var = index_sup, 
                 graph = FALSE)

# Clustering
res.hcpc <- HCPC(res.famd, nb.clust = -1, graph = FALSE)

# Affichage
fviz_cluster(res.hcpc, geom = "point", main = "Clustering")

# Caractérisation
print(res.hcpc$desc.var$category)
