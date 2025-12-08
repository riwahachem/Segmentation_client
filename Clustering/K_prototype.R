################################################################################
## 0) Packages
################################################################################


library(tibble)
library(dplyr)
library(tidyr)
library(factoextra)
library(ggplot2)
library(dbscan)
library(FactoMineR)
library(e1071)
library(clustMixType)

################################################################################
# Chargement des données + vérification des NA
################################################################################

load("Data_Cleaning/data_finale.RData")

# Vérif des NA
colSums(is.na(data_impute))

colnames(data_impute)

## One-hot encoding
################################################################################

data_encoded <- model.matrix(~ . - 1, data = data_impute) %>%
  as.data.frame()

# Centrage-réduction de chaque colonne
data_scaled <- scale(data_encoded)


## Échantillonnage aléatoire de X individus
################################################################################
set.seed(123)  # pour reproductibilité

n <- nrow(data_scaled)

# Indices de l'échantillon
idx <- sample(seq_len(n), size = 20000)

# Sous-échantillon 
data_scaled_small <- data_scaled[idx, , drop = FALSE]
data_impute_small <- data_impute[idx, , drop = FALSE] 




################################################################################
# Méthode SVM one-class + k-means
################################################################################





## ACP 
################################################################################

# On réduit la dimension : projection dans un espace de 20 composantes principales
res_pca <- PCA(
  data_scaled_small,
  scale.unit = FALSE,   # déjà C-R
  ncp        = 47,      # nb composantes
  graph      = FALSE
)


# Valeurs propres 
eigen <- res_pca$eig
eigen

# Coordonnées des individus dans l'espace des composantes principales
scores_pca <- res_pca$ind$coord  # matrice 10000 x nb comp

## One-Class SVM non supervisé 
################################################################################

# Objectif : apprendre la "forme" du nuage de points et détecter les outliers

set.seed(322)

svm_model <- svm(
  x      = scores_pca,
  type   = "one-classification",
  kernel = "radial",
  nu     = 0.05,                        # 5% d'outliers autorisés
  gamma  = 1 / ncol(scores_pca)   # règle : gamma = 1 / dimension
)

## Récupérer inliers / outliers + scores SVM
################################################################################

# Prédictions : TRUE = inlier, FALSE = outlier
svm_pred <- predict(
  svm_model,
  scores_pca,
  decision.values = TRUE
)

# Valeurs de décision (decision values = "profondeur" dans la région SVM)
svm_scores<- attr(svm_pred, "decision.values")

# Dataframe pour la visualisation
df_visl <- data.frame(
  PC1    = scores_pca[, 1],
  PC2    = scores_pca[, 2],
  inlier = as.factor(svm_pred),        # "TRUE" / "FALSE"
  score  = as.numeric(svm_scores)
)

# Nombre d'inliers vs outliers
table(df_visl$inlier)

## Visualisation : inliers vs outliers dans le plan PCA
################################################################################

ggplot(df_visl, aes(x = PC1, y = PC2, color = inlier)) +
  geom_point(alpha = 0.6, size = 0.8) +
  coord_equal() +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "grey60")) +
  labs(
    title = "One-Class SVM (échantillon 10 000) : inliers vs outliers",
    color = "Statut SVM"
  ) +
  theme_minimal()


## (Optionnel) Visualisation continue du score SVM
################################################################################

ggplot(df_visl, aes(x = PC1, y = PC2, color = score)) +
  geom_point(alpha = 0.7, size = 0.8) +
  coord_equal() +
  labs(
    title = "Score de décision One-Class SVM sur l'échantillon",
    color = "Decision value"
  ) +
  theme_minimal()


##  Clustering sur les inliers (k-means dans l'espace PCA)
################################################################################

# Idée : 
# - On garde uniquement les inliers (profils "normaux" selon SVM)
# - On applique k-means sur leurs coordonnées PCA
# - On obtient k segments de clients "cohérents"

# Indices des inliers
idx_inliers <- df_vis$inlier == "TRUE"

scores_inliers <- scores_pca[idx_inliers, , drop = FALSE]

set.seed(456)

# Choix du nombre de clusters
k <- 3
km_inliers <- kmeans(scores_inliers, centers = k, nstart = 25)

# On crée un vecteur de clusters de taille 10 000 (NA pour les outliers)
clusters_all <- rep(NA_integer_, nrow(scores_pca))
clusters_all[idx_inliers] <- km_inliers$cluster

# On ajoute l'info cluster dans le df de visualisation
df_vis$cluster <- factor(clusters_all)

table(df_vis$inlier, df_vis$cluster)


##  Visualisation des clusters (sur les inliers) dans le plan PCA
################################################################################

# On met les outliers en gris, et on colorie uniquement les inliers par cluster
ggplot(df_vis, aes(x = PC1, y = PC2)) +
  # Outliers en arrière-plan
  geom_point(
    data = subset(df_vis, inlier == "FALSE"),
    color = "black", alpha = 0.4, size = 0.6
  ) +
  # Inliers colorés par cluster
  geom_point(
    data = subset(df_vis, inlier == "TRUE"),
    aes(color = cluster),
    alpha = 0.7, size = 0.8
  ) +
  coord_equal() +
  labs(
    title = "Clustering des inliers (k-means sur PCA) après One-Class SVM",
    color = "Cluster (inliers)"
  ) +
  theme_minimal()





################################################################################
# K - prototype
################################################################################


# Séparer variables num et catégorielles
num_vars <- names(which(sapply(data_impute, is.numeric)))
cat_vars <- setdiff(names(data_impute), num_vars)
data_num   <- data_impute[, num_vars]
data_vars <- data_impute[, cat_vars]


str(data_impute) #verif des var num et factor
data_scaled <- scale(data_num)
data_mix <- cbind(data_scaled, data_vars)

set.seed(123)

# Choix du nombre de clusters k
################################################################################
ks <- 2:10
tot_withinss <- numeric(length(ks))

for (i in seq_along(ks)) {
  k <- ks[i]
  cat("k =", k, "\n")
  
  tmp <- kproto(data_mix, k = k, lambda = NULL)
  tot_withinss[i] <- sum(tmp$withinss)
}

plot(ks, tot_withinss, type = "b",
     xlab = "Nombre de clusters k",
     ylab = "Inertie totale (withinss)",
     main = "Choix du nombre de clusters (k-prototypes)")


# K-Protypes
################################################################################
k <- 5

res_kp <- kproto(
  data_mix,
  k = k,
  lambda = NULL   # pondération automatique
)

res_kp$lambda #ponderation 
res_kp$size #tailles
res_kp$centers #centre et var quali les plus fréqquentes


data_mix$cluster <- factor(res_kp$cluster)




################################################################################

# Encodage binaire pour ACP
data_encoded <- model.matrix(~ . - 1, data = data_mix) %>% 
  as.data.frame()

# ACP
res_pca <- PCA(data_encoded, scale.unit = TRUE, ncp = 2, graph = FALSE)

# Visualisation
fviz_pca_ind(
  res_pca,
  habillage  = data_mix$cluster,  # couleur = cluster k-prototypes
  geom       = "point",
  addEllipses = TRUE,
  title      = "Clusters K-prototypes projetés sur les 2 premières composantes PCA"
)



profil <- data_impute %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    montant_moyen = mean(montant, na.rm = TRUE),
    dette_moyenne = mean(as.numeric(dette), na.rm = TRUE),
    taux_refus = mean(est_refuse == 1),
    taux_accord = mean(banque_accord == 1),
    taux_encaisse = mean(est_encaisse == 1),
    taux_risque = mean(dossier_a_risque == 1),
    taux_frigo = mean(est_frigo == 1),
    age_moyen = mean(age_emprunteur, na.rm = TRUE)
  )

profil






#Visualisation inter clusters
################################################################################

centers_raw <- as.data.frame(res_kp$centers)

# Identifier num vs quali AVANT conversion
num_vars_centers <- names(which(sapply(centers_raw, is.numeric)))
cat_vars_centers <- setdiff(names(centers_raw), num_vars_centers)

types_df <- tibble(
  variable = colnames(centers_raw),
  type     = if_else(variable %in% num_vars_centers,
                     "numerique", "qualitative")
)

# Tout convertir en character pour pivot_longer
centers_char <- centers_raw %>%
  mutate(across(everything(), as.character))

# Long -> Wide : 1 ligne = 1 variable, colonnes = clusters
centers_long <- centers_char %>%
  rownames_to_column("cluster") %>%
  mutate(cluster = paste0("cluster_", cluster)) %>%  # cluster_1,...,cluster_5
  pivot_longer(
    cols      = -cluster,
    names_to  = "variable",
    values_to = "valeur"
  )

centers_resume <- centers_long %>%
  left_join(types_df, by = "variable") %>%          # ajoute la colonne "type"
  pivot_wider(
    names_from  = cluster,
    values_from = valeur
  ) %>%
  select(variable, type, starts_with("cluster_")) %>%
  arrange(type, variable)

# Aperçu
head(centers_resume)
centers_resume


