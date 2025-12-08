library(dplyr)
library(tidyr)
library(factoextra)
library(ggplot2)
library(dbscan)
library(FactoMineR)
library(e1071)

# Charger les données
load("Data_Cleaning/data_finale.RData")

#Verif des NA 
colSums(is.na(data_impute))


# One-hot encoding
data_encoded <- model.matrix(~ . - 1, data = data_impute) %>% 
  as.data.frame()

# Standardisation
data_scaled <- scale(data_encoded)


# Taille totale
n <- nrow(data_scaled)

set.seed(123)

# Indices de l'échantillon 
idx <- sample(seq_len(n), size = 10000)

# Sous-échantillon
data_scaled_small <- data_scaled[idx, , drop = FALSE]
data_impute_small <- data_impute[idx, , drop = FALSE]  # optionnel, pour analyse derrière

################################################################################
# ACP 
################################################################################

res_pca_small <- PCA(
  data_scaled_small,
  scale.unit = FALSE,   # déjà standardisé avec scale()
  ncp        = 20,      # 20 composantes suffisent pour SVM + visu
  graph      = FALSE
)

# Coordonnées des individus dans l'espace PCA
scores_pca_small <- res_pca_small$ind$coord  # toutes les composantes calculées
dim(scores_pca_small)  # 10000 x 20 normalement

################################################################################
## 3) One-Class SVM non supervisé sur l'échantillon
################################################################################

set.seed(322)

svm_model_small <- svm(
  x      = scores_pca_small,
  type   = "one-classification",
  kernel = "radial",
  nu     = 0.05,
  gamma  = 1 / ncol(scores_pca_small)    # règle : 1 / dimension
)

################################################################################
## 4) Récupérer inliers / outliers + scores SVM
################################################################################

svm_pred_small <- predict(
  svm_model_small,
  scores_pca_small,
  decision.values = TRUE
)

# Valeurs de décision (score SVM)
svm_scores_small <- attr(svm_pred_small, "decision.values")

# Dataframe pour visualisation
df_vis_small <- data.frame(
  PC1    = scores_pca_small[, 1],
  PC2    = scores_pca_small[, 2],
  inlier = as.factor(svm_pred_small),        # TRUE = inlier, FALSE = outlier
  score  = as.numeric(svm_scores_small)
)

table(df_vis_small$inlier)  # pour voir combien d'inliers / outliers
################################################################################
## 5) Visualisation : inliers vs outliers dans le plan PCA
################################################################################

ggplot(df_vis_small, aes(x = PC1, y = PC2, color = inlier)) +
  geom_point(alpha = 0.6, size = 0.8) +
  coord_equal() +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "grey60")) +
  labs(
    title = "One-Class SVM (échantillon 10 000) : inliers vs outliers",
    color = "Statut SVM"
  ) +
  theme_minimal()

################################################################################
## 6) (Optionnel) Visualisation continue des scores SVM
################################################################################

ggplot(df_vis_small, aes(x = PC1, y = PC2, color = score)) +
  geom_point(alpha = 0.7, size = 0.8) +
  coord_equal() +
  labs(
    title = "Score de décision One-Class SVM sur l'échantillon",
    color = "Decision value"
  ) +
  theme_minimal()





























# Indices échantillon
idx <- sample(seq_len(n), size = 10000)

# Sous-échantillon
data_scaled_small <- data_scaled[idx, , drop = FALSE]
data_impute_small <- data_impute[idx, , drop = FALSE]


################################################################################
##  SVM ##
################################################################################

#ACP
res_pca <- PCA(
  data_scaled,
  scale.unit = FALSE,  # déjà standardisé
  ncp        = 62,     # nombre de composantes que tu veux
  graph      = FALSE
)

# Valeurs propres (variance expliquée)
eigen <- res_pca$eig
eigen

# Coordonnées des individus sur les 62 composantes
scores_pca <- res_pca$ind$coord[, 1:62]
dim(scores_pca)


set.seed(322)

# Gamma auto
#D <- dist(scores_pca)
#gamma_auto <- 1 / median(D)^2
#gamma_auto


svm_model <- svm(
  x = scores_pca,
  type = "one-classification",
  kernel = "radial",
  nu = 0.05,        # proportion d’outliers, ajustable
  gamma = 1/62      # règle : 1 / nb_dims
)


svm_scores <- attr(
  predict(svm_model, scores_pca, decision.values = TRUE),
  "decision.values"
)
svm_scores



# Prédictions SVM (TRUE = inlier, FALSE = outlier)
svm_pred <- predict(
  svm_model,
  scores_pca,
  decision.values = TRUE
)

# Valeurs de décision (score SVM)
svm_scores <- attr(svm_pred, "decision.values")

# Dataframe pour visualisation
df_vis <- data.frame(
  PC1    = scores_pca[, 1],
  PC2    = scores_pca[, 2],
  inlier = as.factor(svm_pred),            # "TRUE" / "FALSE"
  score  = as.numeric(svm_scores)
)

table(df_vis$inlier)





#Cluster
idx <- sample(seq_along(svm_scores), size = 3000)  #échantillion

db_small <- hdbscan(as.matrix(svm_scores[idx]), minPts = 100)
clusters_small <- db_small$cluster
table(clusters_small)



# Visualisation DES CLUSTERS EN acp


fviz_cluster(
  list(
    data    = scores_pca[idx, 1:2],  # seulement les 3000 points
    cluster = clusters_small
  ),
  geom = "point",
  repel = TRUE,
  main = "Support Vector Clustering (SVM + HDBSCAN) - Échantillon (PCA 2D)"
)



#########KMeans 


set.seed(1)

km <- kmeans(as.matrix(svm_scores), centers = 4, nstart = 20)
clusters_svm <- km$cluster
table(clusters_svm)

# Visualisation sur TOUT le nuage en PCA 2D
fviz_cluster(
  list(
    data    = scores_pca[, 1:2],
    cluster = clusters_svm
  ),
  geom = "point",
  repel = TRUE,
  main = "Clustering basé sur les scores One-Class SVM (k-means) - PCA 2D"
)






################################################################################
################################################################################
### K PROTOTYPE ###
################################################################################
################################################################################
library(clustMixType)

## Séparation des var quali et quanti
num_vars <- names(which(sapply(data_impute, is.numeric)))
cat_vars <- setdiff(names(data_impute), num_vars)

# Verif/Conversion explicite
data_impute[num_vars] <- lapply(data_impute[num_vars], as.numeric)
data_impute[cat_vars] <- lapply(data_impute[cat_vars], factor)
str(data_impute)

# Standardisation
data_scaled <- scale(data_encoded)





# K poroto
set.seed(123)

res_kp <- kproto(
  x       = data_impute,
  k       = 3,      # nombre de clusters à tester
  lambda  = NULL    # laisse l’algorithme choisir la pondération num vs cat
)

res_kp$lambda        # valeur utilisée pour pondérer les variables catégorielles
res_kp$size          # taille de chaque cluster
res_kp$withinss      # "inertie" intra-cluster par cluster
sum(res_kp$withinss) # inertie totale, utile pour comparer différents k

# ajout du cluster dans le df
data_impute$cluster_kp <- factor(res_kp$cluster)





# CHoix des k groupes
ks <- 1:6
tot_withinss <- numeric(length(ks))
for (i in seq_along(ks)) {
  k <- ks[i]
  cat("k =", k, "\n")
  tmp <- kproto(data_impute, k = k, lambda = NULL)
  # on stocke la somme des withinss pour ce k
  tot_withinss[i] <- sum(tmp$withinss)
}


plot(ks, tot_withinss, type = "b",
     xlab = "Nombre de clusters k",
     ylab = "Inertie intra-cluster totale",
     main = "Choix de k pour k-prototypes")
 # k =3 





################################################################################
### VISUALISATION DES 3 CLUSTERS (k-prototypes) VIA FAMD
################################################################################
library(FactoMineR)
library(factoextra)

set.seed(123)

# 1. FAMD sur data_impute (sans la variable cluster)
res_famd <- FAMD(data_impute[, setdiff(colnames(data_impute), "cluster_kp")],
                 ncp = 5,           # nombre de dimensions à garder
                 graph = FALSE)

# 2. Coordonnées FAMD des individus
coord_famd <- res_famd$ind$coord

# 3. Visualisation des clusters en 2D
fviz_cluster(
  list(
    data    = coord_famd[, 1:2],          # projection 2D
    cluster = data_impute$cluster_kp      # tes groupes k-prototypes
  ),
  geom = "point",
  alpha = 0.35,                           # transparence pour éviter le bruit
  palette = "jco",                        # joli jeu de couleurs
  repel = TRUE,
  main = "Visualisation des 3 clusters k-prototypes (FAMD)"
)


