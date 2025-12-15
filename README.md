# Segmentation client

---

## Objectifs
Ce projet vise à réaliser une segmentation de clients à partir de données mixtes, dans un contexte métier.  

Les principales étapes :  

- Nettoyer et préparer les données,  
- Traiter les valeurs manquantes,  
- Identifier des groupes de clients homogènes à l’aide de méthodes de clustering,  
- Décrire les clusters obtenus.  

---

## Structure du projet

```text
Segmentation_client/  
├── data/  
│   ├── new_data  
│   └── data_to_umontpellier_new1  
├── preprocessing/  
│   ├── data_cleaning.R  
│   ├── processing.R  
│   ├── stratification.R  
│   └── missing_values.R  
├── clustering/  
│   ├── clustering_CAH.R  
│   ├── clustering_Clara.R  
│   ├── clustering_kmeans_FAMD.R  
│   ├── clustering_kmeans_RF.R  
│   ├── clustering_PAM.R  
│   └── profiling.R  
├── visualization/  
│   ├── visualization_missing_values.R  
│   ├── visualization_clusters.R  
│   └── descriptive_stats.R  
├── .gitignore  
└── README.md  
```
---

## Mode d'emploi

### 1. Préparation et nettoyage des données  
Dossier : `preprocessing/`

Exécuter les scripts dans l’ordre suivant :  
1. `data_cleaning.R`  
2. `processing.R`  
3. `stratification.R`  
4. `missing_values.R`  


### 2. Clustering 
Dossier : `clustering/`

Les méthodes de segmentation peuvent être exécutées indépendamment :  

- `clustering_CAH.R`  
- `clustering_PAM.R`  
- `clustering_Clara.R`    
- `clustering_kmeans_FAMD.R`  
- `clustering_kmeans_RF.R`  

Le script `profiling.R` permet de décrire et interpréter les clusters obtenus.


### 3. Visualisation et analyse descriptive  
Dossier : `visualization/`

Les fichiers peuvent être exécutées indépendamment :

- `visualization_missing_values.R`  
  Visualisation des données manquantes.
- `descriptive_stats.R`  
  Statistiques descriptives globales et par cluster.
- `visualization_clusters.R`  
  Visualisation des caractéristiques des clusters.
  
---

## Auteurs

- **Wahel EL-MAZZOUJI** - [wahel.el-mazzouji@etu.umontpellier.fr](mailto:wahel.el-mazzouji@etu.umontpellier.fr)
- **Marine GERMAIN** – [marine.germain@etu.umontpellier.fr](mailto:marine.germain@etu.umontpellier.fr)  
- **Riwa HACHEM REDA** - [riwa.hachem-reda@etu.umontpellier.fr](mailto:riwa.hachem-reda@etu.umontpellier.fr)  
- **Samy M'RAD** -[samy.mrad@etu.umontpellier.fr](mailto:samy.mrad@etu.umontpellier.fr)
- **Coralie ROMANI DE VINCI** – [coralie.romani-de-vinci@etu.umontpellier.fr](coralie.romani-de-vinci@etu.umontpellier.fr)

Ce projet a été réalisé dans le cadre de notre Master 2 Statistiques pour l'Information et l'Aide à la Décision, sous l’encadrement de Credo Vovor-Dassu (Premista) et de Maximilien Dossa (IAE).

---