library(dplyr)
library(ggplot2)
load("Data_Cleaning/data_de_base.RData")
load("Data_Cleaning/data.RData")

#Statistiques descriptives (variables qualitatives) : 

# Proportions de non-relancés : 
prop.table(table(data_de_base$a_ete_relance)) * 100

ggplot(data_de_base, aes(x = a_ete_relance)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution de la variable a_ete_relance",
       x = "A été relancé",
       y = "Nombre d'observations")

ggplot(data_de_base, aes(x = status, fill = a_ete_relance)) +
  geom_bar(position = "dodge") +
  labs(title = "Status selon a_ete_relance",
       x = "Status",
       y = "Nombre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_de_base, aes(x = type_dossier, fill = a_ete_relance)) +
  geom_bar(position = "dodge") +
  labs(title = "Type de dossier selon a_ete_relance",
       x = "Type de dossier",
       y = "Nombre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_de_base, aes(x = pret_conso_conserve, fill = a_ete_relance)) +
  geom_bar(position = "dodge") +
  labs(title = "Conservation du prêt à la consommation selon a_ete_relance",
       x = "Conservation du prêt",
       y = "Nombre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_de_base, aes(x = situation_familliale_emprunteur, fill = a_ete_relance)) +
  geom_bar(position = "dodge") +
  labs(title = "Situation familiale de l'emprunteur selon a_ete_relance",
       x = "Situation familiale",
       y = "Nombre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Statistiques descriptives (variables quantitatives) :

ggplot(data_de_base, aes(x = a_ete_relance, y = montant)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution du montant à financer selon a_ete_relance",
       x = "A été relancé",
       y = "Montant à financer")
ggplot(data_de_base, aes(x = montant, fill = a_ete_relance)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(title = "Histogramme du montant à financer")

ggplot(data_de_base, aes(x = a_ete_relance, y = nb_pret_conso)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution du nombre de prêt à la consommation selon a_ete_relance",
       x = "A été relancé",
       y = "Nombre et prêt à la consommation")

ggplot(data_de_base, aes(x = a_ete_relance, y = nombre_rejets)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution du nombre de rejets selon a_ete_relance",
       x = "A été relancé",
       y = "Nombre de rejets")

#Analyse decriptive sur la table data (après suppression des clients relancés)

ggplot(data, aes(x = pret_conso_conserve, fill = type_dossier)) +
  geom_bar(position = "dodge") +
  labs(title = "Conservation des prêts à la consommation selon le type de dossier",
       x = "Status",
       y = "Conservation des prêts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Statistiques descriptives à afficher dans le rapport

ggplot(data, aes(x = type_dossier, fill = dette)) +
  geom_bar(position = "fill") +
  labs(x = "Type de dossier",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = type_dossier, fill = premier_rdv_realise)) +
  geom_bar(position = "fill") +
  labs(
       x = "Type de dossier",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = premier_rdv_realise, fill = status)) +
  geom_bar(position = "fill") +
  labs(
       x = "Type de dossier",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
