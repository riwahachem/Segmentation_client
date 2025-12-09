library(ggplot2)
library(dplyr)
library(scales)

taches <- data.frame(
  Tache = c("Découverte du projet", "Exploration des données", "Nettoyage des données", "ACP", "Clustering", "Rédaction du rapport"),
  Debut = as.Date(c("2025-11-18","2025-11-24", "2025-11-25", "2025-12-05", "2025-12-06", "2025-11-25")),
  Fin = as.Date(c("2025-11-24","2025-11-26", "2025-12-05", "2025-12-07", "2025-12-12", "2025-12-20"))
)

taches <- taches %>% mutate(Duree = Fin - Debut)

reunion <- data.frame(
  Evenement = c("Réunion", "Réunion", "Réunion", "Réunion", "Réunion"),
  Date = as.Date(c("2025-11-18", "2025-11-25", "2025-12-02", "2025-12-09", "2025-12-16"))
)

y_levels <- c("Réunions de groupe", rev(taches$Tache))
taches$Tache <- factor(taches$Tache, levels = rev(taches$Tache))
reunion$Tache <- factor("Réunions de groupe", levels = y_levels)

all_dates <- seq(min(taches$Debut), max(taches$Fin), by = "7 days")

ggplot() +
  geom_segment(data = taches, 
               aes(x = Debut, xend = Fin, y = Tache, yend = Tache),
               size = 8, color = "steelblue") +
  
  geom_point(data = reunion, aes(x = Date, y = Tache),
             size = 3, color = "red") +
  
  geom_vline(xintercept = all_dates, color = "grey40", linetype = 3) +
  
  scale_x_date(
    breaks = reunion$Date,          
    date_labels = "%d-%m"
  ) +
  
  scale_y_discrete(limits = y_levels) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ) +
  
  labs(
    title = "Diagramme de Gantt du projet",
    x = "",
    y = ""
  )
