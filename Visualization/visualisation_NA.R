load("Data/data.RData")

library(visdat)
library(naniar)
library(UpSetR)
library(ggplot2)

gg_miss_fct(x = data, fct = type_dossier)
gg_miss_fct(x = data, fct = coemprunteur)



