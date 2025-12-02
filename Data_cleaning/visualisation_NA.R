load("Data_Cleaning/data_nouveau.RData")

library(visdat)
library(naniar)
library(UpSetR)

vis_miss(data, warn_large_data = FALSE)

gg_miss_var(data)

library(ggplot2)
ggplot(data = data) + aes(x = status , y = type_dossier ) + geom_miss_point()

gg_miss_fct(x = data, fct = status) 