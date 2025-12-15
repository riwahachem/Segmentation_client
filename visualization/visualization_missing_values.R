library(visdat)
library(naniar)
library(UpSetR)
library(ggplot2)

load("data/data_loc_avec_NA.RData")
load("data/data_loc_sans_NA.RData")
load("data/data_prop_avec_NA.RData")
load("data/data_prop_sans_NA.RData")

data_loc_avec_na <- data_loc_avec %>% 
  select(where(~ any(is.na(.))))

data_loc_sans_na <- data_loc_sans %>% 
  select(where(~ any(is.na(.))))

data_prop_avec_na <- data_prop_avec %>% 
  select(where(~ any(is.na(.))))

data_prop_sans_na <- data_prop_sans %>% 
  select(where(~ any(is.na(.))))

gg_miss_var(x = data_loc_avec_na, show_pct = TRUE)
gg_miss_var(x = data_loc_sans_na, show_pct = TRUE)
gg_miss_var(x = data_prop_avec_na, show_pct = TRUE)
gg_miss_var(x = data_prop_sans_na, show_pct = TRUE)
