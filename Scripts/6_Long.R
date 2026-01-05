#==================================================================
#Objectives: 

#Transform database to long
#Warning, it taked too long and it's computationally extensive

#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#libraries
library(tidyverse)
library(ggplot2)
library(purrr)
library(stringr)
library(lubridate)
#Just in case:
library(dplyr)
library(tidyr)

#Databases
filtered_wrds <- readRDS("filtered_wrds.rds")

#Step 1: pivot longer for annual variables
# ========================================
df_anual <- filtered_wrds %>%
  select(gvkey, permno, default, matches("^[^_]+_\\d{4}$")) %>%  # Solo columnas como wcap_1980
  pivot_longer(
    cols = matches("^[^_]+_\\d{4}$"),
    names_to = c("variable", "fyear"),
    names_pattern = "^(.+)_(\\d{4})$",
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) %>%
  mutate(fyear = as.integer(fyear)) %>%
  select(gvkey, permno, fyear, default, everything())

#Step 2: pivot longer for daily variables (in tibble)
# ========================================
df_diario <- filtered_wrds %>%
  select(gvkey, permno, matches("^(vol|shrout|prc|cap|ret)_\\d{4}_\\d{2}_\\d{2}$")) %>%
  pivot_longer(
    cols = matches("^(vol|shrout|prc|cap|ret)_\\d{4}_\\d{2}_\\d{2}$"),
    names_to = c(".value", "fecha"),
    names_pattern = "^(vol|shrout|prc|cap|ret)_(\\d{4}_\\d{2}_\\d{2})$",
    names_transform = list(fecha = ymd)
  ) %>%
  mutate(fyear = year(fecha)) %>%
  group_by(gvkey, permno, fyear) %>%
  summarise(
    serie_diaria = list(
      pick(fecha, vol, shrout, prc, cap, ret) %>% arrange(fecha)
    ),
    .groups = "drop"
  )

#Step 3: Join df_annual and df_daily (in tibble)
# ========================================
df_final <- df_anual %>%
  left_join(df_diario, by = c("gvkey", "permno", "fyear")) %>%
  arrange(gvkey, permno, fyear)

#Step 4: Save
# ========================================
saveRDS(df_final, "filtered_wrds_long.rds")
