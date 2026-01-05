#==================================================================
#Objectives: 

#Redoing the assessment on the baseline models with the data in long
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(dplyr)
library(readxl)
library(stringr)

#Database - we use this one because it's enough for the baseline models
filtered_wrds_long <- readRDS("filtered_wrds_long.rds")
LoPucki <- read_excel("_raw/Florida-UCLA-LoPucki Bankruptcy Research Database 1-12-2023.xlsx")

#Cleaning LoPucki
#========================================
LoPucki_clean <- LoPucki %>%
  select(GvkeyBefore, YearFiled) %>%
  filter(!is.na(GvkeyBefore), !is.na(YearFiled)) %>%
  mutate(
    YearFiled = as.integer(YearFiled),
    GvkeyBefore = as.character(GvkeyBefore)
  ) %>%
  filter(YearFiled >= 1980, YearFiled <= 2010) %>%
  distinct(GvkeyBefore, YearFiled) %>%
  rename(gvkey = GvkeyBefore, default_year_file = YearFiled) %>%
  group_by(gvkey) %>%
  arrange(default_year_file) %>%
  slice(1) %>%
  ungroup()

#Generate default per year
#========================================
data_long <- data_long %>%
  mutate(
    gvkey = as.character(gvkey),
    fyear = as.integer(fyear),
    default_year = 0L
  ) %>%
  left_join(
    LoPucki_clean %>% select(gvkey, default_year_file),
    by = "gvkey"
  ) %>%
  mutate(
    default_year = ifelse(fyear == default_year_file, 1L, default_year)
  ) %>%
  select(-default_year_file)

saveRDS(data_long, "filtered_wrds_long_assessment.rds")
