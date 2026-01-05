#==================================================================
#Objectives: 

#Clean CRSP and Compustat
#Generate a first version of both datasets that contains only relevant companies

#==================================================================

#Libraries
#=============================================

library(tidyverse)
library(scales)
library(readxl)
library(dplyr)
library(tidyfinance)
library(dbplyr)

#Working directory and global variables
#=============================================
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Importing data
#=============================================
link <- readRDS("ccmxpf_linktable.rds")
crsp <- readRDS("_raw/crsp_daily_full.rds")
compustat_fundational_var <- readRDS("_raw/compustat_funda_variables.rds")
compustat_company_var <- readRDS("compustat_company_variables.rds")
compustat <- readRDS("_raw/compustat_all.rds")
company <- readRDS("_raw/company_all.rds")
LoPucki <- read_excel("_raw/Florida-UCLA-LoPucki Bankruptcy Research Database 1-12-2023.xlsx")

#First cleaning
#=============================================

#CRSP
#Tresholds:
#10% for variables to include the 6 main ones if 5% then vol is not included
#1/6% for observations, we allow only one missing variable for each observation

crsp_cleaned <- crsp %>%
  select(date, where(is.numeric)) %>%
  select(where(~ mean(is.na(.x)) < 0.10) | all_of("date")) %>%
  filter(rowMeans(is.na(select(., -date))) < 1/6)
saveRDS(crsp_cleaned, file = "crsp_cleaned.rds")

#Compustat
vars_keep <- c(
  "gvkey", "datadate", "fyear",
  # Altman
  "wcap", "re", "ebit", "sale", "at", "mkvalt", "dlc", "dltt",
  # Default flag
  "dlrsn",
  # Complementarias
  "ni", "oancf", "che", "act", "lct", "xint", "lt",
  "ceq", "ipodate"
)

#Key variables
vars_keep <- c(
  #Important variables
  "gvkey", "datadate", "fyear",
  #Altman
  "wcap", "re", "ebit", "sale", "at", "mkvalt", "dlc", "dltt",
  #Others
  "ni", "oancf", "che", "act", "lct", "xint", "lt",
  "ceq"
)

#Building variables
default_model_data <- compustat %>%
  select(any_of(vars_keep)) %>%
  mutate(
    #Altman ratios
    X1 = wcap / at,                    
    X2 = re / at,                      
    X3 = ebit / at,                    
    #We do X4 = mkvalt / lt later because we have missing data for this one
    X5 = sale / at,                    
    #AltmanZ = 1.2*X1 + 1.4*X2 + 3.3*X3 + 0.6*X4 + 1.0*X5,
    
    #Other useful variables
    log_assets = log(at),
    leverage = lt / at,
    long_term_debt = dltt / at,
    liquidity = act / lct,
    cash_to_assets = che / at,
    profitability_net = ni / at,
    profitability_oper = ebit / at,
    cash_flow = oancf / at,
    interest_cover = ebit / xint,
    book_to_market = ceq / mkvalt
  )
compustat_cleaned <- default_model_data %>%
  select(all_of(vars_keep), where(is.numeric))
saveRDS(compustat_cleaned, file = "compustat_cleaned.rds")

#To get gvkey and permno in Y in both datasets (to clean)
#=============================================

default_train <- LoPucki$GvkeyBefore #Get gvkey in Y
compustat_cleaned_only <- compustat_cleaned %>% filter(gvkey %in% default_train) #Get gvkey that is Y in Compustat -- only 1060 codes
saveRDS(compustat_cleaned_only, file = "compustat_cleaned_only.rds")

link_only <- link %>% filter(gvkey %in% default_train) #Get gvkey that is Y in Link -- only 962
default_train_permno <- unique(link_only$permno) #Get permno that is Y in Link -- only 1209

crsp_cleaned_only <- crsp_cleaned %>% filter(permno %in% default_train_permno) #Get permno that is Y in CRSP -- only 1120
saveRDS(crsp_cleaned_only, file = "crsp_cleaned_only.rds")