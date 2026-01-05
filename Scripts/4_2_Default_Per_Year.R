#==================================================================
#Objectives: 

#Generate the default variable per year for assessment
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(dplyr)
library(readxl)
library(stringr)

#Datasets
full_wrds <- readRDS("final_merge.rds")
LoPucki <- read_excel("_raw/Florida-UCLA-LoPucki Bankruptcy Research Database 1-12-2023.xlsx")

#Filter LoPucki per year
LoPucki_clean <- LoPucki %>%
  select(GvkeyBefore, YearFiled) %>%
  filter(!is.na(GvkeyBefore), !is.na(YearFiled)) %>%
  mutate(
    YearFiled = as.integer(YearFiled),
    GvkeyBefore = as.character(GvkeyBefore)
  ) %>%
  filter(YearFiled >= 1980, YearFiled <= 2010) %>%
  distinct(GvkeyBefore, YearFiled)  # una quiebra por gvkey y año

#Generating Default columns per year for K-fold CV

#Init columns
for (y in 1980:2010) {
  col_name <- paste0("default_", y)
  full_wrds[[col_name]] <- 0L
}

#1 if bankrupt in year y
for (i in 1:nrow(LoPucki_clean)) {
  gv <- LoPucki_clean$GvkeyBefore[i]
  yr <- LoPucki_clean$YearFiled[i]
  col <- paste0("default_", yr)
  
  #Only if the col exists
  if (col %in% names(full_wrds)) {
    full_wrds[full_wrds$gvkey == gv, col] <- 1L
  }
}

#Filter only 1980 to 2010 in wrds dataset
years <- stringr::str_extract(names(full_wrds), "\\d{4}")
years <- as.numeric(years)
filtered_wrds <- full_wrds[, is.na(years) | (years >= 1980 & years <= 2010)]

#Delete _NA columns
filtered_wrds <- filtered_wrds %>%
  select(-ends_with("_NA"))

#Replace - by _
filtered_wrds <- filtered_wrds %>%
  rename_with(~ str_replace_all(., "-", "_"))

#Regenerating mkval and X4

#Rebuilding mktval
price_cols <- names(filtered_wrds)[str_detect(names(filtered_wrds), "^prc_\\d{4}_\\d{2}_\\d{2}$")]
shrout_cols <- names(filtered_wrds)[str_detect(names(filtered_wrds), "^shrout_\\d{4}_\\d{2}_\\d{2}$")]
years <- unique(str_extract(price_cols, "\\d{4}"))
for (y in years) {
  price_candidates <- price_cols[str_detect(price_cols, paste0("^prc_", y, "_"))]
  shrout_candidates <- shrout_cols[str_detect(shrout_cols, paste0("^shrout_", y, "_"))]
  price_col <- sort(price_candidates)[1]
  shrout_col <- sort(shrout_candidates)[1]
  mktval_col <- paste0("mkvalt_", y)
  if (!is.na(price_col) && !is.na(shrout_col)) {
    filtered_wrds[[mktval_col]] <- abs(filtered_wrds[[price_col]]) * filtered_wrds[[shrout_col]] * 1000
  }
}

#Generating X4

years <- 1980:2010
for (y in years) {
  mkv_col <- paste0("mkvalt_", y)
  lt_col   <- paste0("lt_", y)     # Long-Term Debt
  X4_col  <- paste0("X4_", y)
  
  if (all(mkv_col %in% names(filtered_wrds))) {
    filtered_wrds[[X4_col]] <- filtered_wrds[[mkv_col]] / filtered_wrds[[lt_col]]
  }
}

#Generating AltmanZ_YYYY
years <- 1980:2010

for (y in years) {
  x1 <- paste0("X1_", y)
  x2 <- paste0("X2_", y)
  x3 <- paste0("X3_", y)
  x4 <- paste0("X4_", y)
  x5 <- paste0("X5_", y)
  z  <- paste0("AltmanZ_", y)
  if (all(c(x1, x2, x3, x4, x5) %in% names(filtered_wrds))) {
    filtered_wrds[[z]] <- 1.2 * filtered_wrds[[x1]] + 1.4 * filtered_wrds[[x2]] + 3.3 * filtered_wrds[[x3]] + 
      0.6 * filtered_wrds[[x4]] + 1.0 * filtered_wrds[[x5]]
  }
}

#Save
saveRDS(filtered_wrds, file = "filtered_wrds_with_default_per_year.rds")
