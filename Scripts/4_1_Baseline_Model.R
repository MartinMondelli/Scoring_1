#==================================================================
#Objectives: 

#Rebuilding X4 and Zscore (Altman_Z)
#Take the first baseline model using Altman

#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#libraries
library(tidyverse)
library(stargazer)

#Databases
filtered_wrds <- readRDS("filtered_wrds.rds")

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
  
  if (all(c(mkv_col, lt_col) %in% names(filtered_wrds))) {
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
  
  #Generate the Z-score only if the ratios exist
  if (all(c(x1, x2, x3, x4, x5) %in% names(filtered_wrds))) {
    filtered_wrds[[z]] <- 1.2 * filtered_wrds[[x1]] + 1.4 * filtered_wrds[[x2]] + 3.3 * filtered_wrds[[x3]] + 
      0.6 * filtered_wrds[[x4]] + 1.0 * filtered_wrds[[x5]]
  }
}
saveRDS(filtered_wrds, "filtered_wrds.rds")

#Checking altman ratios calculated in 0_process
filtered_wrds %>% select(starts_with("AltmanZ_")) %>% head()

#Take mean over the last 3 years of Altman to avoid correlation
filtered_wrds <- filtered_wrds %>%
  rowwise() %>%
  mutate(AltmanZ_mean = mean(c_across(AltmanZ_2008:AltmanZ_2010), na.rm = TRUE)) %>%
  ungroup()

#With Z-Score
#Baseline model with mean
baseline_model_mean <- glm(default ~ AltmanZ_mean, 
                      family = binomial(link = "logit"),
                      data = filtered_wrds)

summary(baseline_model_mean) #Significative at 1%
stargazer(baseline_model_mean, type = "latex", title = "Result Baseline model (wide)")

baseline_model_five <- glm(default ~ AltmanZ_2006+ AltmanZ_2007+ AltmanZ_2008+ AltmanZ_2009+ AltmanZ_2010, 
                      family = binomial(link = "logit"),
                      data = filtered_wrds)

summary(baseline_model_five) #2006 significative at 0.1%, 2007 at 5%, 2010 at 5%
stargazer(baseline_model_five, type = "latex", title = "Result Baseline model using five Z scores (wide)")

#With mean X1 + X2 + X3 + X4 + X5

#Generating the means
df <- filtered_wrds %>%
  select(gvkey,
         starts_with("X1_"),
         starts_with("X2_"),
         starts_with("X3_"),
         starts_with("X4_"),
         starts_with("X5_"),
         default) %>%
  rowwise() %>%
  mutate(
    X1_mean = mean(c_across(X1_1980:X1_2010), na.rm = TRUE),
    X2_mean = mean(c_across(X2_1980:X2_2010), na.rm = TRUE),
    X3_mean = mean(c_across(X3_1980:X3_2010), na.rm = TRUE),
    X4_mean = mean(c_across(X4_1980:X4_2010), na.rm = TRUE),
    X5_mean = mean(c_across(X5_1980:X5_2010), na.rm = TRUE)
  ) %>%
  ungroup() %>% select(X1_mean, X2_mean, X3_mean, X4_mean ,X5_mean, default)

df_clean <- df %>%
  filter(if_all(X1_mean:X5_mean, ~ is.finite(.)))
df_clean <- df_clean %>%
  mutate(default = factor(default, levels = c(0,1)))

baseline_model_real_mean <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + X5_mean, 
                           family = binomial(link = "logit"),
                           data = df_clean)
summary(baseline_model_real_mean)
stargazer(baseline_model_real_mean, type = "latex", title = "Result Baseline model (wide)")

#X4 and X5 significative at less than 0.001
