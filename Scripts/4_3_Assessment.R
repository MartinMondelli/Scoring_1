#==================================================================
#Objectives: 

#Use walk forward or time series split approach to test oour data
#We will predict using the years before the test year
#Example: 
#Test for 1981, train with 1980
#Test for 1982, train with 1980 and 1981
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(dplyr)
library(readxl)
library(stringr)
library(tidyverse)
library(ggplot2)

#Dataset
filtered_wrds_with_default_per_year <- readRDS("filtered_wrds_with_default_per_year.rds")
#Assesing the model
#======================================
#K-fold CV won't work directly because we have an evolution on the data, 1980 is not comparable to 2010.
#In fact, time influences the data in our case.
#We will asses the model with the original Altman model

#Global parameters
years <- 1980:2010
vars_altman <- c("X1", "X2", "X3", "X4", "X5")


#WALK-FORWARD: EXPANDING WINDOW until t-1
# =============================================
results <- list()
roc_list <- list()

for (test_year in 1981:2010) {
  
  #Train years: 1980 until 2009
  train_years <- 1980:(test_year - 1)
  if (length(train_years) == 0) next
  
  #Builtind the training set: putting in a list all variables' year we will use per year
  train_list <- lapply(train_years, function(y) {
    cols <- c("gvkey", paste0("default_", y), paste0(vars_altman, "_", y))
    if (!all(cols %in% names(filtered_wrds_with_default_per_year))) return(NULL)
    
    df <- filtered_wrds_with_default_per_year[, cols, drop = FALSE]
    names(df) <- c("gvkey", "default", vars_altman)
    df$year <- y
    return(df)
  })
  
  #Delete years witout data
  train_list <- Filter(Negate(is.null), train_list)
  if (length(train_list) == 0) next
  
  train_data <- do.call(rbind, train_list) %>% na.omit()
  if (nrow(train_data) == 0) next
  
  #Test set: only test year
  test_cols <- c("gvkey", paste0("default_", test_year), paste0(vars_altman, "_", test_year))
  if (!all(test_cols %in% names(filtered_wrds_with_default_per_year))) next
  
  test_data <- filtered_wrds_with_default_per_year[, test_cols, drop = FALSE]
  names(test_data) <- c("gvkey", "default", vars_altman)
  test_data <- test_data %>% na.omit()
  if (nrow(test_data) == 0) next
  
  #Model Altman
  formula_altman <- as.formula(paste("default ~", paste(vars_altman, collapse = " + ")))
  model <- glm(formula_altman, data = train_data, family = binomial)
  
  #Predict
  pred_prob <- predict(model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  #Compute metrics (accuracy, ROC and AUC)
  accuracy <- mean(pred_class == test_data$default, na.rm = TRUE)
  roc_obj <- tryCatch(roc(test_data$default, pred_prob, quiet = TRUE), error = function(e) NULL)
  auc_val <- if (!is.null(roc_obj)) as.numeric(auc(roc_obj)) else NA
  
  #Save results
  results[[as.character(test_year)]] <- data.frame(
    test_year = test_year,
    train_years = paste(min(train_years), max(train_years), sep = "–"),
    n_train_firm_years = nrow(train_data),
    n_test_firms = nrow(test_data),
    accuracy = accuracy,
    auc = auc_val
  )
  
  if (!is.null(roc_obj)) {
    roc_list[[as.character(test_year)]] <- roc_obj
  }
  
  cat(sprintf("Test %d ← Train %s | Obs: %d → %d | AUC: %.3f\n",
              test_year, paste(min(train_years), max(train_years), sep = "–"),
              nrow(train_data), nrow(test_data), auc_val))
}

#Print final results
results_df <- do.call(rbind, results)
print(results_df)

cat("\nmean AUC (walk-forward):", round(mean(results_df$auc, na.rm = TRUE), 3), "\n")

#Put all ROC results together
#ROC per year
all_roc_data <- lapply(names(roc_list), function(y) {
  r <- roc_list[[y]]
  data.frame(
    fpr = 1 - r$specificities,
    tpr = r$sensitivities,
    year = y
  )
}) %>% do.call(rbind, .)

#Plot ROC per year
ggplot(all_roc_data, aes(x = fpr, y = tpr, color = year)) +
  geom_line(alpha = 0.7, size = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves per Year (Walk-Forward in Wide)",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)",
    color = "Years"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "lines")
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1)))

#Plot ROC all together
pooled_response <- unlist(lapply(roc_list, function(r) r$response))
pooled_predictor <- unlist(lapply(roc_list, function(r) r$predictor))
pooled_roc <- roc(pooled_response, pooled_predictor, quiet = TRUE)
plot(pooled_roc, main = paste("Agregated ROC - AUC =", round(auc(pooled_roc), 3)))

#CCL
#Huge AUC


