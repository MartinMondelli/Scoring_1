#==================================================================
#Objectives: 

#Redoing the assessment on the baseline models with the data in long
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(dplyr)
library(pROC)
library(ggplot2)

#Database
filtered_wrds_long_assessment <- readRDS("filtered_wrds_long_assessment.rds")

# ========================================
# WALK-FORWARD VALIDATION using 1980 - (test_year -1) as training set and test_year as test set
# ========================================

#We only test with Altman model
vars_altman <- c("X1", "X2", "X3", "X4", "X5")

# ========================================
# WALK-FORWARD LOOP
# ========================================
results <- list()
roc_list <- list()

for (test_year in 1981:2010) {
  
  cat("Processing set:", test_year, "\n")
  
  #Train with all years before test year
  train_data <- filtered_wrds_long_assessment %>%
    filter(fyear < test_year) %>%
    select(gvkey, fyear, default_year, all_of(vars_altman)) %>%
    na.omit()
  
  if (nrow(train_data) == 0) {
    cat("No training set, SKIP.\n")
    next
  }
  
  #Test only test year
  test_data <- filtered_wrds_long_assessment %>%
    filter(fyear == test_year) %>%
    select(gvkey, fyear, default_year, all_of(vars_altman)) %>%
    na.omit()
  
  if (nrow(test_data) == 0) {
    cat("No test set, SKIP.\n")
    next
  }
  
  #Altman model (train)
  formula_altman <- as.formula(paste("default_year ~", paste(vars_altman, collapse = " + ")))
  model <- glm(formula_altman, data = train_data, family = binomial(link = "logit"))
  
  #Predictions
  pred_prob <- predict(model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  #Metrics (accuracy, ROC, AUC)
  accuracy <- mean(pred_class == test_data$default_year, na.rm = TRUE)
  
  roc_obj <- tryCatch(
    roc(test_data$default_year, pred_prob, quiet = TRUE),
    error = function(e) NULL
  )
  
  auc_val <- if (!is.null(roc_obj)) as.numeric(auc(roc_obj)) else NA
  
  #Save them
  results[[as.character(test_year)]] <- data.frame(
    test_year = test_year,
    train_years = paste(1980, test_year - 1, sep = "–"),
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    accuracy = accuracy,
    auc = auc_val,
    stringsAsFactors = FALSE
  )
  
  if (!is.null(roc_obj)) {
    roc_list[[as.character(test_year)]] <- roc_obj
  }
  
  cat(sprintf("  Train: %d obs | Test: %d firms | AUC: %.3f | Acc: %.3f\n",
              nrow(train_data), nrow(test_data), auc_val, accuracy))
}

#Final results
#========================================
results_df <- do.call(rbind, results)
print(results_df)

#Mean AUC
auc_mean <- mean(results_df$auc, na.rm = TRUE)
cat("\nMean AUC (walk-forward expanding):", round(auc_mean, 3), "\n")

#ROC per year
#========================================
all_roc_data <- lapply(names(roc_list), function(y) {
  r <- roc_list[[y]]
  data.frame(
    fpr = 1 - r$specificities,
    tpr = r$sensitivities,
    year = y
  )
}) %>% do.call(rbind, .)

p1 <- ggplot(all_roc_data, aes(x = fpr, y = tpr, color = year)) +
  geom_line(alpha = 0.7, size = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves per Year (Walk-Forward Expanding Window)",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9)
  )
print(p1)

#Pooled ROC
#========================================
pooled_response <- unlist(lapply(roc_list, function(r) r$response))
pooled_predictor <- unlist(lapply(roc_list, function(r) r$predictor))
pooled_roc <- roc(pooled_response, pooled_predictor, quiet = TRUE)

#AUC IC
ci_auc <- ci.auc(pooled_roc, conf.level = 0.95)
cat("AUC Agregado:", round(auc(pooled_roc), 3),
    "[", round(ci_auc[1], 3), "–", round(ci_auc[3], 3), "]\n")

plot(pooled_roc, 
     main = paste0("Agregated ROC\nAUC = ", 
                   round(auc(pooled_roc), 3), 
                   " [", round(ci_auc[1], 3), "–", round(ci_auc[3], 3), "]"),
     col = "blue", lwd = 2)
abline(0, 1, lty = 2, col = "gray")
dev.off()

#Better the wide way - more predicting power