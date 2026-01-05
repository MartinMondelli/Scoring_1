#==================================================================
#Objectives: 

#Lasso CV

#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(tidyverse)
library(glmnet)
library(pROC)
library(ggplot2)

#Database
long_w <- readRDS("filtered_wrds_long_w_final_DtD_gdp_vol_ratios_w.rds")

#Filtering NAs in default (only 30)
long_w <- long_w %>% filter(!is.na(default))

#Converting to Numeric (just in case)
long_w$default <- as.numeric(as.character(long_w$default))

#Defining the variables we will use
X_vars <- c("X1","X2","X3","X4","X5", "SALEAT","QUALT","NIAT",
            "LCTSALE","CHAT","ACTLCT","CHEAT",
            "r1","r6","r7","r11","r17","r24","r28","r36",
            "Operating_CFM","Capex_At","RTO_r","ITS_r","debt_EBITDA")


#Global parameters
years <- sort(unique(long_w$fyear))
 
train_start <- 1980 #First year
oos_start   <- 1991 #At least 10 years of training
oos_end     <- 2010 #Last year

K_inner <- 5 #Folds by COMPANY
lambda_grid <- 10^seq(2, -4, length.out = 50)

#Generate folds by company
make_firm_folds <- function(gvkeys, K = 5, seed = 123) {
  set.seed(seed) #Seed
  firms <- unique(gvkeys) #To have the number of firms
  folds <- split(firms, sample(rep(1:K, length.out = length(firms)))) #Generate the folds, take different samples for each fold
  return(folds)
}

#To save the results
results_oos <- data.frame(
  year   = integer(),
  auc    = numeric(),
  lambda = numeric()
)

#Walk-Forward with Lasso restriction
for (test_year in oos_start:oos_end) {
  
  cat("Processing year:", test_year, "\n")
  
  # --------------------
  # Train / Test split
  # --------------------
  train <- dplyr::filter(long_w, fyear < test_year)
  test  <- dplyr::filter(long_w, fyear == test_year)
  
  # Matrices
  X_train_raw <- as.matrix(train[, X_vars])
  y_train     <- train$default
  
  X_test_raw  <- as.matrix(test[, X_vars])
  y_test      <- test$default
  
  # --------------------
  # Scaling (IMPORTANT)
  # --------------------
  X_train <- scale(X_train_raw)
  
  X_test <- scale(
    X_test_raw,
    center = attr(X_train, "scaled:center"),
    scale  = attr(X_train, "scaled:scale")
  )
  
  #Input NAs
  for (j in 1:ncol(X_train)) {
    X_train[is.na(X_train[, j]), j] <- mean(X_train[, j], na.rm = TRUE)
    X_test[is.na(X_test[, j]), j]   <- mean(X_train[, j], na.rm = TRUE)
  }
  
  # --------------------
  # INNER LOOP — CV by company
  # --------------------
  firm_folds <- make_firm_folds(train$gvkey, K = K_inner)
  
  lambda_auc <- numeric(length(lambda_grid))
  
  for (l in seq_along(lambda_grid)) {
    
    aucs_inner <- numeric(K_inner)
    
    for (k in 1:K_inner) {
      
      val_firms <- firm_folds[[k]]
      
      idx_val <- train$gvkey %in% val_firms
      idx_tr  <- !idx_val
      
      fit <- glmnet(
        x = X_train[idx_tr, ],
        y = y_train[idx_tr],
        family = "binomial",
        alpha  = 1,
        lambda = lambda_grid[l]
      )
      
      prob_val <- predict(
        fit,
        X_train[idx_val, ],
        type = "response"
      )
      
      roc_inner <- roc(y_train[idx_val], prob_val, quiet = TRUE)
      aucs_inner[k] <- as.numeric(auc(roc_inner))
      
      
    }
    
    lambda_auc[l] <- mean(aucs_inner, na.rm = TRUE)
  }
  
  # --------------------
  # Best lambda
  # --------------------
  best_lambda <- lambda_grid[which.max(lambda_auc)]
  
  # --------------------
  # Refit on FULL training
  # --------------------
  final_fit <- glmnet(
    x = X_train,
    y = y_train,
    family = "binomial",
    alpha  = 1,
    lambda = best_lambda
  )
  
  # --------------------
  # OOS evaluation
  # --------------------
  #OOS predictions
  prob_oos <- predict(final_fit, X_test, type = "response")
  
  #OOS AUC (without warnings)
  roc_obj <- roc(y_test, prob_oos, quiet = TRUE)
  auc_oos <- as.numeric(auc(roc_obj))
  
  #Save results
  results_oos <- rbind(
    results_oos,
    data.frame(
      year   = test_year,
      auc    = auc_oos,
      lambda = best_lambda
    )
  )
  
}

#Output for AUC 
mean(results_oos$auc, na.rm = TRUE)

#Lambda
summary(results_oos$lambda)

#Full model
X_full <- scale(as.matrix(long_w[, X_vars]))
y_full <- long_w$default

#Input NAs in X_full
for (j in 1:ncol(X_full)) {
  X_full[is.na(X_full[, j]), j] <- mean(X_full[, j], na.rm = TRUE)
}

#Scale
X_full <- scale(X_full)

lambda_final <- median(results_oos$lambda)

final_model <- glmnet(
  X_full, y_full,
  family = "binomial",
  alpha  = 1,
  lambda = lambda_final
)

coef_lasso <- coef(final_model)
coef_lasso #All coefficients chosen by Lasso
#[1] "(Intercept)" "X2" "X4" "X5" "r17" "Operating_CFM"
coef(final_model)[selected_vars, ] #Their values (to make it clearer)

#ROC Curve
prob_final <- predict(final_model, X_full, type = "response")
roc_obj <- roc(y_full, prob_final, quiet = TRUE)
plot(
  roc_obj,
  col = "blue",
  lwd = 2,
  main = paste0("ROC Curve (AUC = ", round(auc(roc_obj), 3), ")")
)
abline(a = 0, b = 1, lty = 2, col = "gray")

#Final AUC
auc_value <- auc(roc_obj)
print(auc_value)
