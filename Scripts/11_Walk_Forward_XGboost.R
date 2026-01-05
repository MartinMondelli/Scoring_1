#==================================================================
#Objectives: 

#Classification trees and ensemble methods by CV

#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(tidyverse)
library(xgboost)
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

X_full <- as.matrix(long_w[, X_vars])
y_full <- as.numeric(long_w$default)

#Global parameters
years <- sort(unique(long_w$fyear))

train_start <- 1980 #First year
oos_start   <- 1991 #At least 10 years of training
oos_end     <- 2010 #Last year

#Grid search
param_grid <- expand.grid(
  max_depth   = c(2, 4, 6),
  eta         = c(0.01, 0.05, 0.1),
  nrounds     = c(50, 100, 200)
)

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
results_oos <- data.frame()

for (test_year in oos_start:oos_end) {
  
  cat("Processing year:", test_year, "\n")
  
  train <- dplyr::filter(long_w, fyear < test_year)
  test  <- dplyr::filter(long_w, fyear == test_year)
  
  X_train <- as.matrix(train[, X_vars])
  y_train <- train$default
  
  X_test <- as.matrix(test[, X_vars])
  y_test <- test$default
  
  # -----------------------
  # Inner CV para hiperparámetros
  # -----------------------
  firm_folds <- make_firm_folds(train$gvkey, K = 5)
  best_auc <- -Inf
  best_params <- NULL
  
  for (i in 1:nrow(param_grid)) {
    
    params <- param_grid[i, ]
    
    aucs_inner <- c()
    
    for (k in 1:5) {
      
      val_firms <- firm_folds[[k]]
      idx_val <- train$gvkey %in% val_firms
      idx_tr  <- !idx_val
      
      dtrain <- xgb.DMatrix(data = X_train[idx_tr, ], label = y_train[idx_tr])
      dval   <- xgb.DMatrix(data = X_train[idx_val, ], label = y_train[idx_val])
      
      bst <- xgb.train(
        data = dtrain,
        max_depth = params$max_depth,
        eta       = params$eta,
        nrounds   = params$nrounds,
        objective = "binary:logistic",
        verbose   = 0
      )
      
      pred_val <- predict(bst, dval)
      roc_obj <- roc(y_train[idx_val], pred_val, quiet = TRUE)
      aucs_inner[k] <- as.numeric(auc(roc_obj))
    }
    
    mean_auc <- mean(aucs_inner)
    if (mean_auc > best_auc) {
      best_auc <- mean_auc
      best_params <- params
    }
  }
  
  # -----------------------
  # Train final model with the best parameters
  # -----------------------
  dtrain_full <- xgb.DMatrix(data = X_train, label = y_train)
  dtest <- xgb.DMatrix(data = X_test, label = y_test)
  
  bst_final <- xgb.train(
    data = dtrain_full,
    max_depth = best_params$max_depth,
    eta       = best_params$eta,
    nrounds   = best_params$nrounds,
    objective = "binary:logistic",
    verbose   = 0
  )
  
  pred_oos <- predict(bst_final, dtest)
  roc_obj_oos <- roc(y_test, pred_oos, quiet = TRUE)
  auc_oos <- as.numeric(auc(roc_obj_oos))
  
  results_oos <- rbind(results_oos, 
                       data.frame(year = test_year, auc = auc_oos,
                                  max_depth = best_params$max_depth,
                                  eta = best_params$eta,
                                  nrounds = best_params$nrounds))
}
results_oos
saveRDS(results_oos, "results_oos_xgboost.rds")
#   year       auc max_depth  eta nrounds
#1  1991 0.7868620         2 0.05      50
#2  1992 0.8231638         2 0.10      50
#3  1993 0.8199064         2 0.10      50
#4  1994 0.7880904         2 0.05     100
#5  1995 0.7615814         2 0.05     100
#6  1996 0.7889395         2 0.05      50
#7  1997 0.7799354         2 0.05      50
#8  1998 0.7990260         2 0.05     100
#9  1999 0.7965814         2 0.10      50
#10 2000 0.7507830         2 0.10      50
#11 2001 0.7653654         2 0.10      50
#12 2002 0.7598428         2 0.01     200
#13 2003 0.8027783         2 0.05     100
#14 2004 0.7864430         2 0.05      50
#15 2005 0.8033467         2 0.10      50
#16 2006 0.7666372         2 0.05      50
#17 2007 0.7386539         2 0.01     200
#18 2008 0.7534683         2 0.05      50
#19 2009 0.7057655         2 0.10      50
#20 2010 0.6534521         2 0.05     100

#Plot evolution of AUC
ggplot(results_oos, aes(x = year, y = auc)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "red") +
  ylim(0.5, 0.85) +
  labs(
    title = "AUC yearly OOS by year - XGBoost",
    x = "Year",
    y = "AUC"
  ) +
  theme_minimal()

#Choose best hyper parameters
best_eta <- as.numeric(names(sort(table(results_oos$eta), decreasing = TRUE)[1]))
best_nrounds <- as.numeric(names(sort(table(results_oos$nrounds), decreasing = TRUE)[1]))
best_max_depth <- as.numeric(names(sort(table(results_oos$max_depth), decreasing = TRUE)[1]))

best_eta
best_nrounds
best_max_depth

#Make the model xgboost total
X_full <- as.matrix(long_w[, X_vars])
y_full <- as.numeric(long_w$default)

#For NAs
for(j in 1:ncol(X_full)){
  X_full[is.na(X_full[,j]), j] <- mean(X_full[,j], na.rm = TRUE)
}

#Dtrain matrix
dtrain <- xgb.DMatrix(data = X_full, label = y_full)

#Model
set.seed(123)
bst_global <- xgb.train(
  data = dtrain,
  max_depth = best_max_depth,
  eta = best_eta,
  nrounds = best_nrounds,
  objective = "binary:logistic",
  verbose = 0
)

#Predict AUC
prob_global <- predict(bst_global, dtrain)
roc_global <- roc(y_full, prob_global, quiet = TRUE)
auc_global <- as.numeric(auc(roc_global))
auc_global #0.7704859

#ROC
roc_df <- data.frame(
  tpr = rev(roc_global$sensitivities),
  fpr = rev(1 - roc_global$specificities)
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = paste0("ROC Curve - XGBoost Global (AUC = ", round(auc_global,3),")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme_minimal()
