# U.S. Public Company Failures Prediction (1980–2010)

Final project for the Master's in Data Science for Social Sciences  
**Students:** Francisco González García · Martin Mondelli  
**Date:** December 2025

## Project Goal

Build predictive models of bankruptcy (default) for U.S. public companies using mainly the **CRSP**, **Compustat**, and **UCLA–LoPucki Bankruptcy Research Database** datasets, covering the period 1980–2010.

The project explores and compares:

- Classic Altman (1968) model
- Extensions (Altman & Rijken 2004, naive and real Distance to Default)
- Additional financial ratios (WRDS Industry ratios + Desbois-inspired ratios)
- LASSO penalized logistic regression
- Gradient Boosting (XGBoost)

## Repository Structure

```bash
project-bankruptcy-prediction/
├── 0_process.R               # Initial cleaning & preparation of CRSP + Compustat
├── 1_descriptive_statistics.R # Descriptive analysis (e.g. airlines comparison)
├── 2_wide.R                  # Wide format construction
├── 3_final_dataset.R         # Final merge + Altman Z-score calculation
├── 4_1_baseline_models.R     # Baseline Altman models (mean & last years)
├── 4_2_default_per_year.R    # Yearly default indicator for walk-forward
├── 5_enhanced_model.R        # Winsorization + extended models
├── 6_dtd.R                   # Distance to Default calculation (naive & real)
├── 7_macro_vars.R            # Adding GDP & market trading volume
├── 8_long_format.R           # Long format conversion + long walk-forward
├── 9_ratios.R                # Additional financial ratios construction
├── 10_lasso.R                # Walk-forward + LASSO
├── 11_xgboost.R              # Walk-forward + XGBoost
├── Scoring_Gonzalez_Mondelli_2.Rmd   # Main results report (the document you're reading)
└── README.md
```

## Data (not included)

The original and processed datasets are **not** included in this repository due to:

- Very large size (several GB)
- Restrictive licensing of WRDS/Compustat/CRSP
- Sensitive company-level information

To fully reproduce the analysis you will need your own access to:

- WRDS platform (Compustat, CRSP, Industry Financial Ratios)
- Bankruptcy Research Database (LoPucki)
- Then run `0_process.R` as the starting point

## Main Requirements

```r
R ≥ 4.0
tidyverse, lubridate, pROC, glmnet, xgboost, purrr
```

## Important Note
The main results document (tables, plots, interpretation) is:
Scoring_Gonzalez_Mondelli_2.Rmd
Feel free to review, comment on or improve any part of the code!
Thank you for your interest in this academic project! ✈️📉

## Licence

This is an **academic project** developed as part of the Master's program in Data Science for Social Sciences.
