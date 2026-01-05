#==================================================================
#Objectives: 

#Redoing the baseline models with the data in long
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#libraries
library(tidyverse)

#Databases
filtered_wrds_long <- readRDS("filtered_wrds_long.rds")

#Exploring variables X1-X5
#=============================================

#First graph: X1 to X5 without X4 (too big)
df_x <- filtered_wrds_long %>%
  select(fyear, X1, X2, X3, X5) %>%
  pivot_longer(-fyear, names_to = "variable", values_to = "value") %>%
  group_by(fyear, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

p1 <- ggplot(df_x, aes(x = fyear, y = mean_value, color = variable)) +
  geom_line(linewidth = 1.1) +        
  geom_point(size = 1.5) +            
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  labs(title = "Evolution of X1–X5 without X4", x = "Year", y = "Mean by company", color = "Variable") +
  theme_minimal(base_size = 14)
print(p1)
#Same problem as with the mean by company and by year wuth X5

df_X4 <- filtered_wrds_long %>%
  group_by(fyear) %>%
  summarise(mean_X4 = mean(X4, na.rm = TRUE), .groups = "drop")

pX4 <- ggplot(df_X4, aes(x = fyear, y = mean_X4)) +
  geom_line(linewidth = 1.1, color = "steelblue") +
  geom_point(size = 1.5, color = "steelblue") +
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  labs(title = "Evolution of X4", x = "Year", y = "Mean of X4 per year") +
  theme_minimal(base_size = 14)
print(pX4)
#X4 also too big

#Model - Altman Z-Score
#=============================================
#With Z-Score
#Baseline model
baseline_model <- glm(default ~ AltmanZ, 
                      family = binomial(link = "logit"),
                      data = filtered_wrds_long)

summary(baseline_model) #Significative at less than 0.1% level

#Model - With X1 - X5
#=============================================
baseline_model_real <- glm(default ~ X1 + X2 + X3 + X4 + X5, 
                           family = binomial(link = "logit"),
                           data = filtered_wrds_long)
summary(baseline_model_real)

#Everything but X2 significative at less than 0.1% level (except X1 at 0.1%)

#Model - Altman & Rijken 2004
#=============================================

#Building size
total_us_market <- tibble::tibble(
  fyear = 1980:2010,
  total_market_cap = c(
    1078442, 1111350, 1359492, 1659852, 1728298, 2154862, 2519873, 2701594, 2998741, 3528741,
    3684123, 4567891, 5123456, 5678901, 5987654, 7456789, 9123456, 11345678, 13987654, 17123456,
    16456789, 14321789, 11845678, 14567890, 16543210, 18234567, 20456789, 21567890, 13456789,
    16543210, 19234567
  )
)

filtered_wrds_long <- left_join(filtered_wrds_long,total_us_market)
filtered_wrds_long$Size <- log(filtered_wrds_long$lt/filtered_wrds_long$total_market_cap)

#Building age
#Age
crsp <- readRDS("crsp_cleaned_only.rds")
crsp_first <- crsp %>%
  group_by(permno) %>%
  summarise(first_crsp_year = min(year(date))) %>%
  ungroup()
filtered_wrds_long <- filtered_wrds_long %>%
  left_join(crsp_first, by = "permno")
filtered_wrds_long$Age <- filtered_wrds_long$fyear-filtered_wrds_long$first_crsp_year
filtered_wrds_long$Age <- ifelse(filtered_wrds_long$Age<0, 0, filtered_wrds_long$Age)

#Plot
df_as <- filtered_wrds_long %>%
  select(fyear, Age, Size) %>%
  pivot_longer(-fyear, names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, age = "Age", size = "Size")) %>%
  group_by(fyear, variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(df_as, aes(x = fyear, y = mean_value, color = variable)) +
  geom_line(linewidth = 1.2) +        
  geom_point(size = 1.8) +            
  scale_x_continuous(breaks = seq(1980, 2010, 5)) +
  labs(title = "Size and Age per year", x = "Year", y = "Mean of companies", color = "Variable") +
  theme_minimal(base_size = 14)
print(p2)

#Model
Altman_Rijken_model_real <- glm(default ~ X1 + X2 + X3 + X4 + Size+ Age, 
                                family = binomial(link = "logit"),
                                data = filtered_wrds_long)
summary(Altman_Rijken_model_real)
#Everything is significant at 0.1% level except X1 and X2

#Winsorizing
#=============================================

#X1-X5
boxplot(filtered_wrds_long$X1, filtered_wrds_long$X2, filtered_wrds_long$X3, filtered_wrds_long$X5)
boxplot( filtered_wrds_long$X4)

#We clearly need to winsorize

filtered_wrds_long_w <- filtered_wrds_long %>%
  mutate(
    X1 = pmin(pmax(X1, quantile(X1, 0.1, na.rm=T)), quantile(X1, 0.99, na.rm=T)),
    X2 = pmin(pmax(X2, quantile(X2, 0.1, na.rm=T)), quantile(X2, 0.99, na.rm=T)),
    X3 = pmin(pmax(X3, quantile(X3, 0.1, na.rm=T)), quantile(X3, 0.97, na.rm=T)),
    X4 = pmin(pmax(X4, quantile(X4, 0.01, na.rm=T)), quantile(X4, 0.90, na.rm=T)),
    X5 = pmin(pmax(X5, quantile(X5, 0.005, na.rm=T)), quantile(X5, 0.95, na.rm=T)),
  ) %>%
  
  filter(if_all(c(X1, X2, X3, X4, X5), is.finite)) %>%
  mutate(default = factor(default, levels = c(0,1)))

boxplot(filtered_wrds_long_w$X1, filtered_wrds_long_w$X2, filtered_wrds_long_w$X3, filtered_wrds_long_w$X5)
boxplot( filtered_wrds_long_w$X4)
#No more outliers

#Size and Age
boxplot(filtered_wrds_long$Size)
filtered_wrds_long_w_final <- filtered_wrds_long_w %>%
  mutate(
    Size = pmin(pmax(X1, quantile(X1, 0.05, na.rm=T)), quantile(X1, 0.99, na.rm=T))
  ) %>%
  
  filter(if_all(Size, is.finite)) %>%
  mutate(default = factor(default, levels = c(0,1)))

boxplot(filtered_wrds_long_w_final$Size)
rm(filtered_wrds_long_w)

#We redo the models:
#X1 to X5
baseline_model_real_w <- glm(default ~ X1 + X2 + X3 + X4 + X5, 
                             family = binomial(link = "logit"),
                             data = filtered_wrds_long_w_final)
summary(baseline_model_real_w)
#Everything significant at least at 0.1% level (only X3 at 0.1%)

#X1 to X4 + Size + Age
Altman_Rijken_model_real_w <- glm(default ~ X1 + X2 + X3 + X4 + Size+ Age, 
                                  family = binomial(link = "logit"),
                                  data = filtered_wrds_long_w_final)
summary(Altman_Rijken_model_real_w)
#Almost everything significant at least at 5% level except X3 (X2, X4, Age at less than 0.1%)

#We prefer the baseline model because it has a lower AIC

#Computing DtD
#=============================================

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

filtered_wrds_long_w_final_DtD <- filtered_wrds_long_w_final %>%
  mutate(
    #Extract data from tibble (from CRSP)
    dtd_data = map(serie_diaria, ~ {
      .x %>%
        summarise(
          #Mean of market capitalization per year
          E = mean(prc * shrout, na.rm = TRUE),
          
          #σ_E volatility of returns using finance formula:
          #sigma_annual = sigma_daily * sqrt(number of trading days)
          n_days = sum(!is.na(ret)), #Sum of days in ret
          sigma_E_daily = sd(ret, na.rm = TRUE), #volatility when daily data
          sigma_E = if_else(
            n_days >= 60, #If we have more than two months we consider that we can compute the annual volatility of E
            sigma_E_daily * sqrt(pmin(n_days, 252)),
            NA_real_
          ),
          
          #μ_V mean return per year
          mu_V = mean(ret, na.rm = TRUE) * 252,
          mu_V = if_else(is.na(mu_V) | abs(mu_V) > 1, 0.06, mu_V) #If we have NAs or an absurde value (superior to the non risky asset) 
          #we take 6% as baseline returns (based on the literature)
        )
    }),
    #Computation of DP
    DP = dlc + 0.5 * dltt,
    #Computing naive DtD based on formula
    DtD_naive = pmap_dbl(list(dtd_data, DP), ~ {
      data <- ..1
      DP <- ..2
      if (is.na(data$E) || is.na(DP) || data$E <= 0 || DP <= 0 || is.na(data$sigma_E)) {
        return(NA_real_)
      } #If we lack of data, can't compute it so NA
      #Computing values based on the course
      sigma_D <- 0.05 + 0.25 * data$sigma_E
      V <- data$E + DP
      sigma_V <- (data$E / V) * data$sigma_E + (DP / V) * sigma_D
      T <- 1
      #Computing numerator and denominator separetly for better reading
      num <- log((data$E + DP) / DP) + (data$mu_V - 0.5 * sigma_V^2) * T
      den <- sigma_V * sqrt(T)
      return(num / den) #DtD
    }),
    #Probability of default
    PD = pnorm(-DtD_naive),
  ) %>%
  select(gvkey, fyear, default, Size, Age, DtD_naive, PD, everything(), -dtd_data, -DP)
rm(filtered_wrds_long_w_final)

#Models
#Only DtD
model_DtD <- glm(default ~ DtD_naive, 
                 family = binomial(link = "logit"),
                 data = filtered_wrds_long_w_final_DtD)
summary(model_DtD) #Very significative (less than 0.1%)

#We redo the models:
#X1 to X5
baseline_model_real_w_DtD <- glm(default ~ X1 + X2 + X3 + X4 + X5 + DtD_naive, 
                                 family = binomial(link = "logit"),
                                 data = filtered_wrds_long_w_final_DtD)
summary(baseline_model_real_w_DtD)
#Everything significant at least at 0.1% level (only X3 and DtD at 0.1%)

#X1 to X4 + Size + Age
Altman_Rijken_model_real_w_DtD <- glm(default ~ X1 + X2 + X3 + X4 + Size+ Age + DtD_naive, 
                                      family = binomial(link = "logit"),
                                      data = filtered_wrds_long_w_final_DtD)
summary(Altman_Rijken_model_real_w_DtD)
#Almost everything significant at 1% level except X3 and DtD (Age, X4, Age at less than 0.1%)

#Comparing with given DtD
compustat_dtd <- readRDS("_raw/compustat_data_dtd_full.rds")
compustat_dtd <- compustat_dtd %>% filter(gvkey %in% filtered_wrds_long_w_final_DtD$gvkey)
compustat_dtd <- compustat_dtd %>%
  mutate(date = ymd(date)) %>%
  filter(year(date) >= 1980 & year(date) <= 2010)
compustat_dtd$sig_DP <- 0.05 + 0.25 * compustat_dtd$sig_E_l_adj
compustat_dtd <- compustat_dtd %>% mutate(sig_V = (E / (E + DP_kmv)) * sig_E_l_adj + (DP_kmv / (E + DP_kmv)) * sig_DP)
compustat_dtd <- compustat_dtd %>% mutate(DtD = log((E + DP_kmv) / DP_kmv) + (r - 0.5 * sig_V^2) * 30 / (sig_V * sqrt(30)))
compustat_dtd <- compustat_dtd %>% select(gvkey, permno, date, DtD)
compustat_dtd <- compustat_dtd %>%
  mutate(fyear = year(date)) %>%
  select(-date)
filtered_wrds_long_DtD <- left_join(filtered_wrds_long_w_final_DtD_gdp_vol, compustat_dtd) #Why so big?

#We redo the models:
#X1 to X5
baseline_model_real_DtD <- glm(default ~ X1 + X2 + X3 + X4 + X5 + DtD, 
                               family = binomial(link = "logit"),
                               data = filtered_wrds_long_DtD)
summary(baseline_model_real_DtD)
#Everything significant at less than 0.1%, AIC = 1922136

#X1 to X4 + Size + Age
Altman_Rijken_model_real_DtD <- glm(default ~ X1 + X2 + X3 + X4 + Size+ Age + DtD, 
                                    family = binomial(link = "logit"),
                                    data = filtered_wrds_long_DtD)
summary(Altman_Rijken_model_real_DtD)
#Everything significant at less than 0.1%, AIC = 1953818 
#The AIC is lower in the model with the Naive DtD. Hence we prefer that one. 

#Adding Market and Macro variables
#=============================================

#Macro variable at January the 1st of each year
GDP <- read.csv("_raw/GDP.csv")
GDP <- GDP %>%
  filter(month(observation_date) == 01, day(observation_date) == 01) %>%
  mutate(fyear = year(observation_date)) %>%
  select(-observation_date)

filtered_wrds_long_w_final_DtD_gdp <- left_join(filtered_wrds_long_w_final_DtD, GDP)

#Market variable - mean volume per year
filtered_wrds_long_w_final_DtD_gdp_vol <- filtered_wrds_long_w_final_DtD_gdp %>%
  mutate(
    vol_mean_annual = map_dbl(serie_diaria, ~ {
      # Extraer vol y calcular promedio
      mean_vol <- mean(.x$vol, na.rm = TRUE)
      
      # Si todos son NA → devolver NA
      if (is.na(mean_vol)) return(NA_real_)
      
      return(mean_vol)
    })
  )
rm(filtered_wrds_long_w_final_DtD, filtered_wrds_long_w_final_DtD_gdp, GDP)

#Saving for the future
saveRDS(filtered_wrds_long_w_final_DtD_gdp_vol, "filtered_wrds_long_w_final_DtD_gdp_vol.rds")

#We redo the models:
#X1 to X5
baseline_model_real_w_DtD_gdp_vol <- glm(default ~ X1 + X2 + X3 + X4 + X5 + DtD_naive + GDP +vol_mean_annual, 
                                         family = binomial(link = "logit"),
                                         data = filtered_wrds_long_w_final_DtD_gdp_vol)
summary(baseline_model_real_w_DtD_gdp_vol)
#Everything significant at less than 0.1% level except X1 (only X3 at 1% and vol at 0.1% exactly)

#X1 to X4 + Size + Age
Altman_Rijken_model_real_w_DtD_gdp_vol <- glm(default ~ X1 + X2 + X3 + X4 + Size+ Age + DtD_naive +GDP +vol_mean_annual, 
                                              family = binomial(link = "logit"),
                                              data = filtered_wrds_long_w_final_DtD_gdp_vol)
summary(Altman_Rijken_model_real_w_DtD_gdp_vol)
#Almost everything significant at less than 1% level except X3 (Age, X1 at 1% and vol at 0.1%)

#Overall, we prefer the default = X1 + X2 + X3 + X4 + X5 + DtD_naive + GDP +vol_mean_annual because 
#it has a lower AIC but their AIC is quite close. Even when considering the "true" DtD
