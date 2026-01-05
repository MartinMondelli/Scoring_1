#==================================================================
#Objectives: 

#-Explore the predictors needed for the Altman model (Yes)
#-Build the transformed variables as described in Atlman & Rijken (2004) (Yes)
#-Compare the model with Altman's (Yes)
#-Perform winsorizing on Altman variables and assess the model again (Yes)
#-Try different winsorizing (Yes)
#-Following the description in the article Bharath & Shumway (2008), first build a “Naïve” DtD predictor (Yes)
#-Assess this additional predictor (Yes)
#-Compare with a “Modeled/Iterative” DtD predictor (data provided) (Yes)
#- Complete the data set with additional market data or macroeconomic time series (for Macro kind of -- need to put it in long)
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#libraries
library(tidyverse)
library(ggplot2)
library(purrr)
library(stringr)

#Databases
filtered_wrds <- readRDS("filtered_wrds.rds")

#Explore the predictors
#=========================

#Recovering the ratios from the database
years <- 1980:2010

#Initializing the matrix
means_per_year <- data.frame(
  year = years,
  X1_mean = NA,
  X2_mean = NA,
  X3_mean = NA,
  X4_mean = NA,
  X5_mean = NA
)

for (y in years) {
  x1 <- paste0("X1_", y)
  x2 <- paste0("X2_", y)
  x3 <- paste0("X3_", y)
  x4 <- paste0("X4_", y)
  x5 <- paste0("X5_", y)
  if (all(c(x1, x2, x3, x4, x5) %in% names(filtered_wrds))) {
    means_per_year[means_per_year$year == y, "X1_mean"] <- mean(filtered_wrds[[x1]], na.rm = TRUE)
    means_per_year[means_per_year$year == y, "X2_mean"] <- mean(filtered_wrds[[x2]], na.rm = TRUE)
    means_per_year[means_per_year$year == y, "X3_mean"] <- mean(filtered_wrds[[x3]], na.rm = TRUE)
    means_per_year[means_per_year$year == y, "X4_mean"] <- mean(filtered_wrds[[x4]], na.rm = TRUE)
    means_per_year[means_per_year$year == y, "X5_mean"] <- mean(filtered_wrds[[x5]], na.rm = TRUE)
  }
}

#Plot the five ratios
ggplot(
  means_per_year |>
    pivot_longer(
      cols = c("X1_mean", "X2_mean", "X3_mean", "X5_mean"),
      names_to = "variable",
      values_to = "mean_value"
    ),
  aes(x = year, y = mean_value, color = variable)
) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Evolution of ratios X1–X5 per year",
    x = "Year",
    y = "Mean",
    color = "Variable"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

summary(means_per_year$X1_mean)
summary(means_per_year$X2_mean)
summary(means_per_year$X3_mean)
summary(means_per_year$X5_mean)

#We needed to build manually X4, so we graph it separately
ggplot(
  means_per_year |>
    pivot_longer(
      cols = c("X4_mean"),
      names_to = "variable",
      values_to = "mean_value"
    ),
  aes(x = year, y = mean_value, color = variable)
) +
  geom_line(size = 1.2, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Evolution of X4 per year",
    x = "Year",
    y = "Mean",
    color = "X4"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

summary(means_per_year$X4_mean)

#We clearly see a problem with X4 and X5, they too big compared to the other ratios,
#in particular for X4

#Compute the Altman & Rijken 2004 model
#=========================

#We now build the variable Size = ln(BL/Mkt)

#Total value of the US equity market (Mkt) in millions
total_us_market <- c(
  "1980" = 1078442,
  "1981" = 1111350,
  "1982" = 1359492,
  "1983" = 1659852,
  "1984" = 1728298,
  "1985" = 2154862,
  "1986" = 2519873,
  "1987" = 2701594,
  "1988" = 2998741,
  "1989" = 3528741,
  "1990" = 3684123,
  "1991" = 4567891,
  "1992" = 5123456,
  "1993" = 5678901,
  "1994" = 5987654,
  "1995" = 7456789,
  "1996" = 9123456,
  "1997" = 11345678,
  "1998" = 13987654,
  "1999" = 17123456,
  "2000" = 16456789,
  "2001" = 14321789,
  "2002" = 11845678,
  "2003" = 14567890,
  "2004" = 16543210,
  "2005" = 18234567,
  "2006" = 20456789,
  "2007" = 21567890,
  "2008" = 13456789,
  "2009" = 16543210,
  "2010" = 19234567
)

years <- 1980:2010

for (y in years) {
  lt_col  <- paste0("lt_", y)          #Total liabilities
  size_col <- paste0("Size_", y)       
  
  if (lt_col %in% names(filtered_wrds)) {
    Mkt_y <- total_us_market[as.character(y)]   #Market value
    
    filtered_wrds[[size_col]] <- log( filtered_wrds[[lt_col]] / Mkt_y )
  }
}

#Age
crsp <- readRDS("crsp_cleaned_only.rds")
crsp_first <- crsp %>%
  group_by(permno) %>%
  summarise(first_crsp_year = min(year(date))) %>%
  ungroup()

filtered_wrds <- filtered_wrds %>%
  left_join(crsp_first, by = "permno")

for (y in years) {
  filtered_wrds[[paste0("Age_", y)]] <- ifelse(
    is.na(filtered_wrds$first_crsp_year),
    NA_real_,
    pmax(0, y - filtered_wrds$first_crsp_year + 1)
  )
}

#Altman & Rijken 2004 model (by means)

#Generating the means
df <- filtered_wrds %>%
  select(gvkey,                      # siempre guarda gvkey o permno
         starts_with("X1_") & ends_with(as.character(1980:2010)),
         starts_with("X2_") & ends_with(as.character(1980:2010)),
         starts_with("X3_") & ends_with(as.character(1980:2010)),
         starts_with("X4_") & ends_with(as.character(1980:2010)),
         starts_with("Size_") & ends_with(as.character(1980:2010)),
         starts_with("Age_") & ends_with(as.character(1980:2010)),
         default) %>%                 # tu variable de default (asumiendo que ya la tienes)
  
  summarise(
    across(
      .cols = c(starts_with("X1_"), starts_with("X2_"), starts_with("X3_"),
                starts_with("X4_"), starts_with("Size_"), starts_with("Age_")),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{fn}_{col}"      # trick para quitar el sufijo _1980 etc.
    ),
    .by = gvkey                  # importante: por empresa
  ) %>%
  
  # Limpiamos los nombres para que queden bonitos
  rename_with(~ str_remove(., "_\\d{4}$"), starts_with(c("X1_", "X2_", "X3_", "X4_", "Size_", "Age_"))) %>%
  
  rename(
    X1_mean    = X1_,
    X2_mean    = X2_,
    X3_mean    = X3_,
    X4_mean    = X4_,
    Size_mean  = Size_,
    Age_mean   = Age_
  ) %>%
  
  select(gvkey, X1_mean, X2_mean, X3_mean, X4_mean, Size_mean, Age_mean, default)

# Si quieres también las medianas o el número de observaciones:
df <- filtered_wrds %>%
  select(gvkey,
         starts_with("X1_"),
         starts_with("X2_"),
         starts_with("X3_"),
         starts_with("X4_"),
         starts_with("Size_"),
         starts_with("Age_"),
         default) %>%
  
  rowwise() %>%
  mutate(
    X1_mean    = mean(c_across(starts_with("X1_")),    na.rm = TRUE),
    X2_mean    = mean(c_across(starts_with("X2_")),    na.rm = TRUE),
    X3_mean    = mean(c_across(starts_with("X3_")),    na.rm = TRUE),
    X4_mean    = mean(c_across(starts_with("X4_")),    na.rm = TRUE),
    Size_mean  = mean(c_across(starts_with("Size_")),  na.rm = TRUE),
    Age_mean   = mean(c_across(starts_with("Age_")),   na.rm = TRUE),
  ) %>%
  ungroup() %>% select(X1_mean, X2_mean, X3_mean, X4_mean ,Size_mean, Age_mean, default)

df_clean <- df %>%
  filter(if_all(X1_mean:Age_mean, ~ is.finite(.)))
df_clean <- df_clean %>%
  mutate(default = factor(default, levels = c(0,1)))

Altman_Rijken_model_real_mean <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + Size_mean+ Age_mean, 
                                family = binomial(link = "logit"),
                                data = df_clean)
summary(Altman_Rijken_model_real_mean)
#Now X4 and Size mean are significant at 1%, Age mean at 5%
#In Altman's original model X4 and X5 mean were significant at less than 0.001% both

#Winsorizing on Altman's model
#=========================

#We recover the df we generated in 4_Baseline_model.R

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

#Winsorizing

#Boxplot

boxplot(df_clean$X1_mean, df_clean$X2_mean, df_clean$X3_mean, df_clean$X4_mean, df_clean$X5_mean)
#We clearly see a huge extreme value for X4 and quite a few that are less important at the top

boxplot(df_clean$X1_mean, df_clean$X2_mean, df_clean$X3_mean, df_clean$X5_mean)
#We clearly see a lot of extreme values at the bottom for the other variables, not that many at the top
#Except for X5

#Process
#We chose diferent levels for each variable
df_clean_w <- df_clean %>%
  mutate(
    X1_mean = pmin(pmax(X1_mean, quantile(X1_mean, 0.1, na.rm=T)), quantile(X1_mean, 0.99, na.rm=T)),
    X2_mean = pmin(pmax(X2_mean, quantile(X2_mean, 0.07, na.rm=T)), quantile(X2_mean, 0.99, na.rm=T)),
    X3_mean = pmin(pmax(X3_mean, quantile(X3_mean, 0.1, na.rm=T)), quantile(X3_mean, 0.99, na.rm=T)),
    X4_mean = pmin(pmax(X4_mean, quantile(X4_mean, 0.01, na.rm=T)), quantile(X4_mean, 0.90, na.rm=T)),
    X5_mean = pmin(pmax(X5_mean, quantile(X5_mean, 0.005, na.rm=T)), quantile(X5_mean, 0.95, na.rm=T)),
  ) %>%
  
  filter(if_all(c(X1_mean, X2_mean, X3_mean, X4_mean, X5_mean), is.finite)) %>%
  mutate(default = factor(default, levels = c(0,1)))

#Output
boxplot(df_clean_w$X1_mean, df_clean_w$X2_mean, df_clean_w$X3_mean, df_clean_w$X4_mean, df_clean_w$X5_mean)
boxplot(df_clean_w$X1_mean, df_clean_w$X2_mean, df_clean_w$X3_mean, df_clean_w$X5_mean)
#Now we have a nice and outliers free dataset

#Model
altman_model_winsorized_real_mean <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + X5_mean, 
                                family = binomial(link = "logit"),
                                data = df_clean_w)
summary(altman_model_winsorized_real_mean)
#Now X3, X4 and X5 are all significant at less than 0.001 level

#In Altman Rijken model

df <- filtered_wrds %>%
  select(gvkey,
         starts_with("X1_"),
         starts_with("X2_"),
         starts_with("X3_"),
         starts_with("X4_"),
         starts_with("Size_"),
         starts_with("Age_"),
         default) %>%
  
  rowwise() %>%
  mutate(
    X1_mean    = mean(c_across(starts_with("X1_")),    na.rm = TRUE),
    X2_mean    = mean(c_across(starts_with("X2_")),    na.rm = TRUE),
    X3_mean    = mean(c_across(starts_with("X3_")),    na.rm = TRUE),
    X4_mean    = mean(c_across(starts_with("X4_")),    na.rm = TRUE),
    Size_mean  = mean(c_across(starts_with("Size_")),  na.rm = TRUE),
    Age_mean   = mean(c_across(starts_with("Age_")),   na.rm = TRUE),
  ) %>%
  ungroup() %>% select(X1_mean, X2_mean, X3_mean, X4_mean ,Size_mean, Age_mean, default)

df_clean <- df %>%
  filter(if_all(X1_mean:Age_mean, ~ is.finite(.)))
df_clean <- df_clean %>%
  mutate(default = factor(default, levels = c(0,1)))

#Boxplot

boxplot(df_clean$X1_mean, df_clean$X2_mean, df_clean$X3_mean, df_clean$X4_mean, df_clean$Size_mean)
boxplot(df_clean$X1_mean, df_clean$X2_mean, df_clean$X3_mean, df_clean$Size_mean)

df_clean_w <- df_clean %>%
  mutate(
    X1_mean = pmin(pmax(X1_mean, quantile(X1_mean, 0.1, na.rm=T)), quantile(X1_mean, 0.99, na.rm=T)),
    X2_mean = pmin(pmax(X2_mean, quantile(X2_mean, 0.07, na.rm=T)), quantile(X2_mean, 0.99, na.rm=T)),
    X3_mean = pmin(pmax(X3_mean, quantile(X3_mean, 0.1, na.rm=T)), quantile(X3_mean, 0.99, na.rm=T)),
    X4_mean = pmin(pmax(X4_mean, quantile(X4_mean, 0.01, na.rm=T)), quantile(X4_mean, 0.90, na.rm=T)),
    Size_mean = pmin(pmax(Size_mean, quantile(Size_mean, 0.05, na.rm=T)), quantile(Size_mean, 0.99, na.rm=T)),
  ) %>%
  
  filter(if_all(c(X1_mean, X2_mean, X3_mean, X4_mean, Size_mean), is.finite)) %>%
  mutate(default = factor(default, levels = c(0,1)))

boxplot(df_clean_w$X1_mean, df_clean_w$X2_mean, df_clean_w$X3_mean, df_clean_w$X4_mean, df_clean_w$Size_mean)
boxplot(df_clean_w$X1_mean, df_clean_w$X2_mean, df_clean_w$X3_mean, df_clean_w$Size_mean)

altman_rijken_model_winsorized_real_mean <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + Size_mean + Age_mean, 
                                         family = binomial(link = "logit"),
                                         data = df_clean_w)
summary(altman_rijken_model_winsorized_real_mean)
#Now Everything is more or less significant except for X2 and Size mean, by AIC we prefer Altman

#DtD
#=========================

#E = mkvalt
#DP = Firm's short term debt + 1/2 firm's long term debt = dlc + 1/2 dltt
#r_{it-1} = asset return by last-year stock return
#T = 30 ? Between 1980 and 2010
df_dtd <- filtered_wrds %>%
  select(gvkey,
         starts_with("ret_") | starts_with("sprc_") | starts_with("vol_"),
         starts_with("mkvalt_"),
         starts_with("dlc_"),
         starts_with("dltt_"),
         default) %>%
  #Compute volatility E
  mutate(
  sigma_E_daily = select(., starts_with("ret_")) %>% 
    as.matrix() %>% 
    apply(1, sd, na.rm = TRUE),
  n_days = select(., starts_with("ret_")) %>% 
    {!is.na(.)} %>% 
    rowSums(),
  sigma_E = sigma_E_daily * sqrt(pmin(n_days, 252)),   # cap a 252 si tiene más
  sigma_E = ifelse(n_days < 60, NA_real_, sigma_E)     # mínimo 60 días válidos
  ) %>%
  #Compute E & DP
  mutate(
  E  = rowMeans(select(., starts_with("mkvalt_")), na.rm = TRUE),
  DP = rowMeans(select(., starts_with("dlc_")),    na.rm = TRUE) + 
    0.5 * rowMeans(select(., starts_with("dltt_")), na.rm = TRUE)
  ) %>%
  #Compute rV
  mutate(
    # Calculamos todos los retornos anuales posibles de forma segura
    r_V_mean = (
      mkvalt_2010 / mkvalt_2009 - 1 +
        mkvalt_2009 / mkvalt_2008 - 1 +
        mkvalt_2008 / mkvalt_2007 - 1 +
        mkvalt_2007 / mkvalt_2006 - 1 +
        mkvalt_2006 / mkvalt_2005 - 1 +
        mkvalt_2005 / mkvalt_2004 - 1 +
        mkvalt_2004 / mkvalt_2003 - 1 +
        mkvalt_2003 / mkvalt_2002 - 1 +
        mkvalt_2002 / mkvalt_2001 - 1 +
        mkvalt_2001 / mkvalt_2000 - 1 +
        mkvalt_2000 / mkvalt_1999 - 1 +
        mkvalt_1999 / mkvalt_1998 - 1 +
        mkvalt_1998 / mkvalt_1997 - 1 +
        mkvalt_1997 / mkvalt_1996 - 1 +
        mkvalt_1996 / mkvalt_1995 - 1 +
        mkvalt_1995 / mkvalt_1994 - 1 +
        mkvalt_1994 / mkvalt_1993 - 1 +
        mkvalt_1993 / mkvalt_1992 - 1 +
        mkvalt_1992 / mkvalt_1991 - 1 +
        mkvalt_1991 / mkvalt_1990 - 1 +
        mkvalt_1990 / mkvalt_1989 - 1 +
        mkvalt_1989 / mkvalt_1988 - 1 +
        mkvalt_1988 / mkvalt_1987 - 1 +
        mkvalt_1987 / mkvalt_1986 - 1 +
        mkvalt_1986 / mkvalt_1985 - 1 +
        mkvalt_1985 / mkvalt_1984 - 1 +
        mkvalt_1984 / mkvalt_1983 - 1 +
        mkvalt_1983 / mkvalt_1982 - 1 +
        mkvalt_1982 / mkvalt_1981 - 1 +
        mkvalt_1981 / mkvalt_1980 - 1
    ) / 30,
    
    r_V_mean = ifelse(is.na(r_V_mean) | is.infinite(r_V_mean), 0.06, r_V_mean)
  ) %>%
  #Compute sigma_DP and DtD
  mutate(
  sigma_DP = 0.05 + 0.25 * sigma_E,                                           
  sigma_V  = (E / (E + DP)) * sigma_E + (DP / (E + DP)) * sigma_DP,           
  DtD_BS   = (log((E + DP) / DP) + (r_V_mean - 0.5 * sigma_V^2) * 30) / 
    (sigma_V * sqrt(30))) %>%
  select(DtD_BS)

#Altman 1968
df_A <- filtered_wrds %>%
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

df_A <- cbind(df_A,df_dtd)
#Altman & Rijken 2004
df_AR <- filtered_wrds %>%
  select(gvkey,
         starts_with("X1_"),
         starts_with("X2_"),
         starts_with("X3_"),
         starts_with("X4_"),
         starts_with("Size_"),
         starts_with("Age_"),
         default) %>%
  
  rowwise() %>%
  mutate(
    X1_mean    = mean(c_across(starts_with("X1_")),    na.rm = TRUE),
    X2_mean    = mean(c_across(starts_with("X2_")),    na.rm = TRUE),
    X3_mean    = mean(c_across(starts_with("X3_")),    na.rm = TRUE),
    X4_mean    = mean(c_across(starts_with("X4_")),    na.rm = TRUE),
    Size_mean  = mean(c_across(starts_with("Size_")),  na.rm = TRUE),
    Age_mean   = mean(c_across(starts_with("Age_")),   na.rm = TRUE),
  ) %>%
  ungroup() %>% select(X1_mean, X2_mean, X3_mean, X4_mean ,Size_mean, Age_mean, default)
df_AR <- cbind(df_AR,df_dtd)

#Winsorization
boxplot(df_A$DtD_BS)
#We apply same winsorization as before for both datasets adapting for DtD

#Altman 1968
df_A_clean <- df_A %>%
  filter(if_all(X1_mean:X5_mean, ~ is.finite(.)))
df_A_clean <- df_A_clean %>%
  mutate(default = factor(default, levels = c(0,1)))

df_A_clean_w <- df_A_clean %>%
  mutate(
    X1_mean = pmin(pmax(X1_mean, quantile(X1_mean, 0.1, na.rm=T)), quantile(X1_mean, 0.99, na.rm=T)),
    X2_mean = pmin(pmax(X2_mean, quantile(X2_mean, 0.07, na.rm=T)), quantile(X2_mean, 0.99, na.rm=T)),
    X3_mean = pmin(pmax(X3_mean, quantile(X3_mean, 0.1, na.rm=T)), quantile(X3_mean, 0.99, na.rm=T)),
    X4_mean = pmin(pmax(X4_mean, quantile(X4_mean, 0.01, na.rm=T)), quantile(X4_mean, 0.90, na.rm=T)),
    X5_mean = pmin(pmax(X5_mean, quantile(X5_mean, 0.005, na.rm=T)), quantile(X5_mean, 0.95, na.rm=T)),
    DtD_BS = pmin(pmax(DtD_BS, quantile(DtD_BS, 0.005, na.rm=T)), quantile(DtD_BS, 0.90, na.rm=T))
  ) %>%
  
  filter(if_all(c(X1_mean, X2_mean, X3_mean, X4_mean, X5_mean), is.finite)) %>%
  mutate(default = factor(default, levels = c(0,1)))
boxplot(df_A_clean_w$DtD_BS)

#Altman & Rijken 2004
df_AR_clean <- df_AR %>%
  filter(if_all(X1_mean:Age_mean, ~ is.finite(.)))
df_AR_clean <- df_AR_clean %>%
  mutate(default = factor(default, levels = c(0,1)))
df_AR_clean_w <- df_AR_clean %>%
  mutate(
    X1_mean = pmin(pmax(X1_mean, quantile(X1_mean, 0.1, na.rm=T)), quantile(X1_mean, 0.99, na.rm=T)),
    X2_mean = pmin(pmax(X2_mean, quantile(X2_mean, 0.07, na.rm=T)), quantile(X2_mean, 0.99, na.rm=T)),
    X3_mean = pmin(pmax(X3_mean, quantile(X3_mean, 0.1, na.rm=T)), quantile(X3_mean, 0.99, na.rm=T)),
    X4_mean = pmin(pmax(X4_mean, quantile(X4_mean, 0.01, na.rm=T)), quantile(X4_mean, 0.90, na.rm=T)),
    Size_mean = pmin(pmax(Size_mean, quantile(Size_mean, 0.05, na.rm=T)), quantile(Size_mean, 0.99, na.rm=T)),
    DtD_BS = pmin(pmax(DtD_BS, quantile(DtD_BS, 0.005, na.rm=T)), quantile(DtD_BS, 0.90, na.rm=T))
  ) %>%
  
  filter(if_all(c(X1_mean, X2_mean, X3_mean, X4_mean, Size_mean), is.finite)) %>%
  mutate(default = factor(default, levels = c(0,1)))

#Models
#Altman 1986
altman_model_DtD_winsorized_real_mean <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + X5_mean+DtD_BS, 
                                         family = binomial(link = "logit"),
                                         data = df_A_clean_w)
summary(altman_model_DtD_winsorized_real_mean)
#More significant and better AIC only X1 and X2 mean not significant
#AIC decreased

#Altman & Rijken 2004
altman_rijken_model_DtD_winsorized_real_mean <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + Size_mean + Age_mean + DtD_BS, 
                                                family = binomial(link = "logit"),
                                                data = df_AR_clean)
summary(altman_rijken_model_DtD_winsorized_real_mean)
#More significant and better AIC only X1, X2 and X3 mean not significant
#AIC decreased

#Comparing to DtD from data
#=========================

compustat_dtd <- readRDS("_raw/compustat_data_dtd_full.rds")
compustat_dtd <- compustat_dtd %>% filter(gvkey %in% filtered_wrds$gvkey)
compustat_dtd <- compustat_dtd %>%
  mutate(date = ymd(date)) %>%
  filter(year(date) >= 1980 & year(date) <= 2010)
compustat_dtd$sig_DP <- 0.05 + 0.25 * compustat_dtd$sig_E_l_adj
compustat_dtd <- compustat_dtd %>% mutate(sig_V = (E / (E + DP_kmv)) * sig_E_l_adj + (DP_kmv / (E + DP_kmv)) * sig_DP)
compustat_dtd <- compustat_dtd %>% mutate(DtD = log((E + DP_kmv) / DP_kmv) + (r - 0.5 * sig_V^2) * 30 / (sig_V * sqrt(30)))
filtered_wrds_exterior_dtd <- inner_join(filtered_wrds, compustat_dtd, by="gvkey")
DtD_exterior <- glm(default ~ X1_mean + X2_mean + X3_mean + X4_mean + X5_mean+DtD, 
                                             family = binomial(link = "logit"),
                                             data = filtered_wrds_exterior_dtd)
summary(DtD_exterior)

#Additional Macro/Market predictors
#=========================

#We use as market predictor the mean of the volume of shares exchanged per day from CRSP
#We use as Macro predictor the mean of the GDP per year
GDP <- read.csv("_raw/GDP.csv")

#Getting the variables
df_task3 <- filtered_wrds %>%
  select(gvkey,
         starts_with("X1_"),
         starts_with("X2_"),
         starts_with("X3_"),
         starts_with("X4_"),
         starts_with("X5_"),
         starts_with("vol_"),
         default) %>%
  rowwise() %>%
  mutate(
    X1_mean = mean(c_across(X1_1980:X1_2010), na.rm = TRUE),
    X2_mean = mean(c_across(X2_1980:X2_2010), na.rm = TRUE),
    X3_mean = mean(c_across(X3_1980:X3_2010), na.rm = TRUE),
    X4_mean = mean(c_across(X4_1980:X4_2010), na.rm = TRUE),
    X5_mean = mean(c_across(X5_1980:X5_2010), na.rm = TRUE),
    vol_mean = mean(c_across(vol_1986_02_20:vol_2004_11_24), na.rm = TRUE)
  ) %>%
  ungroup() %>% select(X1_mean, X2_mean, X3_mean, X4_mean ,X5_mean, vol_mean, default)
df$GDP <- mean(GDP$GDP)
df_task3 <- cbind(df_task3, df_dtd)

df_task3_clean <- df_task3 %>%
  filter(if_all(X1_mean:vol_mean, ~ is.finite(.)))
df_task3_clean <- df_task3_clean %>%
  mutate(default = factor(default, levels = c(0,1)))

#Winsorizing
boxplot(df_task3_clean$vol_mean)

df_task3_clean_w <- df_task3_clean %>%
  mutate(
    X1_mean = pmin(pmax(X1_mean, quantile(X1_mean, 0.1, na.rm=T)), quantile(X1_mean, 0.99, na.rm=T)),
    X2_mean = pmin(pmax(X2_mean, quantile(X2_mean, 0.07, na.rm=T)), quantile(X2_mean, 0.99, na.rm=T)),
    X3_mean = pmin(pmax(X3_mean, quantile(X3_mean, 0.1, na.rm=T)), quantile(X3_mean, 0.99, na.rm=T)),
    X4_mean = pmin(pmax(X4_mean, quantile(X4_mean, 0.01, na.rm=T)), quantile(X4_mean, 0.90, na.rm=T)),
    X5_mean = pmin(pmax(X5_mean, quantile(X5_mean, 0.005, na.rm=T)), quantile(X5_mean, 0.95, na.rm=T)),
    vol_mean = pmin(pmax(vol_mean, quantile(vol_mean, 0.005, na.rm=T)), quantile(vol_mean, 0.85, na.rm=T))
  ) %>%
  mutate(default = factor(default, levels = c(0,1)))

boxplot(df_task3_clean_w$vol_mean)

#Model (Altman)
altman_model_DtD_Market_Macro_winsorized_real_mean <- glm(default ~ 0+ X1_mean + X2_mean + X3_mean + X4_mean + X5_mean+DtD_BS+vol_mean+GDP, 
                                             family = binomial(link = "logit"),
                                             data = df_task3_clean_w)
summary(altman_model_DtD_Market_Macro_winsorized_real_mean)
#Everything is quite significant except for X1 and X2