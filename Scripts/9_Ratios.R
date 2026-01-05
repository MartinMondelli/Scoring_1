#==================================================================
#Objectives: 

#Building new ratios

#Notes présentation:
#Penser aux metriques et résultats de la ROC curve, bizarre d'avoir un truc si grand
#probablement trop de 0, peu de pouvoir de prédiction, interprétation et +
#CAP accuracy curve, moodys
#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(tidyverse)
library(pROC)
library(ggplot2)

#Database
filtered_wrds_long_w_final_DtD_gdp_vol <- readRDS("filtered_wrds_long_w_final_DtD_gdp_vol.rds")
#37 variables
compustat <- readRDS("_raw/compustat_all.rds")

#Ratios
#================================

#Variables from compustat we will use:
#1- SALEAT: SALE/AT
#2- QUALT: (ACT-INVT)/LCT
#3- NIAT: NI/AT
#4- LCTSALE: LCT/SALE
#5- CHAT: CH/AT
#6- ACTLCT: ACT/LCT
#7- CHEAT: CHE/AT
#8- r1 = (DLTT+DLC)/AT
#9- r6 = (DLTT+DLC)/(REVT-COGS-xsga)
#10- r7 = DLTT/(REVT-COGS-xsga)
#11- r11 = WCAP/(REVT-COGS-xsga)
#12- r17 = XiNT/(REVT-COGS-xsga)
#13- r24 = OIBDP/AT
#14- r28 = OIBDP/(REVT-COGS-xsga)
#15- r36 = (PPENT+INTAN+GDWAL)/(REVT-COGS-xsga)
#16- Operating CashFlow Margin (CFM) = OANCF/REVT
#17- Capex Intensity = CAPX/AT
#18- Receivables Turnover Ratio = REVT/RECT
#19- Inventory to Sales Ratio = INVT/REVT
#20- Debt to EBITDA = (DLTT+DLC)/OIBDP

#Building the ratios
#================================

df <- compustat %>%
  mutate(
    SALEAT = sale/at,
    QUALT = (act-invt)/lct,
    NIAT = ni/at,
    LCTSALE = lct/sale,
    CHAT = ch/at,
    ACTLCT = act/lct,
    CHEAT = che/at,
    r1 = (dltt+dlc)/at,
    r6 = (dltt+dlc)/(revt-cogs-xsga),
    r7 = dltt/(revt-cogs-xsga),
    r11 = wcap/(revt-cogs-xsga),
    r17 = xint/(revt-cogs-xsga),
    r24 = oibdp/at,
    r28 = oibdp/(revt-cogs-xsga),
    r36 = (ppent+intan+gdwl)/(revt-cogs-xsga),
    Operating_CFM = oancf/revt,
    Capex_At = capx/at,
    RTO_r = revt/rect,
    ITS_r = invt/revt,
    debt_EBITDA = (dltt+dlc)/oibdp
  ) %>%
  select(SALEAT,QUALT, NIAT,LCTSALE,CHAT,ACTLCT,CHEAT,r1,r6,r7,r11,r17,r24,r28,r36,Operating_CFM, Capex_At, RTO_r, ITS_r,debt_EBITDA, gvkey, fyear)
rm(compustat)

filtered_wrds_long_w_final_DtD_gdp_vol_ratios <- left_join(filtered_wrds_long_w_final_DtD_gdp_vol, df)

filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r <- ifelse(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r == Inf, NA, filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r)
rm(filtered_wrds_long_w_final_DtD_gdp_vol)
rm(df)

#Descriptive statistics and distribution
#================================

vars <- c("SALEAT","QUALT","NIAT","LCTSALE","CHAT","ACTLCT","CHEAT",
          "r1","r6","r7","r11","r17","r24","r28","r36",
          "Operating_CFM","Capex_At","RTO_r","ITS_r","debt_EBITDA")

for (v in vars) {
  cat("\n=== ", v, " ===\n")
  filtered_wrds_long_w_final_DtD_gdp_vol_ratios %>%
    summarise(across(all_of(v), summary)) %>%
    print()
}

#Plots
#================================

df_avg <- filtered_wrds_long_w_final_DtD_gdp_vol_ratios %>%
  group_by(gvkey, fyear) %>%
  summarise(across(all_of(vars), mean, na.rm = TRUE), .groups = "drop")
df_year <- df_avg %>%
  group_by(fyear) %>%
  summarise(across(all_of(vars), mean, na.rm = TRUE))
df_long <- df_year %>%
  pivot_longer(cols = all_of(vars),
               names_to = "variable",
               values_to = "value")
ggplot(df_long, aes(x = fyear, y = value)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Annual mean per company",
       x = "Year",
       y = "Mean value per year")
rm(df_avg, df_year, df_long, v, vars)

#Outliers
#================================

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$SALEAT)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$SALEAT <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$SALEAT, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$SALEAT, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$SALEAT, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$QUALT)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$QUALT <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$QUALT, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$QUALT, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$QUALT, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$NIAT)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$NIAT <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$NIAT, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$NIAT, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$NIAT, 0.9, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$LCTSALE)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$LCTSALE <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$LCTSALE, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$LCTSALE, 0.01, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$LCTSALE, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHAT)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHAT <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHAT, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHAT, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHAT, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ACTLCT)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ACTLCT <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ACTLCT, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ACTLCT, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ACTLCT, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHEAT)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHEAT <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHEAT, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHEAT, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$CHEAT, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r1)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r1 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r1, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r1, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r1, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r6)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r6 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r6, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r6, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r6, 0.9, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r7)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r7 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r7, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r7, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r7, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r11)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r11 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r11, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r11, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r11, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r17)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r17 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r17, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r17, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r17, 0.9, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r24)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r24 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r24, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r24, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r24, 0.95, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r28)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r28 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r28, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r28, 0.05, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r28, 0.95, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r36)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r36 <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r36, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r36, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$r36, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Operating_CFM)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Operating_CFM <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Operating_CFM, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Operating_CFM, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Operating_CFM, 0.9, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Capex_At)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Capex_At <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Capex_At, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Capex_At, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$Capex_At, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$RTO_r, 0.80, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ITS_r)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ITS_r <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ITS_r, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ITS_r, 0, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$ITS_r, 0.90, na.rm=T))

boxplot(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$debt_EBITDA)
filtered_wrds_long_w_final_DtD_gdp_vol_ratios$debt_EBITDA <- pmin(pmax(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$debt_EBITDA, quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$debt_EBITDA, 0.1, na.rm=T)), quantile(filtered_wrds_long_w_final_DtD_gdp_vol_ratios$debt_EBITDA, 0.9, na.rm=T))

#Save it
saveRDS(filtered_wrds_long_w_final_DtD_gdp_vol_ratios, "filtered_wrds_long_w_final_DtD_gdp_vol_ratios_w.rds")
