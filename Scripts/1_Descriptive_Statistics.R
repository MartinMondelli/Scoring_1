#==================================================================
#Objectives: 

#Generate descriptive statistics for both datasets (CRSP and Compustat)

#==================================================================
#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(ggplot2)
library(dplyr)
library(purrr)

#Datasets
compustat_cleaned_only <- readRDS("compustat_cleaned_only.rds")
crsp_cleaned_only <- readRDS("crsp_cleaned_only.rds")
company <- readRDS("_raw/company_all.rds")
link <- readRDS("ccmxpf_linktable.rds")

#Generating variables
#Market capitalization in millions
crsp_cleaned_only <- crsp_cleaned_only |>
  mutate(
    mktcap = shrout * prc / 10^6,
    mktcap = na_if(mktcap, 0)
  )

#One month lagged market capitalization
mktcap_lag <- crsp_cleaned_only |>
  mutate(date = date %m+% months(1)) |>
  select(permno, date, mktcap_lag = mktcap)

crsp_cleaned_only <- crsp_cleaned_only |>
  left_join(mktcap_lag, join_by(permno, date))

#Now we focus only on Scheduled Passangers Air Transportation: NAIC code 481111 that are still active
#Total of 119 companies
list_gvkey <- company %>% filter(naics == 481111) %>% select(conm, gvkey, costat, dldte, ipodate)
compustat_passangers <- compustat_cleaned_only %>% filter(gvkey %in% list_gvkey$gvkey)
saveRDS(compustat_passangers, file = "Airlines/compustat_passangers.rds")

#American Airlines, PanAm, United Airlines
airlines_gvkeys <- c("001045", "007672", "010795")
airlines_names <- c(
  "001045" = "American Airlines",
  "007672" = "NORTHWEST AIRLINES CORP",
  "010795" = "United Airlines"
)

#Compustat
#Relevant variables
vars <- c(
  "wcap", "re", "ebit", "lt", "sale", "at", "mkvalt",
  "ni", "oancf", "che", "act", "lct", "dltt", "xint", "ceq",
  "log_assets", "leverage", "liquidity",
  "cash_to_assets", "profitability_net", "cash_flow"
)

#Function to generate plots per variable
plot_compustat_var <- function(var) {
airline_data <- compustat_passangers %>%
    filter(gvkey %in% airlines_gvkeys) %>%
    group_by(fyear, gvkey) %>%
    summarise(value = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    mutate(company = airlines_names[gvkey])
  
  ggplot(airline_data, aes(x = fyear, y = value, color = company)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Evolution of", var, "per year"),
      x = "Year",
      y = var,
      color = "Company"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#Generate graphs
plots_compustat <- map(vars, plot_compustat_var)
plots_compustat

#CRSP
#Comparing vol, shrout, prc, cap, ret and mktcap in selected companies

#Get permno number
airlines_permno <- link %>%
  filter(gvkey %in% airlines_gvkeys) %>%
  select(permno, gvkey) %>%
  distinct()

#Convert to numeric if necessary
airlines_permno_vector <- as.numeric(airlines_permno$permno)

#Filter CRSP for these companies
crsp_passangers <- crsp_cleaned_only %>%
  filter(permno %in% airlines_permno_vector) %>%
  mutate(permno = as.numeric(permno))

#Assign company names
airlines_crsp <- crsp_passangers %>%
  mutate(company = case_when(
    permno == 21020 ~ "American Airlines",
    permno %in% c(21485,80345,92013)~ "NORTHWEST AIRLINES CORP",
    permno %in% c(19596, 91103) ~ "United Airlines",
    TRUE ~ "Other"
  ))

#Variables and titles
vars <- c("vol", "shrout", "prc", "cap", "ret", "mktcap")
titles <- c(
  vol = "Total volume per airline",
  shrout = "Number of shares circulating per company",
  prc = "Price per share per company",
  cap = "Market capitalization (prc*shrout) per company",
  ret = "Returns per company",
  mktcap = "Adjusted Market Capitalization per company"
)

#Funtion to do plots
plot_metric <- function(var) {
  airlines_crsp %>%
    group_by(date, company) %>%
    summarise(value = sum(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = date, y = value, color = company)) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    labs(
      title = titles[[var]],
      x = "Date", y = var, color = "Airline"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

#Generate plots
plots <- map(vars, plot_metric)
plots
