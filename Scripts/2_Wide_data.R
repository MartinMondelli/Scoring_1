#==================================================================
#Objectives: 

#Generate a wide version of both datasets (CRSP and Compustat)
#Generate a full dataset containing the merge of CRSP and Compustat by gvkey and permno

#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#libraries
library(tidyverse)
library(lubridate)

#Datasets
compustat_cleaned_only <- readRDS("compustat_cleaned_only.rds")
crsp_cleaned_only <- readRDS("crsp_cleaned_only.rds")
link <- readRDS("ccmxpf_linktable.rds")

#First we pivot wider the two datasets

#CRSP
crsp_wide <- crsp_cleaned_only %>%
  select(date, permno, vol, shrout, prc, cap, ret) %>%
  pivot_wider(
    names_from = date,
    values_from = c(vol, shrout, prc, cap, ret),
    names_glue = "{.value}_{date}"
  )
names(full_wrds) <- gsub("-", "_", names(full_wrds))
saveRDS(crsp_wide, file = "crsp_cleaned_wide.rds")

#Compustat
compustat_wide <- compustat_cleaned_only %>%
  distinct() %>%
  select(-datadate) %>%
  group_by(gvkey, fyear) %>%
  summarise(
    across(everything(), ~ first(.x)),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    names_from = fyear,
    values_from = setdiff(names(.), c("gvkey", "fyear")),
    names_glue = "{.value}_{fyear}"
  )
saveRDS(compustat_wide, file = "compustat_cleaned_wide.rds")

#Merges

#First we merge:
#Compustat - link
#Important: we need to consider that in link there are some gvkey that are continuous but divided by chunks
#Example: gvkey       linkdt    linkenddt
#        000001   11/12/2000   12/04/2008
#        000001   13/04/2008     1/1/2011

merge1 <- inner_join(link,compustat_wide,by="gvkey") %>% distinct()
merged_compustat_link <- merge1 %>%
  arrange(permno, gvkey, linkdt) %>%
  group_by(permno, gvkey) %>%
  mutate(
    continuous = (linkdt <= lag(linkenddt) + days(1))
  ) %>%
  mutate(
    group_id = cumsum(ifelse(is.na(continuous) | !continuous, 1, 0))
  ) %>%
  group_by(permno, gvkey, group_id) %>%
  summarise(
    linkdt    = min(linkdt, na.rm = TRUE),
    linkenddt = max(linkenddt, na.rm = TRUE),
    across(
      .cols = everything(),           # toma TODAS las columnas que existan
      .fns  = ~ if (all(is.na(.x))) NA else .x[!is.na(.x)][1]
    ),
    
    .groups = "drop"
  ) %>%
  select(-group_id)

#Cleaning
merged_compustat_link <- merged_compustat_link %>%
  arrange(gvkey, permno, desc(linkdt), linkenddt) %>%   # prioriza links más recientes
  distinct(gvkey, permno, .keep_all = TRUE)           # ¡¡MANTIENE TODAS LAS COLUMNAS!!
merged_compustat_link$group_id <- NULL
merged_compustat_link$linkdt <- NULL
merged_compustat_link$linkenddt <- NULL
saveRDS(merged_compustat_link, file = "merged_compustat_link.rds")

#CRSP - Compustat
#We use the code provided for many-to-many in the lecture

ccm_links <- crsp_cleaned_only |>
  inner_join(link, 
             join_by(permno), relationship = "many-to-many") |>
  filter(!is.na(gvkey) & 
           (date >= linkdt & date <= linkenddt)) |>
  select(permno, gvkey, date)
saveRDS(ccm_links, file = "ccm_links.rds")
ccm_links <- ccm_links %>% distinct(gvkey, permno)
merge2 <- inner_join(ccm_links, crsp_wide, by="permno")

#Final merge
final_merge <- inner_join(merged_compustat_link, merge2, by=c("gvkey","permno"))
final_merge <- final_merge %>% 
  rename_with(~ str_replace_all(., "-", "_"))
saveRDS(final_merge, file = "final_merge.rds")

#############
#Extra      #
#############


#Transform CRSP to get only companies that are airlines and transport passangers
#Get permno number
airlines_permno <- link %>%
  filter(gvkey %in% compustat_passangers$gvkey) %>%
  select(permno, gvkey) %>%
  distinct()

#Convert to numeric if necessary
airlines_permno_vector <- as.numeric(airlines_permno$permno)

crsp_passangers <- crsp_cleaned_only %>%
  filter(permno %in% airlines_permno_vector) %>%
  mutate(permno = as.numeric(permno))
saveRDS(crsp_passangers, file = "Airlines/crsp_passangers.rds")
