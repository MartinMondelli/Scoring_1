#==================================================================
#Objectives: 

#Generate the default variable
#Generate the final dataset that is comprised between 1980 and 2010

#==================================================================

#Set working directory
setwd("/Users/martinmondelli/Desktop/Scoring/Project/Data")

#Libraries
library(dplyr)

#Datasets
full_wrds <- readRDS("final_merge.rds")
LoPucki <- read_excel("_raw/Florida-UCLA-LoPucki Bankruptcy Research Database 1-12-2023.xlsx")

#We will keep only years from 1980 to 2010

#First we construct the variable default in LoPucki
LoPucki <- LoPucki %>%
  mutate(
    start_year = ifelse(!is.na(YearFiled), YearFiled - 10, 1900),
    end_year   = ifelse(!is.na(YearDisposed), YearDisposed, 2025),
    
    default = case_when(
      #Doesn't exist in 1980–2010
      end_year < 1980 | start_year > 2010 ~ NA_real_,
      
      #Exist in 1980–2010 and never defaulted
      (is.na(YearFiled) | YearFiled < 1980 | YearFiled > 2010) ~ 0,
  
      #Exist in 1980–2010 and defaulted between 1980–2018
      YearFiled >= 1980 & YearFiled <= 2010 ~1
    )
  )

#Now we want to translate this new variable default from LoPucki to full_wrds without doing a merge that would take too long
#We recover the gvkey from companies that did default and thowe who did not
default <- LoPucki %>% filter(default==1) %>%
  distinct(GvkeyBefore)
not_default <- LoPucki %>% filter(default==0) %>%
  distinct(GvkeyBefore)

#Now we translate this in full_wrds
full_wrds$default <- ifelse(full_wrds$gvkey %in% default$GvkeyBefore, 1, NA)
full_wrds$default <- ifelse(full_wrds$gvkey %in% not_default$GvkeyBefore, 0, full_wrds$default)
saveRDS(full_wrds, file = "full_wrds.rds")

#Now we only filter the years between 1980 and 2010 (arbitrary window)
years <- stringr::str_extract(names(full_wrds), "\\d{4}")
years <- as.numeric(years)
filtered_wrds <- full_wrds[, is.na(years) | (years >= 1980 & years <= 2010)]

#Filter columns that finish with NA
filtered_wrds <- filtered_wrds %>%
  select(-ends_with("_NA"))

filtered_wrds <- filtered_wrds %>%
  rename_with(~ str_replace_all(., "-", "_"))
saveRDS(filtered_wrds, file = "filtered_wrds.rds")

