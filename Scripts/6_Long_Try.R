



#From var1_AAAA to var1_AAAA_MM_DD
#=================================

annual_cols <- names(filtered_wrds)[str_detect(names(filtered_wrds), "_\\d{4}$")]
filtered_wrds <- filtered_wrds %>%
  rename_with(
    .cols = all_of(annual_cols),
    .fn = ~ str_replace(., "_(\\d{4})$", "_\\1_01_01")
  )

#Pivot long
#=================================

df_long <- filtered_wrds %>%
  pivot_longer(
    cols = matches("_.{4}_\\d{2}_\\d{2}$"),           # Selecciona columnas con _YYYY_MM_DD al final
    names_to = c(".value", "fecha"),                  # .value = nombre de variable, fecha = fecha
    names_pattern = "^(.+)_(\\d{4}_\\d{2}_\\d{2})$",   # Captura: (nombre_var) + _ + (fecha)
    names_transform = list(fecha = ymd)               # Convierte "1980_01_01" → Date
  ) %>%
  mutate(year = year(fecha)) %>%                      # Extrae el año
  select(gvkey, permno, year, default, fecha, everything()) %>%
  arrange(gvkey, permno, fecha)
colnames(df_long)[colnames(df_long) == "fecha"] <- "date"

rm(df_long, filtered_wrds, annual_cols, colnames)
