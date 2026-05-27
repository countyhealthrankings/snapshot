files <- jsonlite::fromJSON(
  "https://api.github.com/repos/countyhealthrankings/county_health_measure_calculations/contents/calculations"
)

calc_index <- tibble(file = calc_files) %>%
  
  # remove sas files
  filter(!str_detect(file, "\\.sas$")) %>%
  
  # extract full prefix like "v002_009_143_145"
  mutate(
    id_block = str_extract(file, "^v[0-9_]+")
  ) %>%
  
  # extract ALL numbers inside that prefix block
  mutate(
    measure_ids = str_extract_all(id_block, "\\d+")
  ) %>%
  
  unnest(measure_ids) %>%
  
  mutate(
    measure_id = as.integer(measure_ids)
  ) %>%
  
  filter(!is.na(measure_id)) %>%
  
  # deterministic selection
  arrange(measure_id, file) %>%
  group_by(measure_id) %>%
  slice(1) %>%
  ungroup() %>%
  
  mutate(
    calculations_link = paste0(
      "https://github.com/countyhealthrankings/county_health_measure_calculations/blob/main/calculations/",
      file
    )
  ) %>%
  
  select(measure_id, calculations_link)

# Write Parquet
write_parquet(calc_index, "parquet/calculation_links.parquet")
