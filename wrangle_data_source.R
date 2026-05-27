library(arrow)

# Read CSV
df <- read.csv("t_measure_data_source.csv", header = FALSE)

colnames(df)[1:9] = c("release_year", "measure_id", "statecode", "countycode", 
                 "data_system", "years", "data_source", "data_source_desciption", 
                 "data_source_description2") 


# Find first column in each row that starts with "http"
df$data_source_link <- apply(df, 1, function(row) {
  
  # Find values starting with http
  links <- row[grepl("^http", row)]
  
  # Return first match or NA
  if (length(links) > 0) {
    links[1]
  } else {
    NA
  }
})

library(tidyverse)
dff = df %>% 
  filter(statecode == 0 & countycode == 0) %>% 
  
  
  mutate(
    data_system_clean = if_else(
      str_detect(data_system, regex("see note", ignore_case = TRUE)),
      NA_character_,
      data_system
    ),
    
    data_source_clean = if_else(
      str_detect(data_source, regex("see note", ignore_case = TRUE)),
      NA_character_,
      data_source
    )
  )  %>% 
  
  mutate(
    data_source_system = case_when(
      !is.na(data_system_clean) & data_system_clean != "" &
        !is.na(data_source_clean) & data_source_clean != "" ~ 
        paste0(data_system_clean, ": ", data_source_clean),
      
      !is.na(data_system_clean) & data_system_clean != "" ~ data_system_clean,
      
      !is.na(data_source_clean) & data_source_clean != "" ~ data_source_clean,
      
      TRUE ~ NA_character_
    ) %>%
      (\(x) if_else(
        !is.na(x),
        paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))),
        NA_character_
      ))()
  ) %>% 
  
  select(
    release_year,
    measure_id,
    years,
    data_source_system,
    data_source_link
  )

# Write Parquet
write_parquet(dff, "parquet/t_measure_data_source.parquet")
