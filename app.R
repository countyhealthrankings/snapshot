# app.R —
# ----------------------------------------------------
# - Load relational county-health data files 
# - Let user pick a Year and County
# - Show a snapshot table
# - Provide users with the option to download the data as a csv
# - Include a placeholder "Latest" option that currently points to the newest year in the repo

suppressPackageStartupMessages({
  library(shiny)
  library(httr)
  library(jsonlite)
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(ggplot2)
  library(memoise)
  library(gt)
  library(digest)
  library(shiny.semantic)
  library(htmltools)
  library(arrow)
  library(duckdb)
  library(DBI)
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = ":memory:"
  )
})


#define default years so something shows before api call
available_years <- reactiveVal(c("2023", "2022"))

# build measure map (will be used alwasy)
cat_names <- arrow::read_parquet(
  "parquet/t_category.parquet"
)

fac_names <- arrow::read_parquet(
  "parquet/t_factor.parquet"
)

foc_names <- arrow::read_parquet(
  "parquet/t_focus_area.parquet"
)

mea_years <- arrow::read_parquet(
  "parquet/t_measure_years.parquet"
) %>%
  select(
    year,
    measure_id,
    years_used
  )

mea_compare <- arrow::read_parquet(
  "parquet/t_measure.parquet"
)

measure_sources <- arrow::read_parquet(
  "parquet/t_measure_data_source.parquet"
)

measure_links = arrow::read_parquet(
  "parquet/calculation_links.parquet"
)

measure_map_all <- mea_years %>%
  
  full_join(
    mea_compare,
    by = c("measure_id", "year")
  ) %>%
  
  left_join(measure_links, by = "measure_id") %>%
  mutate(
    calculations_link = if_else(
      year == max(year, na.rm = TRUE),
      calculations_link,
      NA_character_
    )
  ) %>% 
  
  left_join(
    foc_names,
    by = c(
      "measure_parent" = "focus_area_id",
      "year"
    )
  ) %>%
  
  left_join(
    fac_names,
    by = c(
      "focus_area_parent" = "factor_id",
      "year"
    )
  ) %>%
  
  left_join(
    cat_names,
    by = c(
      "factor_parent" = "category_id",
      "year"
    )
  ) %>%
  
  left_join(
    measure_sources,
    by = c(
      "measure_id" = "measure_id",
      "year" = "release_year"
    )
  ) %>%
  
  mutate(
    data_source_system_link = case_when(
      
      !is.na(data_source_link) &
        data_source_link != "" &
        !is.na(data_source_system) &
        data_source_system != "" ~
        
        paste0(
          "<a href='",
          data_source_link,
          "' target='_blank'>",
          data_source_system,
          "</a>"
        ),
      
      TRUE ~ data_source_system
    )
  ) %>%
  
  mutate(
    
    compare_years_text = case_when(
      compare_years == -1 ~ "Comparability across release years is unknown",
      compare_years ==  0 ~ "Not comparable across release years",
      compare_years ==  1 ~ "Comparable across release years",
      compare_years ==  2 ~ "Use caution when comparing across release years",
      TRUE ~ ""
    ),
    
    compare_states_text = case_when(
      compare_states == -1 ~ "Comparability across states is unknown",
      compare_states ==  0 ~ "Not comparable across states",
      compare_states ==  1 ~ "Comparable across states",
      compare_states ==  2 ~ "Use caution when comparing across states",
      TRUE ~ ""
    )
    
  ) %>%
  
  select(
    year,
    measure_id,
    measure_name,
    description,
    years_used,
    compare_years_text,
    compare_states_text,
    factor_name,
    focus_area_name,
    category_name,
    direction,
    display_precision,
    format_type,
    data_source_system_link,
    calculations_link
  )


# ---- Load county list ----
get_county_list <- function() {
  readRDS("data/county_fips.rds")
}

# add the full state name in
state_lookup <- data.frame(
  state = state.abb,
  state_name = state.name,
  stringsAsFactors = FALSE
)
#need DC too
state_lookup <- rbind(
  state_lookup,
  data.frame(
    state = "DC",
    state_name = "Washington, D.C.",
    stringsAsFactors = FALSE
  )
)

county_choices <- get_county_list() %>%
  dplyr::left_join(state_lookup, by = "state")


state_choices <- county_choices %>%
  dplyr::distinct(state, state_name) %>%
  dplyr::arrange(state_name)


##################################################################################
# ---- UI ----
ui <- semanticPage(
  
  #need this for styling 
  tags$head(
    
    # Google font
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
    ),
    
    # Your CSS file
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "custom.css"
    )
    
    ,
    
    # favicon
    tags$link(
      rel = "shortcut icon",
      href = "favicon.ico"
    )
    
  ),
   
  title = "Health Snapshot",
  div(
    class = "ui warning message",
    div(
      class = "content",
      div(class = "header", "This tool is under development"),
      HTML(
        paste0(
          "For official county-level data, please visit the ",
          "<a href='https://www.countyhealthrankings.org/health-data' target='_blank'>",
          "County Health Rankings &amp; Roadmaps website</a>",
          " which will remain available through December 2026.<br><br>",
          "We appreciate your help troubleshooting this tool. ", 
          "If you encounter any bugs or have an idea for an improvement, please ", 
          "<a href='https://github.com/County-Health-Rankings-and-Roadmaps/snapshot/issues'target='blank'>",
          "open an issue on the app's GitHub repo</a>. You can also view planned changes and known bugs there. Thank you!"
        )
      )
    )
  ),

  # Grid layout: left panel (filters) + right panel (main content)
  
  div(
    class = "page-wrapper",
    
    h2(class = "ui header", "Health Snapshot"),
      
      uiOutput("location_header_ui"),
      
      
      div(
        class = "download-buttons",
        br(),
        uiOutput("download_data_ui"),
        br(),
        uiOutput("download_all_counties_ui")
      ),
      
      
    ),
  
  div(
    class = "ui stackable grid",

    # --- Sidebar ---
    div(
      class = "four wide column",
      div(
        class = "ui raised segment",
        h4(class = "ui header", ""),

        selectInput(
          inputId = "state",
          label = "Select State:",
          choices = setNames(
            state_choices$state, #this is what input$state is defined as
            state_choices$state_name
          ), # this is waht the user selects
          selected = state_choices$state[1]
        ),
        br(),
        uiOutput("county_ui"),
        br(),
        uiOutput("year_ui"),
        br(),
        br(),
        helpText(HTML(
          "
                    <b>Legend:</b> <br>
                    ✅ These data can be compared across states<br>
                    ❌ These data are incomparable across states<br>
                    ⚠️ Use caution if comparing these data across states")),
      )
    ),

    # --- Main Panel ---
    div(
      class = "twelve wide column",
      
      div(
        class = "ui segment",
        
       
        # Tables / accordion below
        uiOutput("category_tables_ui")
      )
    )
  )
)


#############################################################################
# ---- Server ----
server <- function(input, output, session) {
  # ---- Compute available years from mea_names ----
  #available_years <- reactive({
  #  req(mea_names)  # make sure the data is loaded
  #  mea_names %>%
  #    pull(year) %>%       # extract the year column
  #    unique() %>%         # keep unique values
  #    sort(decreasing = TRUE)  # newest first
  #})
  # provide default years immediately, gets updated later 
  available_years <- reactiveVal(c("2023", "2022"))

  # reactive to get full state name
  state_full <- reactive({
    req(input$state)
    full_name <- state_lookup[state_lookup$state == input$state, ]$state_name
    if (length(full_name) == 0) {
      full_name <- input$state
    }
    full_name
  })

  observe({
    parquet_files <- list.files(
      "parquet",
      pattern = "\\.parquet$",
      full.names = FALSE
    )
    
    yrs <- parquet_files %>%
      stringr::str_extract("\\d{4}") %>%
      unique() %>%
      na.omit() %>%
      as.numeric() %>%
      sort(decreasing = TRUE)
  
    if (length(yrs) == 0) {
      yrs <- c("2023", "2022")
    }
    available_years(yrs)
  })

  output$year_ui <- renderUI({
    yrs <- available_years()

    req(length(yrs) > 0)

    # Make sure they're sorted numerically (descending, newest first)
    yrs_num <- sort(as.numeric(yrs), decreasing = TRUE)
    # Remove the maximum year (will be represented by "Latest")
    yrs_no_latest <- yrs_num[yrs_num != max(yrs_num)]
    selectInput(
      "year",
      "Release year",
      choices = c("Latest" = "Latest", setNames(yrs_no_latest, yrs_no_latest)),
      selected = "Latest"
    )
  })

  # Dynamically update county dropdown
  output$county_ui <- renderUI({
    req(input$state)
    counties_in_state <- county_choices %>%
      filter(state == input$state)

    if (nrow(counties_in_state) == 0) {
      return(selectInput(
        "county",
        "Select County (optional: default is statewide):",
        choices = "Select a state first"
      ))
    }

    selectInput(
      "county",
      "Select County (optional; default is statewide):",
      #choices = setNames(counties_in_state$fipscode, counties_in_state$county),
      choices = c("Statewide", counties_in_state$county),
      #selected = counties_in_state$fipscode[1])
      selected = "Statewide"
    )
  })

  resolved_year <- reactive({
    if (identical(input$year, "Latest")) {
      yrs <- available_years()
      req(yrs)
      max(yrs)
    } else {
      input$year
    }
  })

  output$download_data_ui <- renderUI({
    req(
      nzchar(input$state),
      nzchar(input$county),
      nzchar(input$year)
    )
    # Use empty string if statewide
    county_label <- if (input$county == "Statewide") "" else input$county

    label <- paste0(
      "Download data for ",
      county_label,
      if (county_label != "") paste0(", ", input$state) else state_full(), # add comma only if county is not empty
      " (",
      resolved_year(),
      " )"
    )

    downloadButton(
      outputId = "download_data",
      label = label
    )
  })

  output$download_all_counties_ui <- renderUI({
    req(nzchar(input$year))

    downloadButton(
      "download_analytic_all_counties",
      paste0("Download data for all U.S. counties (", resolved_year(), ")")
    )
  })

  output$download_data <- downloadHandler(
    filename = function() {
      req(
        nzchar(input$state),
        nzchar(input$county),
        nzchar(input$year)
      )

      county_label <- if (input$county == "Statewide") "" else input$county

      paste0(
        input$state,
        if (county_label != "") paste0("_", county_label) else "",
        "_",
        resolved_year(),
        ".csv"
      )
    },
    content = function(file) {
      req(
        nzchar(input$state),
        nzchar(input$county),
        nzchar(input$year)
      )
      write.csv(measure_values, file, row.names = FALSE)
    }
  )

  output$download_analytic_all_counties <- downloadHandler(
    
    filename = function() {
      req(nzchar(input$year))
      paste0("all_US_counties_", resolved_year(), ".csv")
    },
    
    content = function(file) {
      
      req(nzchar(input$year))
      
      year <- resolved_year()
      
      f <- list.files("parquet", full.names = TRUE)
      
      f <- f[
        grepl("analytic", f, ignore.case = TRUE) &
          grepl(as.character(year), f)
      ]
      
      req(length(f) > 0)
      
      parquet_file <- f[1]
      
      df <- arrow::read_parquet(parquet_file)
      
      readr::write_csv(df, file)
      
    }
  )


  output$location_header_ui <- renderUI({
    # Resolve year
    year_text <- if (identical(input$year, "Latest")) {
      resolved_year()
    } else {
      input$year
    }

    # Safely handle county input
    county_label <- if (is.null(input$county) || input$county == "Statewide") {
      ""
    } else {
      input$county
    }

    # Build location text
    location_text <- if (nzchar(county_label)) {
      paste0(county_label, " County, ", state_full())
    } else {
      state_full()
    }

    h3(
      class = "ui header",
      paste(location_text, " ", year_text)
    )
  })

  county_df <- reactive({
    y = resolved_year()
    req(input$state, input$county)
    req(y)

    # If statewide, don't create county_df
    if (input$county == "Statewide") {
      return(NULL)
    }
    # find the fips codes for the chosen state + county
    chosen <- county_choices %>%
      filter(state == input$state & county == input$county) #note that the input county is the county name
    #chosen = county_choices %>% filter(state == "MN" & county == "Olmsted County")

    # Defensive check
    req(nrow(chosen) > 0)

    state_fips <- chosen$statecode[1]
    county_fips <- chosen$countycode[1]
    req(state_fips, county_fips)
    
    parquet_file <- sprintf(
      "parquet/t_measure_data_years_%s.parquet",
      y
    )
    
    query <- sprintf("
    SELECT
      measure_id,
      county_fips,
      state_fips,
      raw_value,
      ci_low,
      ci_high
    FROM read_parquet('%s')
    WHERE state_fips = '%s'
      AND county_fips = '%s'
  ",
                     parquet_file,
                     state_fips,
                     county_fips)
    
    dbGetQuery(con, query) %>%
      mutate(
        state_fips = stringr::str_pad(state_fips, 2, pad = "0"),
        county_fips = stringr::str_pad(county_fips, 3, pad = "0")
      ) %>%
      distinct(measure_id, state_fips, .keep_all = TRUE)
    
  })

  state_df <- reactive({
    req(input$state, input$year)
    y <- resolved_year()
    req(y)

    # find the fips codes for the chosen state + county
    chosen <- county_choices %>%
      filter(state == input$state)
    # Defensive check
    req(nrow(chosen) > 0) # ensures chosen is not empty

    #for quick n dirty testing
    #chosen = county_choices %>% filter(state == "MN" & county == "Olmsted County")

    input_state_fips <- chosen$statecode[1]
    req(input_state_fips)
    county_fips <- "000"
    
    
    parquet_file <- sprintf(
      "parquet/t_state_data_years_%s.parquet",
      y
    )
    
    
    query <- sprintf("
    SELECT
      measure_id,
      state_fips,
      raw_value AS stateval,
      ci_low AS state_ci_low,
      ci_high AS state_ci_high
    FROM read_parquet('%s')
    WHERE state_fips = '%s'
      AND county_fips = '000'
  ",
                     parquet_file,
                     input_state_fips)
    
    dbGetQuery(con, query) %>%
      distinct(measure_id, state_fips, .keep_all = TRUE) %>% 
      mutate(
        state_fips = stringr::str_pad(state_fips, 2, pad = "0")
      )
    

    
  })

  ntl_df <- reactive({
    req(input$year)
    y <- resolved_year()
    req(y)
    
    
    parquet_file <- sprintf(
      "parquet/t_state_data_years_%s.parquet",
      y
    )
    
    query <- sprintf("
    SELECT
      measure_id,
      state_fips,
      raw_value AS ntlval,
      ci_low AS ntl_ci_low,
      ci_high AS ntl_ci_high
    FROM read_parquet('%s')
    WHERE state_fips = '00'
  ",
                     parquet_file)
    
    dbGetQuery(con, query) %>% 
      mutate(
        state_fips = stringr::str_pad(state_fips, 2, pad = "0")
      )
    

    
  })

  measure_values_data <- reactive({
    req(input$year, input$state)
    y <- resolved_year()
    req(y)

    measure_map = mea_names <- mea_years %>%
      dplyr::full_join(
        mea_compare,
        by = c("measure_id", "year")
      )
    
    mea_names <- mea_names %>%
      dplyr::left_join(
        foc_names,
        by = c(
          "measure_parent" = "focus_area_id",
          "year"
        )
      ) %>%
      dplyr::left_join(
        fac_names,
        by = c(
          "focus_area_parent" = "factor_id",
          "year"
        )
      ) %>%
      dplyr::left_join(
        cat_names,
        by = c(
          "factor_parent" = "category_id",
          "year"
        )
      ) %>%
      dplyr::mutate(
        
        compare_years_text = dplyr::case_when(
          compare_years == -1 ~ "Comparability across release years is unknown",
          compare_years ==  0 ~ "Not comparable across release years",
          compare_years ==  1 ~ "Comparable across release years",
          compare_years ==  2 ~ "Use caution when comparing across release years",
          TRUE ~ ""
        ),
        
        compare_states_text = dplyr::case_when(
          compare_states == -1 ~ "Comparability across states is unknown",
          compare_states ==  0 ~ "Not comparable across states",
          compare_states ==  1 ~ "Comparable across states",
          compare_states ==  2 ~ "Use caution when comparing across states",
          TRUE ~ ""
        )
        
      ) %>%
      dplyr::select(
        year,
        measure_id,
        measure_name,
        description,
        years_used,
        compare_years_text,
        compare_states_text,
        factor_name,
        focus_area_name,
        category_name,
        direction,
        display_precision,
        format_type
      )
    
    
    # subset the measure mapping
    
    measure_map <- measure_map_all %>%
      
      filter(year == y) %>%
      
      mutate(
        calculations_link_html = case_when(
          !is.na(calculations_link) ~ paste0(
            "<a href='", calculations_link,
            "' target='_blank'>Code</a>"
          ),
          TRUE ~ NA_character_
        )
      ) %>%
      
      select(
        measure_id,
        measure_name,
        years_used,
        factor_name,
        category_name,
        display_precision,
        format_type,
        calculations_link,
        calculations_link_html,
        compare_states_text,
        compare_years_text,
        description,
        data_source_system_link
      )
    
    measure_map <- measure_map %>%
      mutate(
        calculations_link_html = case_when(
          !is.na(calculations_link) ~ paste0(
            "<a href='", calculations_link,
            "' target='_blank'>Code</a>"
          ),
          TRUE ~ NA_character_
        )
      )
     

    # Start with the correct base data
    if (input$county == "Statewide") {
      req(state_df(), ntl_df())
      measure_values <- state_df() %>%
        left_join(ntl_df() %>% select(-state_fips), by = "measure_id") %>%
        left_join(measure_map, by = "measure_id")
    } else {
      req(county_df(), state_df(), ntl_df())
      measure_values <- county_df() %>%
        left_join(state_df(), by = c("measure_id", "state_fips")) %>%
        left_join(ntl_df() %>% select(-state_fips), by = "measure_id", relationship = "many-to-many") %>%
        left_join(measure_map, by = "measure_id")
      
     # measure_values <- county_df %>%
    #   left_join(state_df, by = c("measure_id", "state_fips")) %>%
    #  left_join(ntl_df %>% select(-state_fips), by = "measure_id", relationship = "many-to-many") %>%
    #  left_join(measure_map, by = "measure_id")
    }

    # Apply formatting
    measure_values <- measure_values %>%
      mutate(
        # County value only if not statewide
        value_ci = if (input$county != "Statewide") {
          case_when(
            is.na(ci_low) | is.na(ci_high) ~ as.character(
              case_when(
                format_type == 0 ~ scales::number(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                format_type == 1 ~ scales::percent(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                format_type == 2 ~ scales::dollar(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                format_type == 3 ~ scales::number(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                )
              )
            ),
            TRUE ~ case_when(
              format_type == 0 ~ paste0(
                scales::number(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                "\n(",
                scales::number(ci_low, accuracy = 1 / (10^display_precision)),
                ", ",
                scales::number(ci_high, accuracy = 1 / (10^display_precision)),
                ")"
              ),
              format_type == 1 ~ paste0(
                scales::percent(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                "\n(",
                scales::percent(ci_low, accuracy = 1 / (10^display_precision)),
                ", ",
                scales::percent(ci_high, accuracy = 1 / (10^display_precision)),
                ")"
              ),
              format_type == 2 ~ paste0(
                scales::dollar(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                "\n(",
                scales::dollar(ci_low, accuracy = 1 / (10^display_precision)),
                ", ",
                scales::dollar(ci_high, accuracy = 1 / (10^display_precision)),
                ")"
              ),
              format_type == 3 ~ paste0(
                scales::number(
                  raw_value,
                  accuracy = 1 / (10^display_precision)
                ),
                "\n(",
                scales::number(ci_low, accuracy = 1 / (10^display_precision)),
                ", ",
                scales::number(ci_high, accuracy = 1 / (10^display_precision)),
                ")"
              )
            )
          )
        } else {
          NULL # No county values if statewide
        },

        # The rest of the formatting applies to all selections
        measure_display = paste0(measure_name, " (", years_used, ")"),

        stateval_fmt = case_when(
          is.na(state_ci_low) | is.na(state_ci_high) ~ as.character(
            case_when(
              format_type == 0 ~ scales::number(
                stateval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              ),
              format_type == 1 ~ scales::percent(
                stateval,
                accuracy = 1 / (10^display_precision)
              ),
              format_type == 2 ~ scales::dollar(
                stateval,
                accuracy = 1 / (10^display_precision)
              ),
              format_type == 3 ~ scales::number(
                stateval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              )
            )
          ),
          TRUE ~ case_when(
            format_type == 0 ~ paste0(
              scales::number(
                stateval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              ),
              "\n(",
              scales::number(
                state_ci_low,
                accuracy = 1 / (10^display_precision)
              ),
              ", ",
              scales::number(
                state_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            ),
            format_type == 1 ~ paste0(
              scales::percent(stateval, accuracy = 1 / (10^display_precision)),
              "\n(",
              scales::percent(
                state_ci_low,
                accuracy = 1 / (10^display_precision)
              ),
              ", ",
              scales::percent(
                state_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            ),
            format_type == 2 ~ paste0(
              scales::dollar(stateval, accuracy = 1 / (10^display_precision)),
              "\n(",
              scales::dollar(
                state_ci_low,
                accuracy = 1 / (10^display_precision)
              ),
              ", ",
              scales::dollar(
                state_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            ),
            format_type == 3 ~ paste0(
              scales::number(
                stateval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              ),
              "\n(",
              scales::number(
                state_ci_low,
                accuracy = 1 / (10^display_precision)
              ),
              ", ",
              scales::number(
                state_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            )
          )
        ),

        ntlval_fmt = case_when(
          is.na(ntl_ci_low) | is.na(ntl_ci_high) ~ as.character(
            case_when(
              format_type == 0 ~ scales::number(
                ntlval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              ),
              format_type == 1 ~ scales::percent(
                ntlval,
                accuracy = 1 / (10^display_precision)
              ),
              format_type == 2 ~ scales::dollar(
                ntlval,
                accuracy = 1 / (10^display_precision)
              ),
              format_type == 3 ~ scales::number(
                ntlval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              )
            )
          ),
          TRUE ~ case_when(
            format_type == 0 ~ paste0(
              scales::number(
                ntlval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              ),
              "\n(",
              scales::number(ntl_ci_low, accuracy = 1 / (10^display_precision)),
              ", ",
              scales::number(
                ntl_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            ),
            format_type == 1 ~ paste0(
              scales::percent(ntlval, accuracy = 1 / (10^display_precision)),
              "\n(",
              scales::percent(
                ntl_ci_low,
                accuracy = 1 / (10^display_precision)
              ),
              ", ",
              scales::percent(
                ntl_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            ),
            format_type == 2 ~ paste0(
              scales::dollar(ntlval, accuracy = 1 / (10^display_precision)),
              "\n(",
              scales::dollar(ntl_ci_low, accuracy = 1 / (10^display_precision)),
              ", ",
              scales::dollar(
                ntl_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            ),
            format_type == 3 ~ paste0(
              scales::number(
                ntlval,
                accuracy = 1 / (10^display_precision),
                big.mark = ","
              ),
              "\n(",
              scales::number(ntl_ci_low, accuracy = 1 / (10^display_precision)),
              ", ",
              scales::number(
                ntl_ci_high,
                accuracy = 1 / (10^display_precision)
              ),
              ")"
            )
          )
        ),

       # compare_years_text = case_when(
      #    compare_years == -1 ~ "Comparability across years is unknown",
      #    compare_years == 0 ~ "Not comparable across years",
      #    compare_years == 1 ~ "Comparable across years",
      #    compare_years == 2 ~ "Use caution when comparing across years",
      #    TRUE ~ ""
      #  ),

        years_used_display = paste0(years_used, ": ", compare_years_text),

        compare_states_sym = case_when(
          compare_states_text == "Comparability across states is unknown" ~ "❓",
          compare_states_text == "Not comparable across states" ~ "❌",
          compare_states_text== "Comparable across states" ~ "✅",
          compare_states_text == "Use caution when comparing across states" ~ "⚠️",
          TRUE ~ ""
        ),

      measure_label = paste0(
        
        case_when(
          
          !is.na(calculations_link) ~ paste0(
            "<a href='",
            calculations_link,
            "' target='_blank'>",
            "**",
            measure_name,
            "***",
            "</a>"
          ),
          
          TRUE ~ paste0(
            "**",
            measure_name,
            "**"
          )
          
        ),
        
        ": ",
        description,
        "<br>",
        compare_states_sym
      )
      )

    measure_values
  })

  download_data = reactive({
    measure_values_data() %>%
      select(
        state_fips,
        any_of("county_fips"),
        measure_id,
        measure_name,
        description,
        factor_name,
        category_name,
        years_used,
        compare_years_text,
        any_of("raw_value"),
        any_of("ci_low"),
        any_of("ci_high"),
        state_fips,
        stateval,
        state_ci_low,
        state_ci_high,
        ntlval,
        ntl_ci_low,
        ntl_ci_high,
      )
  })

  snapshot_table <- reactive({
    req(measure_values_data())

    final_table <- measure_values_data() %>%
      #final_table = measure_values %>%
      select(
        years_used_display,
        measure_label,
        data_source_system_link, 
        any_of("value_ci"),
        stateval_fmt,
        ntlval_fmt,
        factor_name,
        category_name
      )

    # Force the display order based on year
    if (resolved_year() < 2025) {
      level_order <- c("Demographics", "Health Outcomes", "Health Factors")
    } else {
      level_order <- c(
        "Demographics",
        "Population Health and Well-being",
        "Community Conditions"
      )
    }

    final_table %>%
      mutate(
        category_name_mod = factor(
          ifelse(factor_name == "Demographics", "Demographics", category_name),
          levels = level_order
        )
      ) %>%
      filter(!is.na(category_name_mod))
  })

  output$category_tables_ui <- renderUI({
    req(input$state, input$county, input$year)
    final_table_mod = snapshot_table()
    category_list <- split(final_table_mod, final_table_mod$category_name_mod)

    # Build note text
    note_text <- paste0(
      "*Data source links are provided when available. We do not verify or maintain external links.*",
      
      if (resolved_year() == max(available_years())) {
        paste0(
          "<br>",
          "*\\*In the latest release year, asterisked measure names link to calculation code and documentation when available.*"
        )
      } else {
        ""
      }
    )
    
    # Build HTML for each category with styled collapsible <details>
    tables_html <- map(category_list, function(cat_df) {
      cat_name <- unique(cat_df$category_name_mod)

      cat_df <- cat_df %>%
        mutate(row_group = factor_name) %>%
        select(-category_name, -category_name_mod, -factor_name)

      # Build the label to include county/state/year
      county_label <- if (input$county == "Statewide") "" else input$county
      table_label <- paste0(
        if (county_label != "") county_label else state_full(),
        " ",
        cat_name # category name last
      )
      # Category-specific paragraph
      cat_para <- switch(
        as.character(cat_name),
        "Demographics" = "",
        "Population Health and Well-being" = "Population health and well-being is something we create as a society, not something an individual can attain in a clinic or be responsible for alone. Health is more than being free from disease and pain; health is the ability to thrive. Well-being covers both quality of life and the ability of people and communities to contribute to the world. Population health involves optimal physical, mental, spiritual and social well-being.",
        "Community Conditions" = "Community conditions include the social and economic factors, physical environment and health infrastructure in which people are born, live, learn, work, play, worship and age. Community conditions are also referred to as the social determinants of health.",
        "Health Outcomes" = "Health Outcomes tell us how long people live on average within a community, and how much physical and mental health people experience in a community while they are alive.",
        "Health Factors" = "Health Factors describe the upstream drivers that influence a community’s health, including social and economic conditions, access to healthcare, and the physical environment. They help us understand what contributes to health and well-being before outcomes occur.",
        "" # default blank
      )

      tbl <- gt(cat_df, groupname_col = "row_group") 

      if ("value_ci" %in% colnames(cat_df)) {
        tbl <- tbl %>%
          cols_label(
            value_ci = paste0(input$county, " (95% CI)"),
            stateval_fmt = paste0(state_full(), " (95% CI)"),
            ntlval_fmt = "United States",
            data_source_system_link = "", 
            measure_label = "",
            years_used_display = ""
          )
      } else {
        tbl <- tbl %>%
          cols_label(
            stateval_fmt = paste0(state_full(), " (95% CI)"),
            ntlval_fmt = "United States",
            data_source_system_link = "", 
            measure_label = "",
            years_used_display = ""
          )
      }

      tbl <- tbl %>%
        fmt_markdown(columns = c(
          measure_label, 
          data_source_system_link)) %>%
        tab_options(
          row_group.as_column = FALSE,
          container.width = pct(100),
          table.width = pct(100),
          data_row.padding = px(6),
          heading.align = "left"
        ) %>%
        tab_header(title = table_label, #)  %>%
        subtitle = 
          md(note_text))

      div(
        HTML(
          paste0(
            "<details class='snapshot-details'>",
            
            "<summary>",
            cat_name,
            "</summary>",
            
            "<div class='snapshot-description'>",
            cat_para,
            "</div>",
            
            "<div class='snapshot-table'>",
            as_raw_html(tbl),
            "</div>",
            
            "</details>"
          )
        )
      )
    })

    # Combine all collapsible tables into one UI output
    do.call(tagList, tables_html)
  })

  #output$snapshot_semantic <- gt::render_gt({
  #  req(nrow(measure_values_data()) > 0)
  #  snapshot_table()
  #})

  # Download handler for CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(
        gsub(" ", "_", input$state),
        "_",
        gsub(" ", "_", input$county),
        "_",
        resolved_year(),
        ".csv"
      )
    },
    content = function(file) {
      write.csv(download_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
