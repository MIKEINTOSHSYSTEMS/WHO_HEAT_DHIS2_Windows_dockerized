library(shiny)
library(httr)
library(jsonlite)
library(openxlsx)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

# Load existing functions
source("dhis2_data.R")

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store settings and data
  settings <- reactiveValues()
  data <- reactiveValues()
  choices <- reactiveValues()
  data_fetched <- reactiveVal(FALSE)
  user_role <- reactiveVal(NULL)
  exported_file_name <- reactiveVal(NULL)
  
  # Toggle sidebar visibility
  observeEvent(input$toggle_sidebar, {
    toggleClass(selector = ".sidebar", class = "show")
    toggleClass(selector = ".main", class = "full-width")
  })
  
  # Fetch metadata and populate select inputs
  observe({
    # Fetch Zones and Woredas metadata
    zones_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=3&paging=false")$organisationUnits
    woredas_metadata <- get_dhis2_data("/api/organisationUnits?fields=id,displayName&level=4&paging=true&pageSize=600")$organisationUnits
    
    choices$indicators <- setNames(indicators_metadata$id, indicators_metadata$displayName)
    choices$org_units <- setNames(specific_org_units, org_units_metadata$displayName[org_units_metadata$id %in% specific_org_units])
    choices$zones <- setNames(zones_metadata$id, zones_metadata$displayName)
    choices$woredas <- setNames(woredas_metadata$id, woredas_metadata$displayName)
    
    updateSelectizeInput(session, "indicators", choices = choices$indicators, server = TRUE)
    updateSelectizeInput(session, "org_units", choices = choices$org_units, server = TRUE)
    updateSelectizeInput(session, "zones", choices = choices$zones, server = TRUE)
    updateSelectizeInput(session, "woredas", choices = choices$woredas, server = TRUE)
  })
  
  # Update Favorable Indicators based on selected indicators
  observe({
    if (!is.null(input$indicators)) {
      # Get the display names of the selected indicators
      favorable_choices <- indicators_metadata$displayName[indicators_metadata$id %in% input$indicators]
      updateSelectizeInput(session, "favorable_indicators", choices = favorable_choices, server = TRUE)
    }
  })
  
  # Select All functionality
  observeEvent(input$select_all_indicators, {
    if (input$select_all_indicators) {
      updateSelectizeInput(session, "indicators", selected = names(choices$indicators))
    } else {
      updateSelectizeInput(session, "indicators", selected = NULL)
    }
  })
  
  observeEvent(input$select_all_org_units, {
    if (input$select_all_org_units) {
      updateSelectizeInput(session, "org_units", selected = names(choices$org_units))
    } else {
      updateSelectizeInput(session, "org_units", selected = NULL)
    }
  })
  
  observeEvent(input$select_all_zones, {
    if (input$select_all_zones) {
      updateSelectizeInput(session, "zones", selected = names(choices$zones))
    } else {
      updateSelectizeInput(session, "zones", selected = NULL)
    }
  })
  
  observeEvent(input$select_all_woredas, {
    if (input$select_all_woredas) {
      updateSelectizeInput(session, "woredas", selected = names(choices$woredas))
    } else {
      updateSelectizeInput(session, "woredas", selected = NULL)
    }
  })
  
  # Save settings
  observeEvent(input$save_settings, {
    if (length(input$indicators) != length(strsplit(input$indicator_abbr, ",")[[1]])) {
      showModal(modalDialog(
        title = "Error",
        "The number of indicators and abbreviations must be equal.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Convert favorable indicator display names back to IDs
    favorable_indicator_ids <- indicators_metadata$id[indicators_metadata$displayName %in% input$favorable_indicators]
    
    settings_list <- list(
      base_url = input$base_url,
      username = input$username,
      password = input$password,
      indicators = input$indicators,
      indicator_abbr = setNames(strsplit(input$indicator_abbr, ",")[[1]], input$indicators),
      favorable_indicators = favorable_indicator_ids, # Save favorable indicator IDs
      org_units = input$org_units,
      zones = input$zones,
      woredas = input$woredas,
      periods = strsplit(input$periods, ",")[[1]]
    )
    
    saveRDS(settings_list, file = "./saved_setting/settings.rds")
    sendSweetAlert(session, title = "Success", text = "Settings saved to file.", type = "success")
  })
  
  # Load settings
  observeEvent(input$load_settings, {
    if (file.exists("./saved_setting/settings.rds")) {
      settings_list <- readRDS("./saved_setting/settings.rds")
      print(settings_list) # Print loaded settings to debug
      
      # Convert favorable indicator IDs back to display names
      favorable_indicator_names <- indicators_metadata$displayName[indicators_metadata$id %in% settings_list$favorable_indicators]
      
      updateTextInput(session, "base_url", value = settings_list$base_url)
      updateTextInput(session, "username", value = settings_list$username)
      updateTextInput(session, "password", value = settings_list$password)
      updateSelectizeInput(session, "indicators", selected = settings_list$indicators)
      updateTextInput(session, "indicator_abbr", value = paste(settings_list$indicator_abbr, collapse = ","))
      updateSelectizeInput(session, "favorable_indicators", selected = favorable_indicator_names) # Load favorable indicator display names
      updateSelectizeInput(session, "org_units", selected = settings_list$org_units)
      updateSelectizeInput(session, "zones", selected = settings_list$zones)
      updateSelectizeInput(session, "woredas", selected = settings_list$woredas)
      updateTextInput(session, "periods", value = paste(settings_list$periods, collapse = ","))
      
      sendSweetAlert(session, title = "Success", text = "Settings are loaded!", type = "success")
    } else {
      sendSweetAlert(session, title = "Error", text = "No Settings found!", type = "error")
    }
  })
  
  # Save source settings
  observeEvent(input$save_source_settings, {
    if (user_role() == "mikeintosh") {
      source_settings <- list(
        setting = input$setting,
        source = input$source,
        iso3 = input$iso3
      )
      saveRDS(source_settings, file = "./saved_setting/source_settings.rds")
      sendSweetAlert(session, title = "Success", text = "Source settings saved to file.", type = "success")
    } else {
      sendSweetAlert(session, title = "Error", text = "You do not have permission to save source settings.", type = "error")
    }
  })
  
  # Load source settings
  observeEvent(input$load_source_settings, {
    if (file.exists("./saved_setting/source_settings.rds")) {
      source_settings <- readRDS("./saved_setting/source_settings.rds")
      updateTextInput(session, "setting", value = source_settings$setting)
      updateTextInput(session, "source", value = source_settings$source)
      updateTextInput(session, "iso3", value = source_settings$iso3)
      sendSweetAlert(session, title = "Success", text = "Source settings loaded from file.", type = "success")
    } else {
      sendSweetAlert(session, title = "Error", text = "No source settings found!", type = "error")
    }
  })
  
  # Fetch data
  observeEvent(input$fetch_data, {
    Sys.setenv(DHIS2_BASE_URL = input$base_url)
    Sys.setenv(DHIS2_USERNAME = input$username)
    Sys.setenv(DHIS2_PASSWORD = input$password)
    
    if (length(input$org_units) == 0) {
      sendSweetAlert(session, title = "Error", text = "No organisation units selected.", type = "error")
      return()
    }
    
    settings_list <- readRDS("./saved_setting/settings.rds")
    settings_abbr <- settings_list$indicator_abbr
    favorable_indicators <- settings_list$favorable_indicators
    
    withProgress(message = "Fetching data...", value = 0, {
      incProgress(0.2, detail = "Fetching population data for Region...")
      population_data_region <- fetch_population_data(input$org_units, strsplit(input$periods, ",")[[1]])
      
      incProgress(0.4, detail = "Fetching analytics data for Region...")
      analytics_data_region <- fetch_indicator_data(input$indicators, input$org_units, strsplit(input$periods, ",")[[1]])
      formatted_data_region <- format_analytics_data(analytics_data_region, indicators_metadata, org_units_metadata, population_data_region, indicator_map, dimension = "Region", settings_abbr = settings_abbr)
      
      if (length(input$zones) > 0) {
        incProgress(0.6, detail = "Fetching population data for Zone...")
        population_data_zone <- fetch_population_data(NULL, strsplit(input$periods, ",")[[1]], zone_ids = input$zones)
        
        incProgress(0.8, detail = "Fetching analytics data for Zone...")
        analytics_data_zone <- fetch_indicator_data(input$indicators, input$zones, strsplit(input$periods, ",")[[1]])
        formatted_data_zone <- format_analytics_data(analytics_data_zone, indicators_metadata, org_units_metadata, population_data_zone, indicator_map, dimension = "Zone", settings_abbr = settings_abbr)
      } else {
        formatted_data_zone <- data.frame()
      }
      
      if (length(input$woredas) > 0) {
        incProgress(0.9, detail = "Fetching population data for Woreda...")
        population_data_woreda <- fetch_population_data(NULL, strsplit(input$periods, ",")[[1]], woreda_ids = input$woredas)
        
        incProgress(1, detail = "Fetching analytics data for Woreda...")
        analytics_data_woreda <- fetch_indicator_data(input$indicators, input$woredas, strsplit(input$periods, ",")[[1]])
        formatted_data_woreda <- format_analytics_data(analytics_data_woreda, indicators_metadata, org_units_metadata, population_data_woreda, indicator_map, dimension = "Woreda", settings_abbr = settings_abbr)
      } else {
        formatted_data_woreda <- data.frame()
      }
      
      data$combined <- rbind(formatted_data_region, formatted_data_zone, formatted_data_woreda)
      
      # Ensure all columns are properly converted to the correct types
      data$combined[] <- lapply(data$combined, function(x) {
        if (is.list(x)) {
          return(unlist(x))
        } else {
          return(x)
        }
      })
      
      # Update favourable_indicator column based on selected favorable indicators
      data$combined$favourable_indicator <- ifelse(data$combined$indicator_name %in% input$favorable_indicators, 1, 0)
      
      # Populate filter selection boxes
      updateSelectizeInput(session, "filter_indicators", choices = unique(data$combined$indicator_name))
      updateSelectizeInput(session, "filter_dimensions", choices = unique(data$combined$dimension))
      updateSelectizeInput(session, "filter_subgroups", choices = unique(data$combined$subgroup))
      updateSelectizeInput(session, "filter_dates", choices = unique(data$combined$date))
      
      output$data_preview <- renderDT({
        datatable(data$combined, options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20, 50, 100),
          scrollX = TRUE,
          autoWidth = TRUE,
          searching = TRUE,
          ordering = TRUE
        ))
      })
      data_fetched(TRUE)
      sendSweetAlert(session, title = "Success", text = "Data fetched successfully.", type = "success")
    })
  })
  
  # Apply filters
  observeEvent(input$apply_filters, {
    req(data$combined)
    
    filtered_data <- data$combined
    
    if (!is.null(input$filter_indicators) && length(input$filter_indicators) > 0) {
      filtered_data <- filtered_data[filtered_data$indicator_name %in% input$filter_indicators, ]
    }
    
    if (!is.null(input$filter_dimensions) && length(input$filter_dimensions) > 0) {
      filtered_data <- filtered_data[filtered_data$dimension %in% input$filter_dimensions, ]
    }
    
    if (!is.null(input$filter_subgroups) && length(input$filter_subgroups) > 0) {
      filtered_data <- filtered_data[filtered_data$subgroup %in% input$filter_subgroups, ]
    }
    
    if (!is.null(input$filter_dates) && length(input$filter_dates) > 0) {
      filtered_data <- filtered_data[filtered_data$date %in% input$filter_dates, ]
    }
    
    data$filtered <- filtered_data
    
    output$data_preview <- renderDT({
      datatable(data$filtered, options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 15, 20, 50, 100),
        scrollX = TRUE,
        autoWidth = TRUE,
        searching = TRUE,
        ordering = TRUE
      ))
    })
    
    output$distPlot <- renderPlotly({
      req(data$filtered)
      plot_ly(data = data$filtered, x = ~date, y = ~estimate, type = "scatter", mode = "lines+markers", text = ~ paste("Indicator:", indicator_name, "<br>Dimension:", dimension, "<br>Subgroup:", subgroup, "<br>Date:", date, "<br>Estimate:", estimate), hoverinfo = "text")
    })
    
    output$bluePlot <- renderPlot({
      req(data$filtered)
      x <- data$filtered$estimate
      bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "steelblue", border = "white")
    })
  })
  
  # Update plots based on fetched data
  observe({
    req(data_fetched())
    
    output$distPlot <- renderPlotly({
      req(data$combined)
      plot_ly(data = data$combined, x = ~date, y = ~estimate, type = "scatter", mode = "lines+markers", text = ~ paste("Indicator:", indicator_name, "<br>Dimension:", dimension, "<br>Subgroup:", subgroup, "<br>Date:", date, "<br>Estimate:", estimate), hoverinfo = "text")
    })
    
    output$bluePlot <- renderPlot({
      req(data$combined)
      x <- data$combined$estimate
      bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
      hist(x, breaks = bins, col = "steelblue", border = "white")
    })
  })
  
  # Export data to Excel and trigger download
  output$export_data <- downloadHandler(
    filename = function() {
      file_name <- paste("DHIS2_DATA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
      exported_file_name(file_name)
      file_name
    },
    content = function(file) {
      if (!is.null(data$combined) && nrow(data$combined) > 0) {
        withProgress(message = "Exporting data...", value = 0, {
          incProgress(0.5, detail = "Writing data to Excel...")
          write.xlsx(data$combined,
                     file = file,
                     sheetName = "Indicators_Data", rowNames = FALSE
          )
          incProgress(1, detail = "Export complete.")
          sendSweetAlert(session, title = "Success", text = paste("Excel file '", exported_file_name(), "' exported successfully."), type = "success")
        })
      } else {
        showModal(modalDialog(
          title = "Error",
          "No data to write.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  )
  
  # Password protection
  showPasswordModal <- function(message = NULL) {
    showModal(modalDialog(
      title = "Authentication Required",
      passwordInput("auth_password", "Enter Password: [demo]"),
      if (!is.null(message)) div(style = "color: red;", message),
      footer = tagList(
        actionButton("auth_submit", "Submit")
      )
    ))
  }
  
  observe({
    showPasswordModal()
  })
  
  observeEvent(input$auth_submit, {
    if (input$auth_password == "mikeintosh") {
      user_role("mikeintosh")
      removeModal()
    } else if (input$auth_password == "demo") {
      user_role("demo")
      removeModal()
    } else {
      showPasswordModal("Incorrect password. Please try again.")
    }
  })
  
  output$role <- reactive({
    user_role()
  })
  outputOptions(output, "role", suspendWhenHidden = FALSE)
  
  # Show user guide
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "User Guide",
      HTML("
        <h3>Welcome to the DHIS2 Data Fetcher for HEAT Plus(+)</h3>
        <p>This application allows you to fetch and export data from DHIS2.</p>
        <h4>Steps to use the application:</h4>
        <ol>
          <li><b>Authentication:</b> Enter the correct password to access the application.</li>
          <li><b>DHIS2 Credentials:</b> Enter the DHIS2 Base URL, Username, and Password.</li>
          <li><b>Select Indicators:</b> Choose the indicators you want to fetch data for.</li>
          <li><b>Select Organisation Units:</b> Choose the organisation units you want to fetch data for.</li>
          <li><b>Select Zones and Woredas:</b> Choose the zones and woredas you want to fetch data for.</li>
          <li><b>Specify Periods:</b> Enter the periods (comma-separated) for which you want to fetch data.</li>
          <li><b>Save Settings:</b> Save your settings for
      "),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}