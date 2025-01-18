library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(httr)
library(jsonlite)
library(openxlsx)

# Check for and install required packages
required_packages <- c("shiny", "shinyjs", "shinyWidgets", "shinycssloaders", "DT", "httr", "jsonlite", "openxlsx")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load existing functions
source("dhis2_data.R")

# Define specific organisation units (regions)
specific_org_units <- c(
  "yY9BLUUegel", "UFtGyqJMEZh", "yb9NKGA8uqt", "Fccw8uMlJHN",
  "tDoLtk2ylu4", "G9hDiPNoB7d", "moBiwh9h5Ce", "b9nYedsL8te",
  "XU2wpLlX4Vk", "xNUoZIrGKxQ", "PCKGSJoNHXi", "a2QIIR2UXcd",
  "HIlnt7Qj8do", "Gmw0DJLXGtx"
)

# Define UI
ui <- fluidPage(
  useShinyjs(),
  setBackgroundColor(color = "#f7f7f7"),
  tags$head(
    tags$style(HTML("
            .title {
                font-size: 24px;
                font-weight: bold;
                color: #2c3e50;
                text-align: center;
                margin-top: 7px;
                margin-bottom: 20px;
            }
            .sidebar {
                background-color: #ecf0f1;
                padding: 20px;
                border-radius: 10px;
            }
            .main {
                background-color: #ffffff;
                padding: 20px;
                border-radius: 10px;
                overflow-x: auto;
                overflow-y: auto;
                height: auto;
            }
            .hamburger-menu {
                display: block;
            }
            @media (max-width: 768px) {
                .sidebar {
                    display: none;
                }
                .sidebar.show {
                    display: block;
                }
            }
            .footer {
                text-align: center;
                padding: 10px;
                background-color: #ecf0f1;
                border-radius: 10px;
                margin-top: 20px;
            }
            .download-button {
                float: right;
                margin-bottom: 10px;
            }
            .modal-content {
                position: relative;
                margin-top: 170px;
                background-color: #fff;
                background-clip: padding-box;
                border: 1px solid #999;
                border: 1px solid rgba(0,0,0,.2);
                border-radius: 6px;
                -webkit-box-shadow: 0 3px 9px rgba(0,0,0,.5);
                box-shadow: 0 3px 9px rgba(0,0,0,.5);
                outline: 0;
            }
        "))
  ),
  titlePanel(
    div(
      # tags$img(src = "https://dhis2.org/wp-content/uploads/dhis2-logo-rgb-positive.svg", height = "50px"),
      # tags$img(src = "./dhis2.svg", height = "50px"),
      tags$img(src = "https://res.cloudinary.com/appsembler/image/upload/v1666688747/91167bf0-46e8-47ef-bc21-5966044167df/dhis2-icon-rgb-positive.svg.svg", height = "50px"),
      HTML("<span style='color: #212227;'>DHIS2 Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>"),
      class = "title"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        class = "hamburger-menu",
        actionButton("toggle_sidebar", "☰", class = "btn-primary")
      ),
      div(
        class = "sidebar",
        conditionalPanel(
          condition = "output.role == 'mikeintosh'",
          textInput("base_url", "DHIS2 Base URL", value = Sys.getenv("DHIS2_BASE_URL")),
          textInput("username", "Username", value = Sys.getenv("DHIS2_USERNAME")),
          passwordInput("password", "Password", value = Sys.getenv("DHIS2_PASSWORD"))
        ),
        checkboxInput("select_all_indicators", "Select All Indicators", value = FALSE),
        selectizeInput("indicators", "Select Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
        textInput("indicator_abbr", "Indicator Abbreviations (comma-separated)", value = ""),
        checkboxInput("select_all_org_units", "Select All Organisation Units", value = FALSE),
        selectizeInput("org_units", "Select Organisation Units", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
        checkboxInput("select_all_zones", "Select All Zones", value = FALSE),
        selectizeInput("zones", "Select Zones", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
        checkboxInput("select_all_woredas", "Select All Woredas", value = FALSE),
        selectizeInput("woredas", "Select Woredas", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
        textInput("periods", "Periods (comma-separated)", value = ""),
        conditionalPanel(
          condition = "output.role == 'mikeintosh'",
          actionButton("save_settings", "Save Settings", class = "btn-primary")
        ),
        actionButton("load_settings", "Load Settings", class = "btn-info"),
        actionButton("fetch_data", "Fetch Data", class = "btn-success"),
        downloadButton("export_data", "Export to Excel", class = "btn-warning"), # Change this line
        actionButton("help", "Help", class = "btn-info") # Add this line for the Help button
      )
    ),
    mainPanel(
      div(
        class = "main",
        withSpinner(DTOutput("data_preview"))
      )
    )
  ),
  div(
    class = "footer",
    HTML("&copy; 2025 Designed & Developed by: <a href='https://merqconsultancy.org'><b>MERQ Consultancy</b>.</a>")
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store settings and data
  settings <- reactiveValues()
  data <- reactiveValues()
  choices <- reactiveValues()
  data_fetched <- reactiveVal(FALSE)
  user_role <- reactiveVal(NULL)
  exported_file_name <- reactiveVal(NULL) # Add this line

  # Toggle sidebar visibility
  observeEvent(input$toggle_sidebar, {
    toggleClass(selector = ".sidebar", class = "show")
    toggleClass(selector = ".main", class = "full-width")
  })

  # Fetch metadata and populate select inputs
  observe({
    choices$indicators <- setNames(indicators_metadata$id, indicators_metadata$displayName)
    choices$org_units <- setNames(specific_org_units, org_units_metadata$displayName[org_units_metadata$id %in% specific_org_units])
    choices$zones <- setNames(org_units_metadata$id, org_units_metadata$displayName)
    choices$woredas <- setNames(org_units_metadata$id, org_units_metadata$displayName)

    updateSelectizeInput(session, "indicators", choices = choices$indicators, server = TRUE)
    updateSelectizeInput(session, "org_units", choices = choices$org_units, server = TRUE)
    updateSelectizeInput(session, "zones", choices = choices$zones, server = TRUE)
    updateSelectizeInput(session, "woredas", choices = choices$woredas, server = TRUE)
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

    settings_list <- list(
      base_url = input$base_url,
      username = input$username,
      password = input$password,
      indicators = input$indicators,
      indicator_abbr = setNames(strsplit(input$indicator_abbr, ",")[[1]], input$indicators),
      org_units = input$org_units,
      zones = input$zones,
      woredas = input$woredas, # Ensure woredas are saved
      periods = strsplit(input$periods, ",")[[1]]
    )
    saveRDS(settings_list, file = "../settings.rds")
    sendSweetAlert(session, title = "Success", text = "Settings saved to file.", type = "success")
  })

  # Load settings
  observeEvent(input$load_settings, {
    if (file.exists("../settings.rds")) {
      settings_list <- readRDS("../settings.rds")
      updateTextInput(session, "base_url", value = settings_list$base_url)
      updateTextInput(session, "username", value = settings_list$username)
      updateTextInput(session, "password", value = settings_list$password)
      updateSelectizeInput(session, "indicators", selected = settings_list$indicators)
      updateTextInput(session, "indicator_abbr", value = paste(settings_list$indicator_abbr, collapse = ","))
      updateSelectizeInput(session, "org_units", selected = settings_list$org_units)
      updateSelectizeInput(session, "zones", selected = settings_list$zones)
      updateSelectizeInput(session, "woredas", selected = settings_list$woredas) # Ensure woredas are loaded
      updateTextInput(session, "periods", value = paste(settings_list$periods, collapse = ","))
      #sendSweetAlert(session, title = "Success", text = "Settings loaded from file.", type = "success")
      sendSweetAlert(session, title = "Success", text = "Settings are loaded!", type = "success")
    } else {
      #sendSweetAlert(session, title = "Error", text = "Settings file not found.", type = "error")
      sendSweetAlert(session, title = "Error", text = "No Settings found!", type = "error")
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

    settings_list <- readRDS("../settings.rds")
    settings_abbr <- settings_list$indicator_abbr

    withProgress(message = "Fetching data...", value = 0, {
      incProgress(0.2, detail = "Fetching population data for Region...")
      population_data_region <- fetch_population_data(input$org_units, strsplit(input$periods, ",")[[1]])

      incProgress(0.4, detail = "Fetching analytics data for Region...")
      analytics_data_region <- fetch_indicator_data(input$indicators, input$org_units, strsplit(input$periods, ",")[[1]])
      formatted_data_region <- format_analytics_data(analytics_data_region, indicators_metadata, org_units_metadata, population_data_region, indicator_map, dimension = "Region", settings_abbr = settings_abbr)

      incProgress(0.6, detail = "Fetching population data for Zone...")
      population_data_zone <- fetch_population_data(NULL, strsplit(input$periods, ",")[[1]], zone_ids = input$zones)

      incProgress(0.8, detail = "Fetching analytics data for Zone...")
      analytics_data_zone <- fetch_indicator_data(input$indicators, input$zones, strsplit(input$periods, ",")[[1]])
      formatted_data_zone <- format_analytics_data(analytics_data_zone, indicators_metadata, org_units_metadata, population_data_zone, indicator_map, dimension = "Zone", settings_abbr = settings_abbr)

      incProgress(0.9, detail = "Fetching population data for Woreda...")
      population_data_woreda <- fetch_population_data(NULL, strsplit(input$periods, ",")[[1]], zone_ids = input$woredas)

      incProgress(1, detail = "Fetching analytics data for Woreda...")
      analytics_data_woreda <- fetch_indicator_data(input$indicators, input$woredas, strsplit(input$periods, ",")[[1]])
      formatted_data_woreda <- format_analytics_data(analytics_data_woreda, indicators_metadata, org_units_metadata, population_data_woreda, indicator_map, dimension = "Woreda", settings_abbr = settings_abbr)

      data$combined <- rbind(formatted_data_region, formatted_data_zone, formatted_data_woreda)

      # Ensure all columns are properly converted to the correct types
      data$combined[] <- lapply(data$combined, function(x) {
        if (is.list(x)) {
          return(unlist(x))
        } else {
          return(x)
        }
      })

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

  # Export data to Excel and trigger download
  output$export_data <- downloadHandler(
    filename = function() {
      file_name <- paste("DHIS2_DATA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
      exported_file_name(file_name) # Store the file name
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
      showPasswordModal("Incorrect password. Please try again.") # Show the modal again with an error message
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

# Run the application with configurable port
shinyApp(ui = ui, server = server, options = list(port = as.numeric(Sys.getenv("SHINY_PORT", 3838))))