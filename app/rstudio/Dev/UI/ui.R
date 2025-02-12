library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)

# Add custom CSS for hiding the sidebar
css <- "
.sidebar-hidden {
  display: none;
}
.main-expanded {
  grid-column: span 3;
}
"

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
  tags$head(tags$style(HTML(css))),
  grid_page(
    layout = c(
      "sidebar  header   header",
      "sidebar filters  filters",
      "sidebar main     plotly",
      "sidebar main   bins_slider",
      "sidebar main   bluePlot"
    ),
    row_sizes = c(
      "100px",
      "250px",  # Adjusted height for filters
      "500px",    # "5fr" Adjusted height for data preview
      "150px",    # Adjusted height for bins slider
      "350px"       # "3fr" Adjusted height for plotly
    ),
    col_sizes = c(
      "350px",
      "2fr",
      "1fr"
    ),
    gap_size = "1rem",
    grid_card_text(
      area = "header",
      content = div(
        tags$img(src = "https://res.cloudinary.com/appsembler/image/upload/v1666688747/91167bf0-46e8-47ef-bc21-5966044167df/dhis2-icon-rgb-positive.svg.svg", height = "50px"),
        HTML("<span style='color: #212227;'>DHIS2 Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>"),
        class = "title"
      ),
      alignment = "center",
      is_title = TRUE
    ),
    grid_card(
      area = "sidebar",
      card_header(
        div(
          actionButton("toggle_sidebar", "☰", class = "btn-primary", style = "float: left;"),
          " Settings"
        )
      ),
      card_body(
        tabsetPanel(
          tabPanel("Source Setting",
                   textInput("setting", "Setting", value = "Ethiopia"),
                   textInput("source", "Source", value = "DHIS_2"),
                   textInput("iso3", "ISO3", value = "ETH"),
                   conditionalPanel(
                     condition = "output.role == 'mikeintosh'",
                     actionButton("save_source_settings", "Save Source Settings", class = "btn-primary")
                   ),
                   actionButton("load_source_settings", "Load Source Settings", class = "btn-info")
          ),
          tabPanel("Fetcher Setting",
                   conditionalPanel(
                     condition = "output.role == 'mikeintosh'",
                     actionButton("save_settings", "Save Settings", class = "btn-primary"),
                     textInput("base_url", "DHIS2 Base URL", value = Sys.getenv("DHIS2_BASE_URL")),
                     textInput("username", "Username", value = Sys.getenv("DHIS2_USERNAME")),
                     passwordInput("password", "Password", value = Sys.getenv("DHIS2_PASSWORD"))
                   ),
                   actionButton("load_settings", "Load Settings", class = "btn-info"),
                   actionButton("fetch_data", "Fetch Data", class = "btn-success"),
                   downloadButton("export_data", "Export to Excel", class = "btn-warning"),
                   actionButton("help", "Help", class = "btn-info"),
                   checkboxInput("select_all_indicators", "Select All Indicators", value = FALSE),
                   selectizeInput("indicators", "Select Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                   textInput("indicator_abbr", "Indicator Abbreviations (comma-separated)", value = ""),
                   selectizeInput("favorable_indicators", "Favorable Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                   checkboxInput("select_all_org_units", "Select All Organisation Units", value = FALSE),
                   selectizeInput("org_units", "Select Organisation Units", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                   checkboxInput("select_all_zones", "Select All Zones", value = FALSE),
                   selectizeInput("zones", "Select Zones", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                   checkboxInput("select_all_woredas", "Select All Woredas", value = FALSE),
                   selectizeInput("woredas", "Select Woredas", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
                   textInput("periods", "Periods (comma-separated)", value = "")
          )
        )
      )
    ),
    grid_card(
      area = "filters",
      card_header("Filters"),
      card_body(
        fluidRow(
          column(3, selectizeInput("filter_indicators", "Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
          column(3, selectizeInput("filter_dimensions", "Dimensions", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
          column(3, selectizeInput("filter_subgroups", "Subgroups", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
          column(3, selectizeInput("filter_dates", "Dates", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
          column(12, actionButton("apply_filters", "Apply Filters", class = "btn-primary"))
        )
      )
    ),
    grid_card(
      area = "main",
      card_header("Data Preview"),
      card_body(
        div(
          class = "main",
          withSpinner(DTOutput("data_preview"))
        )
      )
    ),
    grid_card(
      area = "plotly",
      card_header("Interactive Plot"),
      card_body(
        plotlyOutput(
          outputId = "distPlot",
          width = "100%",
          height = "100%"
        )
      )
    ),
    grid_card(
      area = "bins_slider",
      card_body(
        sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
      )
    ),
    grid_card_plot(area = "bluePlot"),
    div(
      class = "footer",
      HTML("&copy; 2025 Designed & Developed by: <a href='https://merqconsultancy.org'><b>MERQ Consultancy</b>.</a>")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$toggle_sidebar, {
    toggleClass(selector = ".sidebar", class = "sidebar-hidden")
    toggleClass(selector = ".main", class = "main-expanded")
  })
  
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$data_preview <- renderDT({
    head(faithful, 10) # Adjust this to match your data
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
