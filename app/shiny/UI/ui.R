library(shiny)
library(shinydashboard) # for Dashboard
library(shinyWidgets) # for radio button widgets
library(shinydashboardPlus)
library(shinyjs) # to perform common useful JavaScript operations in Shiny apps
library(shinyBS) # for bsTooltip function
library(shinyalert) # for alert message very nice format
library(dplyr) # select functions are covered in the require
library(plyr) # empty() function is from this package
library(DT) # for using %>% which works as a pipe in R code
library(ggplot2)
library(plotly)
library(scales) ## used to format date like only month or month and year
library(colorspace) # to generate Rainbow coloring function
library(pastecs) # for descriptive statistics
library(shinycssloaders)
library(gridlayout)
library(bslib)
library(colourpicker)
library(httr)
library(jsonlite)
library(openxlsx)
library(arrow) # Add arrow library for Parquet support

# Source the UI and server components of data exploration and cleaning
#source("exclean.R")
source("exclean.R", local = TRUE)$value

# Custom CSS for hiding the sidebar and settings
css <- "
.sidebar-hidden {
  display: none;
}
.main-expanded {
  grid-column: span 3;
}
.settings-collapsed {
  display: none;
}
.sidebar {
  width: 350px;
  overflow-y: auto;
  height: 100vh; /* Full viewport height */
  position: fixed; /* Fix the sidebar position */
}
.box.box-solid.box-primary>.box-header {
    color: #fff;
    background: #3c8dbc;
    background-color: #9aaab7;
}
.main-header .logo
{ -webkit-transition: width .3s ease-in-out;
-o-transition: width .3s ease-in-out;
transition: width .3s ease-in-out;
display: block;
float: left;
height: 50px;
font-size: 14px;
line-height: 50px;
text-align: left;
width: 350px;
padding: 0px 15px;
font-weight: 300;
overflow: hidden;
}
.main-header .navbar {
    -webkit-transition: margin-left .3s ease-in-out;
    -o-transition: margin-left .3s ease-in-out;
    transition: margin-left .3s ease-in-out;
    margin-bottom: 0;
    margin-left: 350px;
    border: none;
    min-height: 50px;
    border-radius: 0;
}
.skin-blue .main-header .navbar {
    background-color: #0090d7;
}
.skin-blue .main-header .logo {
    background-color: #FFFFFFFF;
    color: #fff;
    border-bottom: 0 solid transparent;
}
.skin-blue .main-header .logo:hover {
    background-color: #37c9ff;
}
.shiny-frame {
    border: none;
    width: 100%;
    height: 777px;
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
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://res.cloudinary.com/appsembler/image/upload/v1666688747/91167bf0-46e8-47ef-bc21-5966044167df/dhis2-icon-rgb-positive.svg.svg", height = "50px"),
      HTML("<span style='color: #212227;'>DHIS2 Data Fetcher for </span><span style='color: #007dc9; font-weight: bold;'>HEAT </span><span style='color: #b5e71c; font-weight: bold;'>Plus(+)</span>")
    )
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Data Preview", tabName = "data_preview", icon = icon("eye")),
      menuItem("Settings",
        tabName = "settings", icon = icon("cogs"),
        menuSubItem("Source Setting", tabName = "source_setting", icon = icon("sliders-h")),
        menuSubItem("Fetcher Setting", tabName = "fetcher_setting", icon = icon("download"))
      ),
      menuItem("Data Management",
        icon = icon("database"),
        menuSubItem("Explore & Clean", tabName = "explore_clean", icon = icon("broom"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(css))),
    tabItems(
      tabItem(
        tabName = "source_setting",
        fluidRow(
          box(
            title = "Source Setting",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textInput("setting", "Setting", value = "Ethiopia"),
            textInput("source", "Source", value = "DHIS_2"),
            textInput("iso3", "ISO3", value = "ETH"),
            conditionalPanel(
              condition = "output.role == 'mikeintosh'",
              actionButton("save_source_settings", "Save Source Settings", class = "btn-primary")
            ),
            actionButton("load_source_settings", "Load Source Settings", class = "btn-info")
          )
        )
      ),
      tabItem(
        tabName = "fetcher_setting",
        fluidRow(
          box(
            title = "Fetcher Setting",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
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
            downloadButton("export_parquet", "Export to Parquet", class = "btn-danger"),
            actionButton("help", "Help", class = "btn-info"),
            hr(),
            checkboxInput("select_all_indicators", "Select All Indicators", value = FALSE),
            selectizeInput("indicators", "Select Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
            textInput("indicator_abbr", "Indicator Abbreviations (comma-separated)", value = ""),
            selectizeInput("favorable_indicators", "Favorable Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
            hr(),
            checkboxInput("custom_indicator_scales", "Set Custom Indicator Scales", value = TRUE),
            conditionalPanel(
              condition = "input.custom_indicator_scales == true",
              selectizeInput("custom_indicators", "Select Indicators for Custom Scales", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
              uiOutput("custom_scales_ui")
            ),
            hr(),
            checkboxInput("select_all_org_units", "Select All Organisation Units", value = FALSE),
            selectizeInput("org_units", "Select Organisation Units", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
            hr(),
            checkboxInput("select_all_zones", "Select All Zones", value = FALSE),
            selectizeInput("zones", "Select Zones", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
            hr(),
            checkboxInput("select_all_woredas", "Select All Woredas", value = FALSE),
            selectizeInput("woredas", "Select Woredas", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000)),
            hr(),
            textInput("periods", "Periods (comma-separated)", value = ""),
            hr(),
            hr()
          )
        )
      ),
      tabItem(
        tabName = "data_preview",
        fluidRow(
            actionButton("load_settings", "Load Settings", class = "btn-info"),
            actionButton("fetch_data", "Fetch Data", class = "btn-success"),
            downloadButton("export_data", "Export to Excel", class = "btn-warning"),
            downloadButton("export_parquet", "Export to Parquet", class = "btn-danger"),
            actionButton("help", "Help", class = "btn-info"),
            hr(),

          box(
            title = "Filters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3, selectizeInput("filter_indicators", "Indicators", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
              column(3, selectizeInput("filter_dimensions", "Dimensions", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
              column(3, selectizeInput("filter_subgroups", "Subgroups", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
              column(3, selectizeInput("filter_dates", "Dates", choices = NULL, multiple = TRUE, options = list(maxOptions = 1000))),
              column(12, actionButton("apply_filters", "Apply Filters", class = "btn-primary", style = "float: right;"))
            )
          ),
          box(
            title = "Data Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            withSpinner(DTOutput("data_preview"))
          ),
          column(
            width = 6,
            box(
              title = "Plot Settings",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(6, colourInput("plot_color", "Select Plot Color", value = "#00ae21"))
              )
            ),
            box(
              title = "Interactive Plot",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("distPlot", width = "100%", height = "100%")
            ),
            box(
              title = "Dynamic Plot",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              selectInput("view_by", "Dynamic Plot:", choices = c("Dimension", "Subgroup")),
              selectInput("chart_type", "Chart Type:", choices = c("Scatter", "Bar")),
              plotlyOutput("dynamicPlotOutput", width = "100%", height = "100%")
            )
          )
        )
      ),
      tabItem(
        tabName = "explore_clean",
        h2("Data Management"),
        hr(),
        uiOutput("exclean_ui"),
        hr(),
        fluidRow(
          #uiOutput("exclean_ui"),
          box(
            title = "Data Management",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("Data Cleansing or Data Wrangling is an important early step in the data analytics process."),
            # Dynamically render the exclean UI here
            #uiOutput("exclean_ui")
            #uiOutput("mDataExplorationUI")
          )
        )
      )
    ),
    hr(),
    hr(),
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
    plot_ly(x = ~ faithful[, 2], type = "histogram", marker = list(color = input$plot_color)) # Use selected color
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
# shinyApp(ui = ui, server = server)
