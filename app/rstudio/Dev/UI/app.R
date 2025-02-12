library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)

# Source the UI and server components
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

# Run the application with configurable port
#shinyApp(ui = ui, server = server, options = list(port = as.numeric(Sys.getenv("SHINY_PORT", 3838))))

# shiny::runApp("./UI/app.R", port = 3838, host = "0.0.0.0")
# launch_editor(app_loc = "./UI/app.R")
# shinyuieditor::launch_editor(app_loc = "./UI/app.R")