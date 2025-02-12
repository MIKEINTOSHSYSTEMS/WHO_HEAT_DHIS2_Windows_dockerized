# install.R script to install required packages

# List of required packages
required_packages <- c(
    "shiny",
    "shinydashboard",
    "shinyWidgets",
    "shinyjs",
    "shinyBS",
    "shinyalert",
    "dplyr",
    "plyr",
    "DT",
    "ggplot2",
    "plotly",
    "scales",
    "pastecs",
    "car",
    "ggfortify",
    "arrow",
    "psych",
    "sqldf",
    "lubridate",
    "kableExtra",
    "gridExtra",
    "reshape2",
    "fastDummies",
    "DataExplorer",
    "tidyverse",
    "colorspace"
)

# Install packages if they are not already installed
install_missing <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
    }
}

# Loop through each package and install if missing
lapply(required_packages, install_missing)

# Print a message when installation is complete
cat("All required packages are installed.\n")

# to install
# source("install.R")
# source("UI/install.R")