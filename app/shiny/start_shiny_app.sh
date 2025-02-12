#!/bin/bash
R -e "shiny::runApp('./UI/app.R', port = 3939, host = '0.0.0.0')"