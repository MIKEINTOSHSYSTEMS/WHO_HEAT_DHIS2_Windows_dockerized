#!/bin/bash

APP_DIR="/srv/shiny-server/UI"  # Path to your Shiny app directory
LOG_FILE="/srv/shiny-server/shiny_app_monitor.log"  # Path to a log file

echo "Monitoring Shiny app directory for changes..." >> $LOG_FILE

# Use inotifywait to monitor changes to the UI folder
inotifywait -m -r -e modify,create,delete $APP_DIR |
while read path action file; do
    echo "$(date) - Change detected: $action $file" >> $LOG_FILE
    
    # Restart Shiny app by sending a signal to shiny-server to restart the app
    echo "$(date) - Restarting Shiny app..." >> $LOG_FILE
    systemctl restart shiny-server
    echo "$(date) - Shiny app restarted." >> $LOG_FILE
done
