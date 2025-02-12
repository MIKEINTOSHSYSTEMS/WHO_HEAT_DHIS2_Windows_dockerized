#!/bin/bash

APP_DIR="/srv/shiny-server/UI"  # Path to your Shiny app directory
LOG_FILE="/srv/shiny-server/shiny_app_monitor.log"  # Path to a log file

# Log rotation: Keep logs for the last 7 days and delete older ones
find /srv/shiny-server/ -name "shiny_app_monitor.log" -mtime +7 -exec rm -f {} \;

echo "$(date) - Monitoring Shiny app directory for changes..." >> $LOG_FILE

# Ensure inotify-tools is installed before proceeding
if ! command -v inotifywait &> /dev/null; then
    echo "$(date) - inotifywait could not be found. Please install inotify-tools." >> $LOG_FILE
    exit 1
fi

# Use inotifywait to monitor changes to the UI folder
inotifywait -m -r -e modify,create,delete $APP_DIR |
while read path action file; do
    echo "$(date) - Change detected: $action $file" >> $LOG_FILE
    
    # Restart Shiny app by sending a signal to shiny-server to restart the app
    echo "$(date) - Restarting Shiny app..." >> $LOG_FILE
    systemctl restart shiny-server

    if [ $? -eq 0 ]; then
        echo "$(date) - Shiny app restarted successfully." >> $LOG_FILE
    else
        echo "$(date) - Error restarting Shiny app." >> $LOG_FILE
    fi

    # Add a small delay before the next restart to avoid excessive restarts (if multiple changes occur quickly)
    sleep 1
done
