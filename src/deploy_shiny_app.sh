#!/bin/bash

# Check and install necessary system dependencies (systemctl, nano, inotify-tools)
echo "Checking system dependencies (systemctl, nano, inotify-tools, libssl-dev, libxml2-dev)..."

for pkg in systemctl nano inotify-tools; do
    if dpkg -l | grep -q "$pkg"; then
        echo "$pkg is already installed."
    else
        echo "$pkg is not installed. Installing..."
        apt-get install -y "$pkg"
    fi
done

# Clean up after installation
apt-get clean

# Shiny app source directory inside the container
APP_SRC="/srv/shiny-server/UI"

echo "Starting Shiny App Deployment Process..."

# Step 1: Copy app files if necessary (if using a mounted volume, this may not be needed)
if [ ! -d "$APP_SRC" ]; then
  echo "Error: Shiny app source directory '$APP_SRC' does not exist."
  exit 1
fi

# Step 2: Install required R libraries only if they are not already installed
R_LIBS=("shiny" "shinyjs" "shinyWidgets" "shinycssloaders" "DT" "httr" "jsonlite" "openxlsx")

echo "Checking R libraries..."

for lib in "${R_LIBS[@]}"; do
    # Check if the library is already installed
    if R -e "if (!require('$lib')) { quit(status = 1) }" > /dev/null 2>&1; then
        echo "$lib is already installed."
    else
        echo "$lib is not installed. Installing..."
        R -e "install.packages('$lib', dependencies = TRUE, repos = 'https://cran.r-project.org/')"
    fi
done

if [ $? -eq 0 ]; then
  echo "R libraries installed successfully."
else
  echo "Error: Failed to install R libraries."
  exit 1
fi

# Step 3: Restart Shiny Server to reflect changes
echo "Restarting Shiny Server..."
systemctl restart shiny-server

# Step 4: Create a file monitoring script to watch for changes in the UI folder
echo "Creating file monitoring script..."

cat << 'EOF' > /srv/shiny-server/monitor_shiny_app.sh
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
EOF

# Step 5: Make the monitoring script executable
chmod +x /srv/shiny-server/monitor_shiny_app.sh

# Step 6: Optionally, you can add the monitoring script to systemd to run on startup
echo "Creating systemd service for Shiny app monitoring..."
cat << 'EOF' > /etc/systemd/system/shiny-app-monitor.service
[Unit]
Description=Shiny App Directory Monitor

[Service]
ExecStart=/srv/shiny-server/monitor_shiny_app.sh
Restart=always

[Install]
WantedBy=multi-user.target
EOF

# Enable and start the systemd service
systemctl enable shiny-app-monitor.service
systemctl start shiny-app-monitor.service

# Step 7: Final confirmation
if [ $? -eq 0 ]; then
  echo "Shiny Server restarted successfully. Monitoring script deployed and service started. Deployment complete!"
else
  echo "Error: Failed to restart Shiny Server or deploy monitoring service."
  exit 1
fi
