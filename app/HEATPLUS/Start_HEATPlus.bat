start .\R-Portable\App\R-Portable\bin\x64\R.exe -e "options(heat.plus.portable = TRUE, shiny.port = 8912, shiny.launch.browser=FALSE);software_path <- gsub('R-Portable/App/R-Portable/library', 'Software', .libPaths());data_path <- gsub('R-Portable/App/R-Portable/library', 'Data/HEAT-data', .libPaths());.libPaths <- .libPaths(c(.libPaths(), software_path, data_path));suppressPackageStartupMessages({library(shiny);library(heat);library(heatplus)});heatPlusApp(launch.browser=FALSE, port = 8912)"

:CheckPort
netstat -an | findstr ":8912.*LISTENING :8912.*ESTABLISHED"
if %ERRORLEVEL% equ 0 (
  GOTO StartChrome
) else (
  GOTO CheckPort
)

:StartChrome
start .\Chrome-Portable\GoogleChromePortable.exe http://localhost:8912
