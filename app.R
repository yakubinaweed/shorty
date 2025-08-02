# app.R

# Source UI and Server logic from separate files
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)