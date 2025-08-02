# app.R

# Load all necessary libraries.
# This ensures all dependencies are met before the app runs.
library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(tidyverse) # Includes dplyr, ggplot2, tibble, etc.
library(mclust)
library(car)

# Source UI and Server logic from separate files
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
