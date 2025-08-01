# app.R

# Load necessary libraries
library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(tidyverse)
library(mclust)
library(car)

# Source UI and Server logic from separate files
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
