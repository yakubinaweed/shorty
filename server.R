# server.R

# Load all necessary libraries.
library(shiny)
library(readxl)
library(tidyverse)
library(mclust)
library(moments)
library(shinyjs)
library(car)
library(refineR)
library(shinyFiles)
library(shinyWidgets)
library(bslib)
library(ggplot2)

# Source all server modules and utility functions
source("utils.R")
source("server_main.R")
source("server_gmm.R")

server <- function(input, output, session) {
  
  # --- Reactive Values for State Management (Centralized) ---
  data_reactive <- reactiveVal(NULL)
  gmm_uploaded_data_rv <- reactiveVal(NULL)
  gmm_processed_data_rv <- reactiveVal(NULL)
  gmm_transformation_details_rv <- reactiveVal(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))
  gmm_models_rv <- reactiveVal(list(male = NULL, female = NULL))
  selected_dir_reactive <- reactiveVal(NULL)
  message_rv <- reactiveVal(list(type = "", text = ""))
  analysis_running_rv <- reactiveVal(FALSE)

  # --- Centralized Message Display ---
  output$app_message <- renderUI({
    msg <- message_rv()
    if (is.null(msg) || msg$text == "") {
      return(NULL)
    }
    class_name <- switch(msg$type,
                         "error" = "alert alert-danger",
                         "success" = "alert alert-success",
                         "warning" = "alert alert-warning",
                         "info" = "alert alert-info",
                         "alert alert-secondary")
    div(class = class_name, msg$text)
  })

  # --- Helper to clear messages ---
  clear_messages <- function() {
    message_rv(list(type = "", text = ""))
  }

  # --- Global Tab Switching Logic ---
  observeEvent(input$tabs, {
    if (!analysis_running_rv()) {
      clear_messages()
    } else {
      message_rv(list(text = "Tab switch blocked: An analysis is currently running. Please wait or reset the analysis.", type = "warning"))
    }
  })

  observeEvent(input$tab_switch_blocked, {
    if (analysis_running_rv()) {
      message_rv(list(text = "Cannot switch tabs while an analysis is running. Please wait or reset the analysis.", type = "warning"))
    }
  })
  
  # Call the modular server functions
  mainServer(input, output, session, data_reactive, selected_dir_reactive, message_rv, analysis_running_rv)
  gmmServer(input, output, session, gmm_uploaded_data_rv, gmm_processed_data_rv, gmm_transformation_details_rv, gmm_models_rv, message_rv, analysis_running_rv)
}