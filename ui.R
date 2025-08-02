library(shiny)
library(bslib)
library(refineR)
library(readxl)
library(moments)
library(shinyjs)
library(shinyWidgets)
library(shinyFiles)
library(ggplot2)

ui <- navbarPage(
  title = "RefineR Reference Interval Estimation",
  id = "tabs",
  theme = bs_theme(version = 4, base_font = font_google("Inter"), heading_font = font_google("Rethink Sans"), font_scale = 1.1, bootswatch = "default"),

  tabPanel(
    title = "Main Analysis",
    useShinyjs(),
    tags$head(
      includeCSS("www/styles.css")
    ),
    tags$script(HTML("
      var analysisRunning = false;
      Shiny.addCustomMessageHandler('analysisStatus', function(status) {
        analysisRunning = status;
        if (status) {
          $('a[data-value=\"Subpopulation Detection (GMM)\"]').attr('data-toggle', 'disabled').addClass('disabled-tab-link');
        } else {
          $('a[data-value=\"Subpopulation Detection (GMM)\"]').attr('data-toggle', 'tab').removeClass('disabled-tab-link');
        }
      });
      $(document).on('click', 'a[data-toggle=\"tab\"]', function(event) {
        if (analysisRunning) {
          event.preventDefault();
          Shiny.setInputValue('tab_switch_blocked', new Date().getTime());
          return false;
        }
      });
    ")),
    sidebarLayout(
      sidebarPanel(
        style = "padding-right: 15px;",
        selectInput(inputId = "gender_choice", label = "Select Gender:", choices = c("Male" = "M", "Female" = "F", "Both" = "Both"), selected = "Both"),
        sliderInput(inputId = "age_range", label = "Age Range:", min = 0, max = 100, value = c(0, 100), step = 1),
        fileInput(inputId = "data_file", label = "Upload Data (Excel File)", accept = c(".xlsx")),
        selectInput(inputId = "col_value", label = "Select Column for Values:", choices = c("None" = ""), selected = ""),
        selectInput(inputId = "col_age", label = "Select Column for Age:", choices = c("None" = ""), selected = ""),
        selectInput(inputId = "col_gender", label = "Select Column for Gender:", choices = c("None" = ""), selected = ""),
        radioButtons(inputId = "nbootstrap_speed", label = "Select Computation Speed:", choices = c("Fast", "Medium", "Slow"), selected = "Fast", inline = TRUE),
        actionButton("analyze_btn", "Analyze", class = "btn-primary"),
        actionButton("reset_btn", "Reset File", class = "btn-secondary"),
        shinyFiles::shinyDirButton(id = "select_dir_btn", label = "Select Output Directory", title = "Select a directory to save plots", style = "margin-top: 5px;"),
        div(style = "margin-top: 5px; display: flex; align-items: center; justify-content: flex-start; width: 100%;",
            prettySwitch(inputId = "enable_directory", label = "Auto-Save Graph", status = "success", fill = TRUE, inline = TRUE)
        ),
        uiOutput("app_message"),
        hr(),
        numericInput("ref_low", "Reference Lower Limit:", value = NA),
        numericInput("ref_high", "Reference Upper Limit:", value = NA),
        textInput(inputId = "unit_input", label = "Unit of Measurement", value = "mmol/L", placeholder = "ex. g/L")
      ),
      mainPanel(
        style = "padding-left: 15px;",
        plotOutput("result_plot"),
        verbatimTextOutput("result_text")
      )
    )
  ),

  tabPanel(
    title = "Subpopulation Detection (GMM)",
    useShinyjs(),
    h4("Detect Subpopulations using HGB and Age"),
    p("Gaussian Mixture Models aim to detect hidden subpopulations within your data based on HGB and Age. The system will automatically select the best number of components (between 2 and 5) using the Bayesian Information Criterion (BIC), where the lowest BIC value indicates the optimal fit. For each detected subpopulation, estimated age ranges will be provided directly from the model's characteristics, avoiding predefined bins. While increasing the number of components can improve model fit, it also increases the risk of overfitting, where the model learns noise rather than true underlying patterns."),
    fluidRow(
      column(4,
             fileInput(inputId = "gmm_file_upload", label = "Upload Data (Excel File)", accept = c(".xlsx")),
             uiOutput("gmm_hgb_col_selector"),
             uiOutput("gmm_age_col_selector"),
             uiOutput("gmm_gender_col_selector"),
             actionButton("run_gmm_analysis_btn", "Run Subpopulation Detection", class = "btn-primary"),
             actionButton("reset_gmm_analysis_btn", "Reset GMM Data", class = "btn-secondary")
      ),
      column(8,
             uiOutput("gmm_results_ui")
      )
    )
  ),

  footer = tags$footer(
    HTML('© 2025 <a href="https://github.com/yakubinaweed/refineR-reference-interval" target="_blank">Naweed Yakubi</a> • All rights reserved.'),
    style = "
      position: relative;
      bottom: 0;
      width: 100%;
      text-align: center;
      padding: 10px 0;
      color: #777;
      font-size: 0.8em;"
  )
)
