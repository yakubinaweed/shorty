# R/server_gmm.R

gmmServer <- function(input, output, session, message_rv, analysis_running_rv, gmm_uploaded_data_rv, gmm_processed_data_rv, gmm_transformation_details_rv) {

  # Observer to render the entire UI for the GMM tab
  output$gmm_tab <- renderUI({
    tagList(
      shinyjs::useShinyjs(),
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
    )
  })

  observeEvent(input$gmm_file_upload, {
    req(input$gmm_file_upload)
    tryCatch({
      data <- readxl::read_excel(input$gmm_file_upload$datapath)
      gmm_uploaded_data_rv(data)
      message_rv(list(text = "GMM data uploaded successfully.", type = "success"))
    }, error = function(e) {
      message_rv(list(text = paste("Error reading GMM file:", e$message), type = "error"))
      gmm_uploaded_data_rv(NULL)
    })
  })

  output$gmm_hgb_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_hgb_col", "Select HGB Column:", choices = names(data),
                selected = c("HGB", "hgb", "HB", "hb")[c("HGB", "hgb", "HB", "hb") %in% names(data)][1])
  })

  output$gmm_age_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_age_col", "Select Age Column:", choices = names(data),
                selected = c("Age", "age")[c("Age", "age") %in% names(data)][1])
  })

  output$gmm_gender_col_selector <- renderUI({
    data <- gmm_uploaded_data_rv()
    if (is.null(data)) return(NULL)
    selectInput("gmm_gender_col", "Select Gender Column:", choices = names(data),
                selected = c("Gender", "gender", "Sex", "sex")[c("Gender", "gender", "Sex", "sex") %in% names(data)][1])
  })

  observeEvent(input$run_gmm_analysis_btn, {
    req(gmm_uploaded_data_rv(), input$gmm_hgb_col, input$gmm_age_col, input$gmm_gender_col)

    if (analysis_running_rv()) {
      message_rv(list(text = "An analysis is already running. Please wait.", type = "warning"))
      return(NULL)
    }

    analysis_running_rv(TRUE)
    shinyjs::disable("tabs")

    withProgress(message = 'Running GMM Analysis', value = 0, {
      incProgress(0.1, detail = "Loading data...")

      data <- gmm_uploaded_data_rv()
      hgb_col <- input$gmm_hgb_col
      age_col <- input$gmm_age_col
      gender_col <- input$gmm_gender_col

      if (!all(c(hgb_col, age_col, gender_col) %in% names(data))) {
        message_rv(list(text = "Selected columns not found in data. Please check selections.", type = "error"))
        analysis_running_rv(FALSE)
        shinyjs::enable("tabs")
        return(NULL)
      }

      gmm_data <- data %>%
        dplyr::select(HGB = !!sym(hgb_col), Age = !!sym(age_col), Gender_orig = !!sym(gender_col)) %>%
        na.omit()

      if (nrow(gmm_data) == 0) {
        message_rv(list(text = "No complete rows for GMM after NA removal. Check data or selections.", type = "error"))
        analysis_running_rv(FALSE)
        shinyjs::enable("tabs")
        return(NULL)
      }

      incProgress(0.2, detail = "Splitting data by gender and transforming...")

      gmm_data <- gmm_data %>%
        mutate(Gender = case_when(
          str_detect(Gender_orig, regex("male|m", ignore.case = TRUE)) ~ "Male",
          str_detect(Gender_orig, regex("female|f", ignore.case = TRUE)) ~ "Female",
          TRUE ~ "Other"
        )) %>%
        filter(Gender %in% c("Male", "Female"))

      male_data <- gmm_data %>% filter(Gender == "Male")
      female_data <- gmm_data %>% filter(Gender == "Female")

      combined_clustered_data <- tibble()
      male_hgb_transformed_flag <- FALSE
      female_hgb_transformed_flag <- FALSE

      if (nrow(male_data) > 0) {
        yj_result_male <- apply_conditional_yeo_johnson(male_data$HGB)
        male_data$HGB_transformed <- yj_result_male$transformed_data
        male_hgb_transformed_flag <- yj_result_male$transformation_applied
        male_data$HGB_z <- z_transform(male_data$HGB_transformed)
        male_data$Age_z <- z_transform(male_data$Age)
        incProgress(0.2, detail = "Running GMM for Male data...")
        tryCatch({
          male_gmm_model <- run_gmm(male_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
          male_data <- assign_clusters(male_data, male_gmm_model)
          male_data$cluster <- as.factor(male_data$cluster)
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for male data:", e$message), type = "error"))
        })
        combined_clustered_data <- bind_rows(combined_clustered_data, male_data %>% dplyr::select(HGB, Age, Gender, cluster))
      }

      if (nrow(female_data) > 0) {
        yj_result_female <- apply_conditional_yeo_johnson(female_data$HGB)
        female_data$HGB_transformed <- yj_result_female$transformed_data
        female_hgb_transformed_flag <- yj_result_female$transformation_applied
        female_data$HGB_z <- z_transform(female_data$HGB_transformed)
        female_data$Age_z <- z_transform(female_data$Age)
        incProgress(0.2, detail = "Running GMM for Female data...")
        tryCatch({
          female_gmm_model <- run_gmm(female_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
          female_data <- assign_clusters(female_data, female_gmm_model)
          female_data$cluster <- as.factor(female_data$cluster)
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for female data:", e$message), type = "error"))
        })
        combined_clustered_data <- bind_rows(combined_clustered_data, female_data %>% dplyr::select(HGB, Age, Gender, cluster))
      }

      gmm_transformation_details_rv(list(male_hgb_transformed = male_hgb_transformed_flag, female_hgb_transformed = female_hgb_transformed_flag))

      if (nrow(combined_clustered_data) > 0) {
        gmm_processed_data_rv(combined_clustered_data)
        message_rv(list(text = "GMM analysis complete!", type = "success"))
      } else {
        message_rv(list(text = "No data available after GMM processing for plotting/summary.", type = "error"))
        gmm_processed_data_rv(NULL)
      }

      incProgress(0.1, detail = "Generating plots and summaries...")
    })

    analysis_running_rv(FALSE)
    shinyjs::enable("tabs")
  })

  observeEvent(input$reset_gmm_analysis_btn, {
    gmm_uploaded_data_rv(NULL)
    gmm_processed_data_rv(NULL)
    gmm_transformation_details_rv(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))
    shinyjs::reset("gmm_file_upload")
    output$gmm_results_ui <- renderUI(NULL)
    message_rv(list(text = "GMM data and results reset.", type = "info"))
  })

  output$gmm_results_ui <- renderUI({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    tabBox(
      title = "Subpopulation Detection Results",
      id = "gmm_results_tabs", width = NULL,
      tabPanel("GMM Plot", plotOutput("plot_output_gmm", height = "600px")),
      tabPanel("Summary", verbatimTextOutput("gmm_summary_output")),
      tabPanel("Age Group Summary", tableOutput("gmm_age_group_summary_output"))
    )
  })

  output$plot_output_gmm <- renderPlot({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No GMM data available for plotting.", size = 6, color = "grey50"))
    }
    plot_age_hgb(plot_data,
                 male_hgb_transformed = gmm_transformation_details_rv()$male_hgb_transformed,
                 female_hgb_transformed = gmm_transformation_details_rv()$female_hgb_transformed)
  })

  output$gmm_summary_output <- renderPrint({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return("No GMM analysis results to display.")
    }

    male_summary <- plot_data %>%
      filter(Gender == "Male") %>%
      group_by(cluster) %>%
      summarise(
        Proportion = n() / nrow(.) * 100,
        Mean_HGB = mean(HGB, na.rm = TRUE),
        SD_HGB = sd(HGB, na.rm = TRUE),
        Mean_Age = mean(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Min_Age = min(Age, na.rm = TRUE),
        Max_Age = max(Age, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, ~round(., 2))

    female_summary <- plot_data %>%
      filter(Gender == "Female") %>%
      group_by(cluster) %>%
      summarise(
        Proportion = n() / nrow(.) * 100,
        Mean_HGB = mean(HGB, na.rm = TRUE),
        SD_HGB = sd(HGB, na.rm = TRUE),
        Mean_Age = mean(Age, na.rm = TRUE),
        SD_Age = sd(Age, na.rm = TRUE),
        Min_Age = min(Age, na.rm = TRUE),
        Max_Age = max(Age, na.rm = TRUE)
      ) %>%
      mutate_if(is.numeric, ~round(., 2))

    cat("--- GMM Analysis Summary (Male Subpopulations) ---\n")
    if (nrow(male_summary) > 0) {
      print(male_summary)
    } else {
      cat("No male subpopulations detected.\n")
    }

    cat("\n--- GMM Analysis Summary (Female Subpopulations) ---\n")
    if (nrow(female_summary) > 0) {
      print(female_summary)
    } else {
      cat("No female subpopulations detected.\n")
    }

    if (gmm_transformation_details_rv()$male_hgb_transformed || gmm_transformation_details_rv()$female_hgb_transformed) {
      cat("\nNote: HGB values were transformed (Yeo-Johnson) for GMM input due to skewness. Reported HGB values are original.\n")
    }
  })

  output$gmm_age_group_summary_output <- renderTable({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    age_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, Inf)
    age_labels <- c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

    plot_data %>%
      mutate(age_group_label = cut(Age, breaks = age_bins, labels = age_labels, right = FALSE)) %>%
      group_by(Gender, age_group_label, cluster) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = cluster, values_from = Count, values_fill = 0)
  }, rownames = FALSE)

}
