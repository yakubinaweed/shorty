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

# Source utility functions
source("utils.R")

server <- function(input, output, session) {
  
  # --- Reactive Values for State Management ---
  # Centralized location for all state variables.
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

  # =========================================================================
  # Main Analysis Tab Logic
  # =========================================================================

  # Observer for file upload: reads the uploaded Excel file
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      data <- read_excel(input$data_file$datapath)
      data_reactive(data)
      message_rv(list(type = "success", text = "Data file uploaded and loaded successfully."))

      col_names <- colnames(data)
      all_col_choices_with_none <- c("None" = "", col_names)

      guess_column <- function(cols_available, common_names) {
        for (name in common_names) {
          match_idx <- grep(paste0("^", name, "$"), cols_available, ignore.case = TRUE)
          if (length(match_idx) > 0) {
            return(cols_available[match_idx[1]])
          }
        }
        return("")
      }

      updateSelectInput(session, "col_value", choices = all_col_choices_with_none, selected = guess_column(col_names, c("HB_value", "Value", "Result", "Measurement", "Waarde")))
      updateSelectInput(session, "col_age", choices = all_col_choices_with_none, selected = guess_column(col_names, c("leeftijd", "age", "AgeInYears", "Years")))
      updateSelectInput(session, "col_gender", choices = all_col_choices_with_none, selected = guess_column(col_names, c("geslacht", "gender", "sex", "Gender", "Sex")))
    }, error = function(e) {
      message_rv(list(type = "error", text = paste("Error loading file:", e$message)))
      data_reactive(NULL)
    })
  })

  # Observer for the Reset button on the Main Analysis tab
  observeEvent(input$reset_btn, {
    shinyjs::reset("data_file")
    data_reactive(NULL)
    message_rv(list(type = "", text = ""))
    output$result_text <- renderPrint({ cat("") })
    output$result_plot <- renderPlot(plot.new())

    updateSelectInput(session, "col_value", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "col_age", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "col_gender", choices = c("None" = ""), selected = "")
  })

  # Observer for directory selection using shinyFiles
  shinyFiles::shinyDirChoose(
    input, id = 'select_dir_btn',
    roots = c(home = '~', wd = '.'), session = session
  )

  observeEvent(input$select_dir_btn, {
    if (!is.integer(input$select_dir_btn)) {
      path <- shinyFiles::parseDirPath(c(home = '~', wd = '.'), input$select_dir_btn)
      if (length(path) > 0) {
        selected_dir_reactive(path)
        message_rv(list(type = "success", text = paste("Output directory selected:", path)))
      } else {
        selected_dir_reactive(NULL)
        message_rv(list(type = "warning", text = "Directory selection cancelled."))
      }
    }
  })

  # Observer for the Analyze button
  observeEvent(input$analyze_btn, {
    if (analysis_running_rv()) {
      message_rv(list(text = "Analysis is already running. Please wait or reset.", type = "warning"))
      return()
    }

    req(data_reactive(), input$col_value, input$col_age, input$col_gender)
    if (input$col_value == "" || input$col_age == "" || input$col_gender == "") {
      message_rv(list(text = "Please select all required columns (Value, Age, Gender).", type = "warning"))
      return()
    }

    analysis_running_rv(TRUE)
    message_rv(list(text = "Analysis started...", type = "info"))
    session$sendCustomMessage('analysisStatus', TRUE)

    isolated_inputs <- isolate({
      list(
        gender_choice = input$gender_choice,
        age_range = input$age_range,
        col_value = input$col_value,
        col_age = input$col_age,
        col_gender = input$col_gender,
        nbootstrap_speed = input$nbootstrap_speed,
        unit_input = input$unit_input,
        ref_low = input$ref_low,
        ref_high = input$ref_high,
        enable_directory = input$enable_directory
      )
    })
    
    refiner_model <- NULL
    
    tryCatch({
      filtered_data <- filter_data(data_reactive(), isolated_inputs$gender_choice, isolated_inputs$age_range[1], isolated_inputs$age_range[2], isolated_inputs$col_gender, isolated_inputs$col_age)
      
      if (nrow(filtered_data) == 0) {
        stop("Filtered dataset is empty. Please adjust your filtering criteria.")
      }
      
      nbootstrap_value <- switch(isolated_inputs$nbootstrap_speed, "Fast" = 1, "Medium" = 50, "Slow" = 200, 1)

      refiner_model <- refineR::findRI(Data = filtered_data[[isolated_inputs$col_value]], NBootstrap = nbootstrap_value)
      
      if (is.null(refiner_model) || inherits(refiner_model, "try-error")) {
        stop("RefineR model could not be generated. Check your input data and parameters.")
      }
      
      output$result_text <- renderPrint({
        print(refiner_model)
      })

      output$result_plot <- renderPlot({
        if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
          plot(refiner_model, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
               title = paste("Estimated Reference Intervals"),
               xlab = sprintf("%s [%s]", isolated_inputs$col_value, isolated_inputs$unit_input))

          if (!is.na(isolated_inputs$ref_low)) { abline(v = isolated_inputs$ref_low, col = "red", lty = 2) }
          if (!is.na(isolated_inputs$ref_high)) { abline(v = isolated_inputs$ref_high, col = "blue", lty = 2) }
        } else {
          plot.new()
          text(0.5, 0.5, "No data to plot after filtering.", cex = 1.5)
        }
      })

      if (isolated_inputs$enable_directory && !is.null(selected_dir_reactive())) {
        filename <- generate_safe_filename("RefineR_Plot", selected_dir_reactive(), "png")
        png(filename, width = 800, height = 600)
        plot(refiner_model, showCI = TRUE, RIperc = c(0.025, 0.975), showPathol = FALSE,
             title = paste("Estimated Reference Intervals"),
             xlab = sprintf("%s [%s]", isolated_inputs$col_value, isolated_inputs$unit_input))
        if (!is.na(isolated_inputs$ref_low)) { abline(v = isolated_inputs$ref_low, col = "red", lty = 2) }
        if (!is.na(isolated_inputs$ref_high)) { abline(v = isolated_inputs$ref_high, col = "blue", lty = 2) }
        dev.off()
        message_rv(list(text = paste0("Plot saved to ", selected_dir_reactive()), type = "success"))
      }

      message_rv(list(text = "Analysis complete!", type = "success"))

    }, error = function(e) {
      error_message <- paste("Analysis Error:", e$message)
      message_rv(list(text = error_message, type = "danger"))
      output$result_text <- renderPrint({ cat(error_message) })
      output$result_plot <- renderPlot(plot.new())
      print(error_message)
    }, finally = {
      analysis_running_rv(FALSE)
      session$sendCustomMessage('analysisStatus', FALSE)
    })
  })

  # =========================================================================
  # Subpopulation Detection (GMM) Tab Logic
  # =========================================================================

  # Observer for GMM file upload
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

  # Dynamic UI for GMM column selectors
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

  # Observer for GMM analysis button
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
          grepl("male|m", Gender_orig, ignore.case = TRUE) ~ "Male",
          grepl("female|f", Gender_orig, ignore.case = TRUE) ~ "Female",
          TRUE ~ "Other"
        )) %>%
        filter(Gender %in% c("Male", "Female"))

      male_data <- gmm_data %>% filter(Gender == "Male")
      female_data <- gmm_data %>% filter(Gender == "Female")

      combined_clustered_data <- tibble()
      male_hgb_transformed_flag <- FALSE
      female_hgb_transformed_flag <- FALSE
      male_gmm_model <- NULL
      female_gmm_model <- NULL

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
      
      gmm_models_rv(list(male = male_gmm_model, female = female_gmm_model))
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
    gmm_models_rv(list(male = NULL, female = NULL))
    shinyjs::reset("gmm_file_upload")
    output$gmm_results_ui <- renderUI(NULL)
    message_rv(list(text = "GMM data and results reset.", type = "info"))
  })

  output$gmm_results_ui <- renderUI({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    tagList(
      div(class = "output-box",
          h4("Subpopulation Plot"),
          plotOutput("plot_output_gmm", height = "600px")),
      div(class = "output-box",
          h4("GMM Summary"),
          verbatimTextOutput("gmm_summary_output")),
      div(class = "output-box",
          h4("Cluster Age Group Summary"),
          tableOutput("gmm_age_group_summary_output"))
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
    models <- gmm_models_rv()
    
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return("No GMM analysis results to display.")
    }

    cat("--- GMM Analysis Summary ---\n")
    
    # Process Male data
    if (!is.null(models$male) && !inherits(models$male, "try-error")) {
        cat("\n--- Male Subpopulations ---\n")
        num_clusters <- models$male$G
        for (i in 1:num_clusters) {
            cat(paste0("Cluster ", i, ":\n"))
            cat(paste0("  Proportion: ", round(models$male$parameters$pro[i], 3), "\n"))
            
            # Use the original (untransformed, un-z-scored) means and sds for display
            male_cluster_data <- plot_data %>% filter(Gender == "Male", cluster == i)
            mean_hgb <- mean(male_cluster_data$HGB, na.rm = TRUE)
            mean_age <- mean(male_cluster_data$Age, na.rm = TRUE)
            sd_hgb <- sd(male_cluster_data$HGB, na.rm = TRUE)
            sd_age <- sd(male_cluster_data$Age, na.rm = TRUE)
            
            cat(paste0("  Mean HGB: ", round(mean_hgb, 3), "\n"))
            cat(paste0("  Mean Age: ", round(mean_age, 3), "\n"))
            cat(paste0("  Std Dev HGB: ", round(sd_hgb, 3), "\n"))
            cat(paste0("  Std Dev Age: ", round(sd_age, 3), "\n"))
            
            # Estimated age range
            if (!is.na(sd_age)) {
              lower_age <- round(mean_age - 2 * sd_age, 1)
              upper_age <- round(mean_age + 2 * sd_age, 1)
              cat(paste0("  Estimated Age Range (Mean +/- 2SD): [", max(0, lower_age), " to ", upper_age, "] years\n"))
            } else {
              cat("  Estimated Age Range: N/A (Std Dev Age problematic)\n")
            }
            cat("\n")
        }
    } else {
        cat("No male subpopulations detected.\n")
    }
    
    # Process Female data
    if (!is.null(models$female) && !inherits(models$female, "try-error")) {
        cat("\n--- Female Subpopulations ---\n")
        num_clusters <- models$female$G
        for (i in 1:num_clusters) {
            cat(paste0("Cluster ", i, ":\n"))
            cat(paste0("  Proportion: ", round(models$female$parameters$pro[i], 3), "\n"))
            
            # Use the original (untransformed, un-z-scored) means and sds for display
            female_cluster_data <- plot_data %>% filter(Gender == "Female", cluster == i)
            mean_hgb <- mean(female_cluster_data$HGB, na.rm = TRUE)
            mean_age <- mean(female_cluster_data$Age, na.rm = TRUE)
            sd_hgb <- sd(female_cluster_data$HGB, na.rm = TRUE)
            sd_age <- sd(female_cluster_data$Age, na.rm = TRUE)
            
            cat(paste0("  Mean HGB: ", round(mean_hgb, 3), "\n"))
            cat(paste0("  Mean Age: ", round(mean_age, 3), "\n"))
            cat(paste0("  Std Dev HGB: ", round(sd_hgb, 3), "\n"))
            cat(paste0("  Std Dev Age: ", round(sd_age, 3), "\n"))
            
            # Estimated age range
            if (!is.na(sd_age)) {
              lower_age <- round(mean_age - 2 * sd_age, 1)
              upper_age <- round(mean_age + 2 * sd_age, 1)
              cat(paste0("  Estimated Age Range (Mean +/- 2SD): [", max(0, lower_age), " to ", upper_age, "] years\n"))
            } else {
              cat("  Estimated Age Range: N/A (Std Dev Age problematic)\n")
            }
            cat("\n")
        }
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