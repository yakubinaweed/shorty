# R/server.R (Complete & Consolidated Script)

# Load all necessary libraries
library(shiny)
library(readxl)
library(tidyverse) # Includes dplyr, ggplot2, tibble, etc.
library(mclust) # For GMM analysis
library(moments) # For skewness calculation
library(shinyjs) # For UI manipulations
library(car) # Required for powerTransform (Yeo-Johnson)
library(refineR) # Explicitly load refineR here
library(shinyFiles)
library(shinyWidgets)
library(bslib)


# =========================================================================
# UTILITY FUNCTIONS
# =========================================================================

# Function to filter data based on gender and age
filter_data <- function(data, gender_choice, age_min, age_max, col_gender, col_age) {
  if (!col_gender %in% names(data) || !col_age %in% names(data)) {
    stop("Gender or age column not found in data.")
  }

  filtered_data <- data %>%
    filter(!!sym(col_age) >= age_min & !!sym(col_age) <= age_max)

  if (gender_choice != "Both") {
    filtered_data <- filtered_data %>%
      filter(grepl(gender_choice, !!sym(col_gender), ignore.case = TRUE))
  }

  return(filtered_data)
}

# Function to generate a safe filename for plots
generate_safe_filename <- function(plot_title, base_path, extension = "png") {
  safe_title <- gsub("[^a-zA-Z0-9_-]", "_", plot_title)
  datestamp <- format(Sys.Date(), "%Y%m%d")
  timestamp <- format(Sys.time(), "%H%M%S")
  file.path(base_path, paste0(safe_title, "_", datestamp, "-", timestamp, ".", extension))
}

# Z-transform a numeric vector (standardization)
z_transform <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) {
    return(rep(0, length(x)))
  }
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Conditional Yeo-Johnson Transformation
apply_conditional_yeo_johnson <- function(data_vector, skewness_threshold = 0.5) {
  transformed_data <- data_vector
  transformation_applied <- FALSE
  skew <- moments::skewness(data_vector, na.rm = TRUE)

  if (abs(skew) > skewness_threshold) {
    tryCatch({
      pt_result <- powerTransform(data_vector)
      lambda <- pt_result$lambda
      transformed_data <- car::yjPower(data_vector, lambda)
      transformation_applied <- TRUE
      message(paste("Yeo-Johnson transformation applied (skewness=", round(skew, 2), ")"))
    }, error = function(e) {
      warning(paste("Could not apply Yeo-Johnson transformation:", e$message))
    })
  } else {
    message(paste("Yeo-Johnson transformation not needed (skewness=", round(skew, 2), ")"))
  }

  return(list(transformed_data = transformed_data, transformation_applied = transformation_applied))
}

# Function to run GMM analysis using mclust
run_gmm <- function(data_mat, G_range = 2:5) {
  if (!is.matrix(data_mat) && !is.data.frame(data_mat)) {
    stop("Input data_mat must be a matrix or data frame for GMM analysis.")
  }
  if (!all(sapply(data_mat, is.numeric))) {
    stop("All columns in data_mat must be numeric.")
  }
  if (any(is.na(data_mat))) {
    stop("Input data_mat contains NA values. Please remove or impute before clustering.")
  }

  multivariate_model_names <- c("EII", "VII", "EEE", "VEE")
  tryCatch({
    gmm_model <- Mclust(data_mat, G = G_range, modelNames = multivariate_model_names)
    return(gmm_model)
  }, error = function(e) {
    stop(paste("GMM Mclust Error:", e$message))
  })
}

# Function to assign clusters back to the original data frame
assign_clusters <- function(df, gmm_model) {
  if (is.null(gmm_model) || is.null(gmm_model$classification)) {
    warning("GMM model or classification is NULL. Cannot assign clusters.")
    return(df)
  }
  df$cluster <- gmm_model$classification
  return(df)
}

# Function to plot age vs HGB colored by cluster
plot_age_hgb <- function(df, male_hgb_transformed, female_hgb_transformed) {
  if (is.null(df) || nrow(df) == 0) {
    return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No GMM data available for plotting.", size = 6, color = "grey50"))
  }

  plot_title <- "HGB vs Age by Subpopulation Cluster"
  plot_subtitle <- ""
  if (male_hgb_transformed) {
    plot_subtitle <- paste(plot_subtitle, "Male HGB transformed for GMM.", sep="\n")
  }
  if (female_hgb_transformed) {
    plot_subtitle <- paste(plot_subtitle, "Female HGB transformed for GMM.", sep="\n")
  }

  ggplot(df, aes(x = Age, y = HGB, color = factor(cluster))) +
    geom_point(alpha = 0.7) +
    facet_wrap(~Gender) +
    theme_minimal() +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         x = "Age", y = "HGB", color = "Cluster") +
    theme(legend.position = "bottom")
}

# Function to apply universal plausibility limits
apply_universal_plausibility_limits <- function(data_df) {
  filtered_data <- data_df %>%
    filter(HGB >= 5 & HGB <= 20) %>% # Example HGB range
    filter(Age >= 0) # Example non-negative age

  if (nrow(filtered_data) < nrow(data_df)) {
    warning(paste(nrow(data_df) - nrow(filtered_data), "rows removed due to plausibility limits."))
  }
  return(filtered_data)
}


# =========================================================================
# MAIN SHINY SERVER LOGIC
# =========================================================================
server <- function(input, output, session) {
  
  # --- Reactive Values for State Management ---
  data_reactive <- reactiveVal(NULL)
  gmm_uploaded_data_rv <- reactiveVal(NULL)
  gmm_processed_data_rv <- reactiveVal(NULL)
  gmm_transformation_details_rv <- reactiveVal(list(male_hgb_transformed = FALSE, female_hgb_transformed = FALSE))
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
  clear_messages <- function(message_rv) {
    message_rv(list(type = "", text = ""))
  }

  # =========================================================================
  # Main Analysis Tab (Window 1) Observers
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
      
      # Check if filtered data is empty
      if (nrow(filtered_data) == 0) {
        stop("Filtered dataset is empty. Please adjust your filtering criteria.")
      }
      
      nbootstrap_value <- switch(isolated_inputs$nbootstrap_speed, "Fast" = 1, "Medium" = 50, "Slow" = 200, 1)

      refiner_model <- refineR::findRI(Data = filtered_data[[isolated_inputs$col_value]], NBootstrap = nbootstrap_value)
      
      # Check if the model was successfully generated
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
      print(error_message) # Print to console for debugging
    }, finally = {
      analysis_running_rv(FALSE)
      session$sendCustomMessage('analysisStatus', FALSE)
    })
  })

  # =========================================================================
  # Subpopulation Detection (GMM) Tab (Window 2) Observers
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
    output$gmm_results_ui <- renderUI(NULL) # Clear the results UI
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
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return("No GMM analysis results to display.")
    }

    gmm_model <- gmm_processed_data_rv()$gmm_model

    cat("--- GMM Analysis Summary ---\n")
    if (!is.null(gmm_model) && !inherits(gmm_model, "try-error")) {
        print(summary(gmm_model))
        cat("\n")
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
