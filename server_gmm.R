# server_gmm.R

gmmServer <- function(input, output, session,
                      gmm_uploaded_data_rv,
                      gmm_processed_data_rv,
                      gmm_transformation_details_rv,
                      gmm_models_rv,
                      message_rv,
                      analysis_running_rv) {

  # --- Helper Functions ---
  # Applies Yeo-Johnson transformation if data is significantly skewed
  apply_conditional_yeo_johnson <- function(x) {
    # Check if the input is numeric and has enough non-missing data for skewness calculation
    if (!is.numeric(x) || sum(!is.na(x)) < 2) {
      warning("Input for skewness calculation is not a valid numeric vector. Returning original data.")
      return(list(transformed_data = x, transformation_applied = FALSE))
    }
    
    skew <- tryCatch({
      moments::skewness(x, na.rm = TRUE)
    }, error = function(e) {
      message("Error calculating skewness: ", e$message)
      return(NA) # Return NA if skewness calculation fails
    })
    
    if (is.na(skew) || abs(skew) <= 0.5) {
      list(transformed_data = x, transformation_applied = FALSE)
    } else {
      transformed_data <- car::yjPower(x, lambda = "auto")
      list(transformed_data = transformed_data, transformation_applied = TRUE)
    }
  }

  # Standardizes data by converting to z-scores
  z_transform <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }

  # Runs the GMM model, now always using BIC
  run_gmm_with_criterion <- function(data) {
    Mclust(data)
  }

  # Assigns cluster classifications to the original data
  assign_clusters <- function(data, model) {
    data$cluster <- as.factor(model$classification)
    data
  }

  # Helper function to guess column names based on common keywords
  guess_column <- function(cols_available, common_names) {
    for (name in common_names) {
      match_idx <- grep(paste0("^", name, "$"), cols_available, ignore.case = TRUE)
      if (length(match_idx) > 0) {
        return(cols_available[match_idx[1]])
      }
    }
    return("")
  }

  # --- Observers for File Upload and Reset ---
  
  # Observer for file upload: reads the uploaded Excel file and updates column selectors
  observeEvent(input$gmm_file_upload, {
    req(input$gmm_file_upload)
    tryCatch({
      data <- read_excel(input$gmm_file_upload$datapath)
      gmm_uploaded_data_rv(data)
      message_rv(list(type = "success", text = "Data file uploaded and loaded successfully."))

      col_names <- colnames(data)
      all_col_choices_with_none <- c("None" = "", col_names)

      # Automatically select likely columns for GMM
      updateSelectInput(session, "gmm_hgb_col", choices = all_col_choices_with_none, selected = guess_column(col_names, c("HGB", "Hemoglobin")))
      updateSelectInput(session, "gmm_age_col", choices = all_col_choices_with_none, selected = guess_column(col_names, c("leeftijd", "age", "AgeInYears", "Years")))
      updateSelectInput(session, "gmm_gender_col", choices = all_col_choices_with_none, selected = guess_column(col_names, c("geslacht", "gender", "sex", "Gender", "Sex")))

    }, error = function(e) {
      message_rv(list(type = "error", text = paste("Error loading file:", e$message)))
      gmm_uploaded_data_rv(NULL)
    })
  })

  # Observer for the Reset button
  observeEvent(input$reset_gmm_analysis_btn, {
    # Reset all reactive values and inputs
    shinyjs::reset("gmm_file_upload")
    gmm_uploaded_data_rv(NULL)
    gmm_processed_data_rv(NULL)
    gmm_models_rv(list(selected = NULL))
    gmm_transformation_details_rv(list(selected_hgb_transformed = FALSE))
    message_rv(list(type = "", text = ""))
    
    # Clear the column selections
    updateSelectInput(session, "gmm_hgb_col", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "gmm_age_col", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "gmm_gender_col", choices = c("None" = ""), selected = "")

    # Clear the output UI
    output$gmm_results_ui <- renderUI(NULL)
  })


  # --- Main Observer for GMM Analysis ---
  observeEvent(input$run_gmm_analysis_btn, {
    req(gmm_uploaded_data_rv())
    analysis_running_rv(TRUE)
    message_rv(NULL)
    
    # Disable run button to prevent multiple analyses at once
    shinyjs::disable("run_gmm_analysis_btn")
    shinyjs::runjs("$('#run_gmm_analysis_btn').text('Analyzing...');")

    # Read and validate the uploaded file
    data <- gmm_uploaded_data_rv()
    req(all(c(input$gmm_hgb_col, input$gmm_age_col, input$gmm_gender_col) %in% names(data)))

    withProgress(message = "Running GMM clustering...", value = 0, {
      # --- Step 1: Data Preprocessing and Gender Splitting ---
      incProgress(0.1, detail = "Preprocessing data...")
      gmm_data <- data %>%
        dplyr::select(
          HGB = !!sym(input$gmm_hgb_col),
          Age = !!sym(input$gmm_age_col),
          Gender_orig = !!sym(input$gmm_gender_col)
        ) %>%
        # Replace commas with periods for numeric conversion, then convert to numeric
        dplyr::mutate(
          HGB = as.numeric(gsub(",", ".", HGB)),
          Age = as.numeric(gsub(",", ".", Age))
        ) %>%
        tidyr::drop_na() %>%
        dplyr::mutate(Gender = dplyr::case_when(
          grepl("male|m", Gender_orig, ignore.case = TRUE) ~ "Male",
          grepl("female|f", Gender_orig, ignore.case = TRUE) ~ "Female",
          TRUE ~ "Other"
        ))
      
      # Validate that the converted columns are now numeric and have data
      # Check if the data has enough non-NA values to proceed
      if (!is.numeric(gmm_data$HGB) || !is.numeric(gmm_data$Age) || nrow(gmm_data) < 2) {
        message_rv(list(text = "The selected HGB or Age columns contain non-numeric data, or the dataset is too small after cleaning. Please check your data.", type = "danger"))
        analysis_running_rv(FALSE)
        shinyjs::enable("run_gmm_analysis_btn")
        shinyjs::runjs("$('#run_gmm_analysis_btn').text('Run Subpopulation Detection');")
        return()
      }
      
      # Filter the data based on the user's choice
      selected_gender_choice <- input$gmm_gender_choice
      if (selected_gender_choice == "Male") {
        subset_data <- gmm_data %>% dplyr::filter(Gender == "Male")
      } else if (selected_gender_choice == "Female") {
        subset_data <- gmm_data %>% dplyr::filter(Gender == "Female")
      } else {
        subset_data <- gmm_data # For "Combined"
      }
      
      if (nrow(subset_data) < 2) {
        message_rv(list(text = "Insufficient data for selected gender group.", type = "danger"))
        analysis_running_rv(FALSE)
        shinyjs::enable("run_gmm_analysis_btn")
        shinyjs::runjs("$('#run_gmm_analysis_btn').text('Run Subpopulation Detection');")
        return()
      }
      
      # --- Step 2: HGB Data Transformation ---
      incProgress(0.3, detail = "Transforming HGB data...")
      
      # Check if the HGB column in the subset has enough data to calculate skewness
      if (sum(!is.na(subset_data$HGB)) < 2) {
          message_rv(list(text = "Insufficient non-missing HGB data in the selected group to run GMM.", type = "danger"))
          analysis_running_rv(FALSE)
          shinyjs::enable("run_gmm_analysis_btn")
          shinyjs::runjs("$('#run_gmm_analysis_btn').text('Run Subpopulation Detection');")
          return()
      }

      yj <- apply_conditional_yeo_johnson(subset_data$HGB)
      subset_data$HGB_transformed <- yj$transformed_data
      subset_data$HGB_z <- z_transform(subset_data$HGB_transformed)
      subset_data$Age_z <- z_transform(subset_data$Age)
      
      # --- Step 3: Automated GMM Fitting and Model Selection ---
      incProgress(0.5, detail = "Fitting GMM model...")
      model <- tryCatch({
        run_gmm_with_criterion(subset_data %>% dplyr::select(HGB = HGB_z, Age = Age_z))
      }, error = function(e) {
        message_rv(list(text = paste("Error in GMM model:", e$message), type = "error"))
        NULL
      })
      
      if (!is.null(model)) {
        subset_data <- assign_clusters(subset_data, model)
      } else {
        subset_data$cluster <- NA
      }

      # Store results in reactives
      gmm_processed_data_rv(subset_data %>% dplyr::mutate(Subset = selected_gender_choice))
      
      # Store the single selected model
      gmm_models_rv(list(selected = model))

      gmm_transformation_details_rv(list(selected_hgb_transformed = yj$transformation_applied))
      
      incProgress(1, detail = "Analysis complete!")
    })

    analysis_running_rv(FALSE)
    shinyjs::enable("run_gmm_analysis_btn")
    shinyjs::runjs("$('#run_gmm_analysis_btn').text('Run Subpopulation Detection');")
    message_rv(list(text = paste0("GMM analysis complete for '", input$gmm_gender_choice, "' group."), type = "success"))
  })

  # --- Output Plots and Tables ---

  # Renders the single Model Selection plot
  output$gmm_model_selection_plot <- renderPlot({
    req(gmm_models_rv()$selected)
    model <- gmm_models_rv()$selected
    
    title <- paste(input$gmm_gender_choice, " - BIC Model Selection Plot")
    
    plot(model, what = "BIC", main = title)
  })

  # Renders the single Cluster plot
  output$gmm_cluster_plot <- renderPlot({
    req(gmm_processed_data_rv())
    data <- gmm_processed_data_rv()

    ggplot2::ggplot(data, aes(x = Age, y = HGB, color = cluster)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::stat_ellipse(type = "norm", level = 0.95, aes(group = cluster)) +
      ggplot2::labs(
        title = "GMM Clusters",
        subtitle = paste("Subset:", input$gmm_gender_choice)
      ) +
      ggplot2::theme_minimal()
  })

  # Renders the summary table for the selected group
  output$gmm_summary_table <- DT::renderDT({
    req(gmm_processed_data_rv())
    data <- gmm_processed_data_rv()

    summary_table <- data %>%
      dplyr::group_by(Subset, cluster) %>%
      dplyr::summarise(
        Count = dplyr::n(),
        Mean_HGB = round(mean(HGB, na.rm = TRUE), 2),
        SD_HGB = round(sd(HGB, na.rm = TRUE), 2),
        Mean_Age = round(mean(Age, na.rm = TRUE), 2),
        SD_Age = round(sd(Age, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(Subset, cluster)

    DT::datatable(summary_table, options = list(pageLength = 10))
  })

  # Renders the final UI for the results
  output$gmm_results_ui <- renderUI({
    req(gmm_models_rv()$selected)
    div(
      h4("Model Selection and Clustering Results"),
      hr(),
      plotOutput("gmm_model_selection_plot"),
      hr(),
      plotOutput("gmm_cluster_plot"),
      hr(),
      h4("Cluster Summary Table"),
      DT::DTOutput("gmm_summary_table")
    )
  })
}
