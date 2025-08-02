# server_gmm.R

gmmServer <- function(input, output, session, gmm_uploaded_data_rv, gmm_processed_data_rv, gmm_transformation_details_rv, gmm_models_rv, message_rv, analysis_running_rv) {

  # Observer for GMM file upload
  observeEvent(input$gmm_file_upload, {
    req(input$gmm_file_upload)
    tryCatch({
      data <- readxl::read_excel(input$gmm_file_upload$datapath)
      gmm_uploaded_data_rv(data)
      message_rv(list(text = "GMM data uploaded successfully.", type = "success"))

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

      updateSelectInput(session, "gmm_hgb_col", choices = all_col_choices_with_none, selected = guess_column(col_names, c("HGB", "hgb", "HB", "hb")))
      updateSelectInput(session, "gmm_age_col", choices = all_col_choices_with_none, selected = guess_column(col_names, c("Age", "age", "leeftijd")))
      updateSelectInput(session, "gmm_gender_col", choices = all_col_choices_with_none, selected = guess_column(col_names, c("Gender", "gender", "Sex", "sex", "geslacht")))
      
    }, error = function(e) {
      message_rv(list(text = paste("Error reading GMM file:", e$message), type = "error"))
      gmm_uploaded_data_rv(NULL)
    })
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
      
      model_criterion <- isolate(input$gmm_model_criterion)

      if (nrow(male_data) > 0) {
        yj_result_male <- apply_conditional_yeo_johnson(male_data$HGB)
        male_data$HGB_transformed <- yj_result_male$transformed_data
        male_hgb_transformed_flag <- yj_result_male$transformation_applied
        male_data$HGB_z <- z_transform(male_data$HGB_transformed)
        male_data$Age_z <- z_transform(male_data$Age)
        incProgress(0.2, detail = "Running GMM for Male data...")
        tryCatch({
          male_gmm_model <- run_gmm_with_criterion(male_data %>% dplyr::select(HGB = HGB_z, Age = Age_z), criterion = model_criterion)
          if (!is.null(male_gmm_model)) {
            male_data <- assign_clusters(male_data, male_gmm_model)
            male_data$cluster <- as.factor(male_data$cluster)
          }
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for male data:", e$message), type = "error"))
        })
        # Only combine if clusters were assigned
        if (!is.null(male_gmm_model)) {
          combined_clustered_data <- bind_rows(combined_clustered_data, male_data %>% dplyr::select(HGB, Age, Gender, cluster))
        }
      }

      if (nrow(female_data) > 0) {
        yj_result_female <- apply_conditional_yeo_johnson(female_data$HGB)
        female_data$HGB_transformed <- yj_result_female$transformed_data
        female_hgb_transformed_flag <- yj_result_female$transformation_applied
        female_data$HGB_z <- z_transform(female_data$HGB_transformed)
        female_data$Age_z <- z_transform(female_data$Age)
        incProgress(0.2, detail = "Running GMM for Female data...")
        tryCatch({
          female_gmm_model <- run_gmm_with_criterion(female_data %>% dplyr::select(HGB = HGB_z, Age = Age_z), criterion = model_criterion)
          if (!is.null(female_gmm_model)) {
            female_data <- assign_clusters(female_data, female_gmm_model)
            female_data$cluster <- as.factor(female_data$cluster)
          }
        }, error = function(e) {
          message_rv(list(text = paste("Error running GMM for female data:", e$message), type = "error"))
        })
        # Only combine if clusters were assigned
        if (!is.null(female_gmm_model)) {
          combined_clustered_data <- bind_rows(combined_clustered_data, female_data %>% dplyr::select(HGB, Age, Gender, cluster))
        }
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
    
    # Reset column selectors
    updateSelectInput(session, "gmm_hgb_col", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "gmm_age_col", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "gmm_gender_col", choices = c("None" = ""), selected = "")
  })

  output$gmm_results_ui <- renderUI({
    plot_data <- gmm_processed_data_rv()
    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(NULL)
    }

    tagList(
      div(class = "output-box",
          h4("Model Selection Plot"),
          plotOutput("gmm_model_selection_plot", height = "400px")),
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

  output$gmm_model_selection_plot <- renderPlot({
    models <- gmm_models_rv()
    if (is.null(models$male) && is.null(models$female)) {
      return(ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No GMM models available for plotting.", size = 6, color = "grey50"))
    }
    
    criterion_selected <- isolate(input$gmm_model_criterion)
    
    if (!is.null(models$male) && !is.null(models$female)) {
      par(mfrow = c(1, 2))
      if (criterion_selected == "BIC") {
        plot(models$male, what = "BIC", main = "Male - BIC Plot")
        plot(models$female, what = "BIC", main = "Female - BIC Plot")
      } else {
        plot(mclustICL(models$male), main = "Male - ICL Plot")
        plot(mclustICL(models$female), main = "Female - ICL Plot")
      }
      par(mfrow = c(1, 1))
    } else {
      if (!is.null(models$male)) {
        if (criterion_selected == "BIC") {
          plot(models$male, what = "BIC", main = "Male - BIC Plot")
        } else {
          plot(mclustICL(models$male), main = "Male - ICL Plot")
        }
      } else if (!is.null(models$female)) {
        if (criterion_selected == "BIC") {
          plot(models$female, what = "BIC", main = "Female - BIC Plot")
        } else {
          plot(mclustICL(models$female), main = "Female - ICL Plot")
        }
      }
    }
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
    
    if (!is.null(models$male) && !inherits(models$male, "try-error")) {
        cat("\n--- Male Subpopulations ---\n")
        
        print(summary(models$male))

        num_clusters <- models$male$G
        for (i in 1:num_clusters) {
            cat(paste0("Cluster ", i, ":\n"))
            cat(paste0("  Proportion: ", round(models$male$parameters$pro[i], 3), "\n"))
            
            male_cluster_data <- plot_data %>% filter(Gender == "Male", cluster == i)
            mean_hgb <- mean(male_cluster_data$HGB, na.rm = TRUE)
            mean_age <- mean(male_cluster_data$Age, na.rm = TRUE)
            sd_hgb <- sd(male_cluster_data$HGB, na.rm = TRUE)
            sd_age <- sd(male_cluster_data$Age, na.rm = TRUE)
            
            cat(paste0("  Mean HGB: ", round(mean_hgb, 3), "\n"))
            cat(paste0("  Mean Age: ", round(mean_age, 3), "\n"))
            cat(paste0("  Std Dev HGB: ", round(sd_hgb, 3), "\n"))
            cat(paste0("  Std Dev Age: ", round(sd_age, 3), "\n"))
            
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
    
    if (!is.null(models$female) && !inherits(models$female, "try-error")) {
        cat("\n--- Female Subpopulations ---\n")
        
        print(summary(models$female))

        num_clusters <- models$female$G
        for (i in 1:num_clusters) {
            cat(paste0("Cluster ", i, ":\n"))
            cat(paste0("  Proportion: ", round(models$female$parameters$pro[i], 3), "\n"))
            
            female_cluster_data <- plot_data %>% filter(Gender == "Female", cluster == i)
            mean_hgb <- mean(female_cluster_data$HGB, na.rm = TRUE)
            mean_age <- mean(female_cluster_data$Age, na.rm = TRUE)
            sd_hgb <- sd(female_cluster_data$HGB, na.rm = TRUE)
            sd_age <- sd(female_cluster_data$Age, na.rm = TRUE)
            
            cat(paste0("  Mean HGB: ", round(mean_hgb, 3), "\n"))
            cat(paste0("  Mean Age: ", round(mean_age, 3), "\n"))
            cat(paste0("  Std Dev HGB: ", round(sd_hgb, 3), "\n"))
            cat(paste0("  Std Dev Age: ", round(sd_age, 3), "\n"))
            
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