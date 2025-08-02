# Observer for file input in the Second Tab
  observeEvent(input$gmm_data_file, {
    print("DEBUG: gmm_data_file observer triggered.")
    req(input$gmm_data_file)
    if (!analysis_running()) {
      tryCatch({
        print("DEBUG: Attempting to read GMM data.")
        data <- read_excel(input$gmm_data_file$datapath)

        gmm_data_reactive(data) # Store raw data initially

        col_names <- colnames(data)

        updateSelectInput(session, "gmm_col_value",
                          choices = c("None" = "", col_names), selected = "")
        updateSelectInput(session, "gmm_col_age",
                          choices = c("None" = "", col_names), selected = "")
        message_rv(list(text = "Data loaded for Subpopulation Detection! Please select columns.", type = "success"))
        print("DEBUG: GMM data loaded and selectors updated.")
      }, error = function(e) {
        message_rv(list(text = paste("Error loading data for GMM:", e$message), type = "danger"))
        print(paste("DEBUG ERROR: Error loading GMM data:", e$message))
      })
    } else {
      message_rv(list(text = "Cannot load data while Main Analysis is running. Please wait or reset.", type = "warning"))
      print("DEBUG: GMM data load blocked by main analysis.")
    }
  })

  # Observer for "Run Subpopulation Detection" button
  observeEvent(input$run_gmm_analysis, {
    print("DEBUG: run_gmm_analysis observer triggered.")
    if (input$tabs == "Subpopulation Detection (GMM)" && !analysis_running()) {
      req(gmm_data_reactive(), input$gmm_col_value, input$gmm_col_age)
      if (is.null(input$gmm_col_value) || input$gmm_col_value == "" || input$gmm_col_value == "None" ||
          is.null(input$gmm_col_age) || input$gmm_col_age == "" || input$gmm_col_age == "None") {
        message_rv(list(text = "Please select both 'HGB Values' and 'Age' columns for subpopulation detection.", type = "warning"))
        print("DEBUG: GMM analysis: Missing column selection.")
        return()
      }

      analysis_running(TRUE)
      message_rv(list(text = "Detecting subpopulations...", type = "info"))
      print("DEBUG: Subpopulation detection started.")

      tryCatch({
        full_original_data <- gmm_data_reactive() # Get the original loaded data

        # --- Generate age_group_label just before analysis, now that gmm_col_age is selected ---
        age_col_values_for_labeling <- as.numeric(full_original_data[[input$gmm_col_age]])
        breaks_for_cut <- c(-Inf, 10, 30, 40, 100, Inf)
        labels_for_cut <- c("0-10 years", "10-30 years", "30-40 years", "40-100 years", "100+ years")
        full_original_data$age_group_label_for_gmm <- cut(age_col_values_for_labeling,
                                                           breaks = breaks_for_cut,
                                                           labels = labels_for_cut,
                                                           right = FALSE,
                                                           include.lowest = TRUE)
        print("DEBUG: 'age_group_label_for_gmm' column created just before analysis.")
        # --- END NEW: Generate age_group_label here ---


        # Extract HGB and Age columns directly using the input names
        hgb_values_raw <- full_original_data[[input$gmm_col_value]]
        age_values_raw <- full_original_data[[input$gmm_col_age]]

        # Combine into a data frame, then omit NAs. This ensures 'data_for_mclust'
        # and its corresponding 'age_group_label' are aligned by row.
        temp_data_with_labels_for_mclust <- data.frame(HGB = as.numeric(hgb_values_raw),
                                                        Age = as.numeric(age_values_raw),
                                                        original_row_index = 1:nrow(full_original_data),
                                                        age_group_label = full_original_data$age_group_label_for_gmm) # Carry over labels


        # Omit rows where either HGB or Age values are NA (after coercion)
        combined_data_for_mclust_cleaned <- na.omit(temp_data_with_labels_for_mclust)

        # Separate the data used for Mclust from the labels for later use
        data_for_mclust <- combined_data_for_mclust_cleaned %>%
          select(HGB, Age)

        # Get the corresponding age_group_labels for the data used in Mclust
        age_group_labels_for_clustered_data <- combined_data_for_mclust_cleaned$age_group_label


        print(paste("DEBUG: Combined data for GMM prepared. Dimensions:", paste(dim(data_for_mclust), collapse = "x")))

        if (nrow(data_for_mclust) < 2) {
            stop("Not enough valid data points (HGB and Age) for subpopulation detection after removing missing values.")
        }
        if (!all(sapply(data_for_mclust, is.numeric))) {
            stop("Combined HGB and Age data must be entirely numeric.")
        }

        # --- Pass combined data and G range for Mclust to choose optimal G ---
        print(paste("DEBUG: Calling run_gmm with G range 2:5 for automatic selection."))
        gmm_model_result <- run_gmm(data_for_mclust, G_range = 2:5) # No input$gmm_n_components here
        print("DEBUG: run_gmm call completed. Optimal G selected by Mclust is:")
        print(gmm_model_result$G)

        print("DEBUG: Calling assign_clusters.")
        clustered_df_result <- assign_clusters(data_for_mclust, gmm_model_result)
        # Add the age_group_label back to the clustered_df_result
        clustered_df_result$age_group_label <- age_group_labels_for_clustered_data
        print("DEBUG: assign_clusters call completed and age_group_label added.")

        # --- DYNAMIC RENDERING OF RESULTS ---
        output$gmm_results_ui <- renderUI({
          num_clusters <- gmm_model_result$G
          tagList(
            h4(paste0("Subpopulation Plot (Detected ", num_clusters, " Clusters)")),
            plotOutput("gmm_plot", height = "400px"),
            h4(paste0("Subpopulation Characteristics (", num_clusters, " Clusters)")),
            verbatimTextOutput("gmm_summary")
          )
        })

        output$gmm_plot <- renderPlot({
          plot_age_hgb(df = clustered_df_result, age = "Age", hgb = "HGB")
        })

        output$gmm_summary <- renderPrint({
          print(summary(gmm_model_result))

          cat("\n--- Detailed Subpopulation Characteristics ---\n")
          num_clusters <- gmm_model_result$G
          
          # Use dplyr for a nice summary table of cluster counts by age group
          if ("age_group_label" %in% colnames(clustered_df_result)) {
            summary_by_age_group <- clustered_df_result %>%
              group_by(cluster, age_group_label) %>%
              summarise(count = n(), .groups = 'drop') %>%
              pivot_wider(names_from = age_group_label, values_from = count, values_fill = 0)
            
            cat("\nCluster Counts by Age Group:\n")
            print(summary_by_age_group)
            cat("\n")
          } else {
            cat("\n(Age group labels not available in summary due to missing 'age_group_label' column in clustered data.)\n")
          }


          for (i in 1:num_clusters) {
            cat(paste0("Cluster ", i, ":\n"))
            cat(paste0("  Proportion (Size): ", round(gmm_model_result$parameters$pro[i], 3), "\n"))
            cat(paste0("  Mean HGB: ", round(gmm_model_result$parameters$mean["HGB", i], 3), "\n"))
            cat(paste0("  Mean Age: ", round(gmm_model_result$parameters$mean["Age", i], 3), "\n"))
            
            # --- FIX: Accessing variances from the 'sigma' 3D array for multivariate models ---
            # gmm_model_result$parameters$variance$sigma is a 3D array: [row_var, col_var, cluster_index]
            cluster_covariance_matrix <- gmm_model_result$parameters$variance$sigma[,,i]
            
            hgb_variance_val <- NA
            age_variance_val <- NA

            if (!is.null(cluster_covariance_matrix) && is.matrix(cluster_covariance_matrix) && all(c("HGB", "Age") %in% colnames(cluster_covariance_matrix))) {
                hgb_variance_val <- cluster_covariance_matrix["HGB", "HGB"]
                age_variance_val <- cluster_covariance_matrix["Age", "Age"]
            } else {
                warning(paste0("Variance matrix for Cluster ", i, " is problematic or missing HGB/Age columns. (Model: ", gmm_model_result$modelName, ")"))
            }

            sd_hgb <- NA # Default to NA
            if (is.numeric(hgb_variance_val) && !is.na(hgb_variance_val) && hgb_variance_val >= 0) {
              sd_hgb <- sqrt(hgb_variance_val)
            } else {
              warning(paste0("HGB variance for Cluster ", i, " is problematic or negative: ", hgb_variance_val))
            }

            sd_age <- NA # Default to NA
            if (is.numeric(age_variance_val) && !is.na(age_variance_val) && age_variance_val >= 0) {
              sd_age <- sqrt(age_variance_val)
            } else {
              warning(paste0("Age variance for Cluster ", i, " is problematic or negative: ", age_variance_val))
            }

            cat(paste0("  Std Dev HGB: ", ifelse(is.na(sd_hgb), "N/A", round(sd_hgb, 3)), "\n")) # Print N/A if sd_hgb is NA
            cat(paste0("  Std Dev Age: ", ifelse(is.na(sd_age), "N/A", round(sd_age, 3)), "\n")) # Print N/A if sd_age is NA
            
            # --- Estimated Age Range for the cluster ---
            if (!is.na(sd_age)) {
              lower_age <- round(gmm_model_result$parameters$mean["Age", i] - 2 * sd_age, 1)
              upper_age <- round(gmm_model_result$parameters$mean["Age", i] + 2 * sd_age, 1)
              cat(paste0("  Estimated Age Range (Mean +/- 2SD): [", max(0, lower_age), " to ", max(0, upper_age), "] years\n")) # Cap lower bound at 0
            } else {
              cat("  Estimated Age Range: N/A (Std Dev Age problematic)\n")
            }
            cat("\n")
          }
        })

        message_rv(list(text = paste0("Subpopulation detection complete! Detected ", gmm_model_result$G, " clusters."), type = "success"))
        print("DEBUG: GMM analysis complete message sent.")

      }, error = function(e) {
        message_rv(list(text = paste("Error during subpopulation detection:", e$message), type = "danger"))
        print(paste("DEBUG ERROR: Error during GMM analysis:", e$message))
        output$gmm_results_ui <- renderUI({ tagList() }) # Clear previous dynamic UI results on error
      }, finally = {
        analysis_running(FALSE)
        print("DEBUG: GMM analysis finally block executed.")
      })
    } else if (input$tabs != "Subpopulation Detection (GMM)") {
      message_rv(list(text = "Please switch to the 'Subpopulation Detection (GMM)' tab to run this analysis.", type = "warning"))
      print("DEBUG: GMM analysis blocked: Not on correct Tab.")
    }
  })

  # Observer for "Reset GMM Data" button
  observeEvent(input$reset_gmm_btn, {
    print("DEBUG: reset_gmm_btn observer triggered.")
    if (!analysis_running()) {
      gmm_data_reactive(NULL)
      updateSelectInput(session, "gmm_col_value", choices = c("None" = ""), selected = "")
      updateSelectInput(session, "gmm_col_age", choices = c("None" = ""), selected = "")
      output$gmm_results_ui <- renderUI({ tagList() })
      shinyjs::reset("gmm_data_file")
      message_rv(list(text = "GMM data and results reset.", type = "info"))
      print("DEBUG: GMM data and results reset.")
    } else {
      message_rv(list(text = "Cannot reset GMM data while Main Analysis is running. Please wait or reset Main Analysis first.", type = "warning"))
      print("DEBUG: GMM reset blocked by main analysis.")
    }
  })