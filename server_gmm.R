gmmServer <- function(input, output, session,
                      gmm_uploaded_data_rv,
                      gmm_processed_data_rv,
                      gmm_transformation_details_rv,
                      gmm_models_rv,
                      message_rv,
                      analysis_running_rv) {

  # --- Helper Functions ---
  apply_conditional_yeo_johnson <- function(x) {
    skew <- moments::skewness(x)
    if (abs(skew) > 1) {
      transformed_data <- car::yjPower(x, lambda = "auto")
      list(transformed_data = transformed_data, transformation_applied = TRUE)
    } else {
      list(transformed_data = x, transformation_applied = FALSE)
    }
  }

  z_transform <- function(x) {
    (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  }

  run_gmm_with_criterion <- function(data, criterion = "BIC") {
    if (criterion == "BIC") {
      Mclust(data)
    } else {
      Mclust(data, G = 1:9, modelNames = NULL, initialization = list(hcPairs = hc(data)))
    }
  }

  # --- Main Observer for GMM ---
  observeEvent(input$run_gmm, {
    req(input$file_input)
    analysis_running_rv(TRUE)
    message_rv(NULL)

    # Read and validate the uploaded file
    data <- readxl::read_excel(input$file_input$datapath)
    req(all(c("HGB", "Age", "Gender") %in% names(data)))

    # Normalize gender labels
    gmm_data <- data %>%
      dplyr::select(HGB, Age, Gender_orig = Gender) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(Gender = dplyr::case_when(
        grepl("male|m", Gender_orig, ignore.case = TRUE) ~ "Male",
        grepl("female|f", Gender_orig, ignore.case = TRUE) ~ "Female",
        TRUE ~ "Other"
      ))

    gmm_uploaded_data_rv(gmm_data)
    model_criterion <- input$model_selection_criterion

    withProgress(message = "Running GMM clustering...", value = 0, {
      subsets <- list(
        male = gmm_data %>% dplyr::filter(Gender == "Male"),
        female = gmm_data %>% dplyr::filter(Gender == "Female"),
        combined = gmm_data
      )

      results <- lapply(names(subsets), function(gender) {
        dat <- subsets[[gender]]
        yj <- apply_conditional_yeo_johnson(dat$HGB)
        dat$HGB_transformed <- yj$transformed_data
        dat$HGB_z <- z_transform(dat$HGB_transformed)
        dat$Age_z <- z_transform(dat$Age)

        model <- tryCatch({
          run_gmm_with_criterion(dat %>% dplyr::select(HGB = HGB_z, Age = Age_z), criterion = model_criterion)
        }, error = function(e) {
          message_rv(list(text = paste("Error in", gender, "model:", e$message), type = "error"))
          NULL
        })

        if (!is.null(model)) {
          dat$cluster <- as.factor(model$classification)
        } else {
          dat$cluster <- NA
        }

        list(data = dat, model = model, transformed = yj$transformation_applied)
      })

      names(results) <- names(subsets)

      # Store results in reactives
      gmm_processed_data_rv(bind_rows(
        male = results$male$data,
        female = results$female$data,
        combined = results$combined$data,
        .id = "Subset"
      ))

      gmm_models_rv(list(
        male = results$male$model,
        female = results$female$model,
        combined = results$combined$model
      ))

      gmm_transformation_details_rv(list(
        male_hgb_transformed = results$male$transformed,
        female_hgb_transformed = results$female$transformed,
        combined_hgb_transformed = results$combined$transformed
      ))
    })

    analysis_running_rv(FALSE)
  })

  # --- Output Plots and Tables ---

  output$gmm_model_selection_plot <- renderPlot({
    req(gmm_models_rv())
    criterion_selected <- input$model_selection_criterion
    models <- gmm_models_rv()

    par(mfrow = c(1, 3))
    for (g in c("male", "female", "combined")) {
      if (!is.null(models[[g]])) {
        title <- paste(toupper(g), "-", criterion_selected)
        if (criterion_selected == "BIC") {
          plot(models[[g]], what = "BIC", main = title)
        } else {
          plot(mclustICL(models[[g]]), main = title)
        }
      }
    }
  })

  output$gmm_cluster_plot <- renderPlot({
    req(gmm_processed_data_rv())
    data <- gmm_processed_data_rv()

    ggplot2::ggplot(data, aes(x = Age, y = HGB, color = cluster)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::stat_ellipse(type = "norm", level = 0.95, aes(group = cluster)) +
      ggplot2::facet_wrap(~Subset) +
      ggplot2::labs(
        title = "GMM Clusters",
        subtitle = "By Subset (Male, Female, Combined)"
      ) +
      ggplot2::theme_minimal()
  })

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
}
