# R/utils.R

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
