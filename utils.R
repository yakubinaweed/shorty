# R/utils.R

# Load necessary libraries (ensure they are installed: install.packages("car"))
# The car package contains the powerTransform function which implements Yeo-Johnson
library(car) # For yeo_johnson_transform
library(moments) # For skewness calculation
library(ggplot2) # For plotting
library(mclust) # For GMM analysis
library(dplyr) # For data manipulation

# Function to filter data based on gender and age
filter_data <- function(data, gender_choice, age_min, age_max, col_gender, col_age) {
  if (!col_gender %in% names(data) || !col_age %in% names(data)) {
    stop("Gender or age column not found in data.")
  }

  filtered_data <- data %>%
    filter(!!sym(col_age) >= age_min & !!sym(col_age) <= age_max)
  
  # Add a temporary Gender column with standardized values based on regex
  filtered_data <- filtered_data %>%
    mutate(Gender_Standardized = case_when(
      # Extensive regex for male terms and symbols (English and Dutch)
      grepl("male|m|man|jongen(s)?|heren|mannelijk(e)?", !!sym(col_gender), ignore.case = TRUE) ~ "Male",
      # Extensive regex for female terms and symbols (English and Dutch), including 'V'
      grepl("female|f|vrouw(en)?|v|meisje(s)?|dame|mevr|vrouwelijke", !!sym(col_gender), ignore.case = TRUE) ~ "Female",
      TRUE ~ "Other"
    ))
  
  if (gender_choice != "Both") {
    # Filter the data based on the standardized gender column and the user's choice
    filtered_data <- filtered_data %>%
      filter(Gender_Standardized == case_when(
        gender_choice == "M" ~ "Male",
        gender_choice == "F" ~ "Female"
      ))
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

# Function to run GMM analysis using mclust with a selectable criterion
run_gmm_with_criterion <- function(data_mat, G_range = 2:5, criterion = "BIC") {
  if (!is.matrix(data_mat) && !is.data.frame(data_mat)) {
    stop("Input data_mat must be a matrix or data frame for GMM analysis.")
  }
  if (!all(sapply(data_mat, is.numeric))) {
    stop("All columns in data_mat must be numeric.")
  }
  if (any(is.na(data_mat))) {
    stop("Input data_mat contains NA values. Please remove or impute before clustering.")
  }
  
  # Using the most common bivariate models
  multivariate_model_names <- c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVV")
  
  tryCatch({
    if (criterion == "BIC") {
      gmm_model <- Mclust(data_mat, G = G_range, modelNames = multivariate_model_names)
      return(gmm_model)
    } else if (criterion == "ICL") {
      mclust_all <- Mclust(data_mat, G = G_range, modelNames = multivariate_model_names, warn = FALSE)
      if (is.null(mclust_all)) {
        stop("Mclust could not be fitted to the data.")
      }
      icl_values <- mclustICL(mclust_all)
      
      best_model_index <- which.max(icl_values)
      
      best_G <- icl_values[best_model_index, "G"]
      best_model_name <- rownames(icl_values)[best_model_index]

      gmm_model <- Mclust(data_mat, G = best_G, modelNames = best_model_name)
      return(gmm_model)

    } else {
      stop("Invalid criterion selected. Please choose 'BIC' or 'ICL'.")
    }
  }, error = function(e) {
    # Return NULL and a warning if the GMM fitting fails for any reason
    warning(paste("GMM run failed:", e$message))
    return(NULL)
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
  
  # Calculate cluster means
  cluster_means <- df %>%
    group_by(Gender, cluster) %>%
    summarise(mean_Age = mean(Age, na.rm = TRUE),
              mean_HGB = mean(HGB, na.rm = TRUE),
              .groups = 'drop')

  ggplot(df, aes(x = Age, y = HGB, color = factor(cluster))) +
    geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.6) +
    # Use stat_ellipse to plot 95% confidence regions for each cluster
    stat_ellipse(geom = "polygon", aes(fill = factor(cluster)), alpha = 0.2, show.legend = FALSE, level = 0.95) +
    # Add a large red point for the mean of each cluster
    geom_point(data = cluster_means, aes(x = mean_Age, y = mean_HGB), shape = 4, size = 5, color = "red", stroke = 2) +
    facet_wrap(~Gender) +
    theme_minimal() +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         x = "Age", y = "HGB", color = "Cluster") +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme(legend.position = "bottom")
}

# Function to apply universal plausibility limits (example, currently not used in server)
apply_universal_plausibility_limits <- function(data_df) {
  filtered_data <- data_df %>%
    filter(HGB >= 5 & HGB <= 20) %>% # Example HGB range
    filter(Age >= 0) # Example non-negative age

  if (nrow(filtered_data) < nrow(data_df)) {
    warning(paste(nrow(data_df) - nrow(filtered_data), "rows removed due to plausibility limits."))
  }
  return(filtered_data)
}