# R/server_main.R

mainServer <- function(input, output, session, data_reactive, selected_dir_reactive, message_rv, analysis_running_rv) {

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
}
