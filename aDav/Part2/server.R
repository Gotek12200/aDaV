library(shiny)
library(tidyverse)
library(glmnet)
library(ggcorrplot)

# Load cleaned Spotify dataset
data <- readRDS("Data/spotify_cleaned.rds")

shinyServer(function(input, output) {
  

  
  # Subset the dataset based on the selected year range
  filteredData <- reactive({
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  # Display the number of tracks in the selected range
  output$n_tracks <- renderText({ 
    paste(nrow(filteredData()), "tracks included") 
  })
  
  # Identify the top 10 most common genres within the selected year range
  topGenres <- reactive({
    genres <- filteredData() %>% 
      count(Genre, sort = TRUE) %>% 
      filter(n > 10) %>% 
      head(10) %>% 
      pull(Genre)
    c("All Genres", as.character(genres))
  })
  
  # Generate UI dropdown for genre filter in heatmap
  output$genre_selector_ui <- renderUI({
    selectInput("genre_filter", "Filter Heatmap by Genre:", 
                choices = topGenres(), selected = "All Genres")
  })
  

  
  output$preface <- renderUI({
    tagList(
      h3("Spotify Popularity Analyzer"),
      p("This application explores how audio features influence the popularity of a total of 1994 songs listed on Spotify."),
      p("Our research question: ", strong("How do audio features predict song popularity across different genres and years?")),
      h4("How to Use This App:"),
      tags$ol(
        tags$li("Filter Data: Use the year slider to select songs from a specific range."),
        tags$li("Visualizations:"),
        tags$ul(
          tags$li(HTML("<b>Feature Plot:</b> Explore how features correlate with popularity.")),
          tags$li(HTML("<b>Correlation Heatmap:</b> View feature inter-relationships within genres.")),
          tags$li(HTML("<b>Model Coefficients:</b> Compare Linear and Lasso regression outputs."))
        ),
        tags$li("Model Interpretation: Understand model performance and key features.")
      )
    )
  })
  

  
  modelResult <- reactive({
    df_full <- filteredData()
    
    if (input$model_type == "lm") {
      # Linear Regression: include Popularity + numerical predictors
      df <- df_full %>% 
        select(Popularity, Danceability, Energy, Loudness, Valence, 
               Acousticness, Speechiness, BPM, Duration)
      model <- lm(Popularity ~ ., data = df)
      
    } else {
      # Lasso Regression: only predictors, no target variable
      df_x <- df_full %>% 
        select(Danceability, Energy, Loudness, Valence, 
               Acousticness, Speechiness, BPM, Duration)
      x <- model.matrix(~ ., data = df_x)[, -1]
      y <- df_full$Popularity
      
      # Set random seed from user input for reproducibility
      set.seed(input$seed)
      model <- cv.glmnet(x, y, alpha = 1)  # Lasso
    }
    
    return(model)
  })
  
  
  
  # Scatter plot: Popularity vs selected feature
  output$featurePlot <- renderPlot({
    df_point <- filteredData()
    correlation <- cor(df_point$Popularity, df_point[[input$feature]])
    
    ggplot(df_point, aes_string(x = "Popularity", y = input$feature)) +
      geom_jitter(alpha = 0.3) +
      geom_smooth(method = "loess", color = "blue") +
      labs(
        title = paste("Popularity vs", input$feature),
        caption = paste("Correlation:", round(correlation, 2))
      ) +
      theme_minimal()
  })
  
  # Bar plot: Regression coefficients from Linear or Lasso
  output$modelPlot <- renderPlot({
    model <- modelResult()
    
    if (input$model_type == "lm") {
      coefs <- summary(model)$coefficients[-1, 1]
      df_model <- data.frame(Feature = names(coefs), Coefficient = coefs)
      
    } else {
      lambda_choice <- "lambda.1se"
      coefs_matrix <- coef(model, s = lambda_choice)
      coefs <- as.numeric(coefs_matrix[-1])
      features <- rownames(coefs_matrix)[-1]
      df_model <- data.frame(Feature = features, Coefficient = coefs)
      df_model <- df_model[abs(df_model$Coefficient) > 1e-4, ]
    }
    
    # If Lasso selects no features, show a placeholder message
    if (nrow(df_model) == 0) {
      return(ggplot() + 
               labs(title = "No features selected by Lasso.") + 
               theme_minimal())
    }
    
    ggplot(df_model, aes(x = reorder(Feature, Coefficient), 
                         y = Coefficient, fill = Coefficient > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste(input$model_type, "Regression Coefficients")) +
      scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
      theme_minimal()
  })
  
  # Correlation heatmap: filterable by genre
  output$correlationPlot <- renderPlot({
    df_cor <- if (input$genre_filter == "All Genres") {
      filteredData()
    } else {
      filteredData() %>% filter(Genre == input$genre_filter)
    }
    
    correlation_matrix <- cor(df_cor %>% select_if(is.numeric))
    
    ggcorrplot(
      correlation_matrix, lab = TRUE, type = "lower",
      colors = c("blue", "white", "red")
    )
  })
  
 
  
  output$modelInterpretation <- renderUI({
    model <- modelResult()
    
    if (input$model_type == "lm") {
      # Extract metrics and coefficients
      s <- summary(model)
      r2 <- s$adj.r.squared * 100
      coef_table <- as.data.frame(s$coefficients)[-1, ]
      
      # Convert coefficients into readable interpretations
      interpretations <- apply(coef_table, 1, function(row) {
        feature_name <- rownames(row)
        est <- row["Estimate"]
        p_val <- row["Pr(>|t|)"]
        direction <- ifelse(est > 0, "positive", "negative")
        signif <- ifelse(p_val < 0.05, "significant", "not significant")
        paste0("The relationship between ", strong(feature_name), 
               " and popularity is ", direction, " and ", signif, 
               " (p = ", round(as.numeric(p_val), 4), ").")
      })
      
      tagList(
        h4("Model Summary"),
        p(paste("Adjusted R-squared:", round(r2, 2), "%")),
        h4("Feature Interpretations"),
        tags$ul(lapply(interpretations, function(i) tags$li(HTML(i)))),
        h4("Full Model Output"),
        verbatimTextOutput("lm_full_summary")
      )
      
    } else {
      # Lasso performance metrics
      lambda_choice <- "lambda.1se"
      coefs <- coef(model, s = lambda_choice)
      selected <- rownames(coefs)[which(abs(coefs) > 1e-4)]
      selected <- selected[selected != "(Intercept)"]
      
      rmse <- round(sqrt(min(model$cvm)), 2)
      mse <- round(min(model$cvm), 2)
      lambda <- round(model$lambda.1se, 4)
      
      tagList(
        h3("Lasso Regression: Model Interpretation"),
        p("Lasso regression helps identify key features by penalizing weaker ones."),
        h4("Model Performance"),
        tags$ul(
          tags$li(HTML(paste("<strong>RMSE:</strong>", rmse))),
          tags$li(HTML(paste("<strong>MSE:</strong>", mse))),
          tags$li(HTML(paste("<strong>Optimal Lambda:</strong>", lambda)))
        ),
        h4("Selected Features"),
        if (length(selected) > 0) {
          tagList(
            p("Lasso selected the following features:"),
            tags$ul(lapply(selected, function(feat) tags$li(HTML(paste("<strong>", feat, "</strong>")))))
          )
        } else {
          p("No features were selected. This may indicate a weak or noisy signal in the current data subset.")
        },
        p(em("You can adjust year range or seed to explore feature stability."))
      )
    }
  })
  
  # Print full summary for linear model
  output$lm_full_summary <- renderPrint({
    if (input$model_type == "lm") summary(modelResult())
  })
  
})