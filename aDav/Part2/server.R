library(shiny)
library(tidyverse)
library(glmnet)
library(ggcorrplot)

# Load preprocessed data
data <- readRDS("Data/spotify_cleaned.rds")

shinyServer(function(input, output) {
  filteredData <- reactive({
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  output$n_tracks <- renderText({ paste(nrow(filteredData()), "tracks included") })
  
  topGenres <- reactive({
    genres <- filteredData() %>% count(Genre, sort = TRUE) %>% filter(n > 10) %>% head(10) %>% pull(Genre)
    c("All Genres", as.character(genres))
  })
  
  output$genre_selector_ui <- renderUI({
    selectInput("genre_filter", "Filter Heatmap by Genre:", choices = topGenres(), selected = "All Genres")
  })
  
  output$preface <- renderUI({
    HTML(paste(
      "<h4>Research Context</h4>",
      "<p>This dashboard explores the question: <strong>What audio characteristics make a song popular on Spotify?</strong></p>",
      "<p>Using data from over 2,000 tracks released between 1955 and 2020, we investigate the relationship between audio features (like Danceability, Energy, and Acousticness) and a song's popularity score.</p>",
      "<h4>Method</h4>",
      "<p>We apply both <strong>linear regression</strong> and <strong>Lasso regression</strong> to identify important features. Lasso further penalizes weak predictors to encourage sparsity.</p>",
      "<p>Explore the plots, correlation heatmaps, and model outputs to understand what drives musical success on Spotify.</p>",
      sep = ""
    ))
  })
  
  modelResult <- reactive({
    df_full <- filteredData()
    
    if (input$model_type == "lm") {
      df <- df_full %>% select(Popularity, Danceability, Energy, Loudness, Valence, Acousticness, Speechiness, BPM, Duration)
      model <- lm(Popularity ~ ., data = df)
    } else {
      df_x <- df_full %>% select(Danceability, Energy, Loudness, Valence, Acousticness, Speechiness, BPM, Duration)
      x <- model.matrix(~ ., data = df_x)[, -1]
      y <- df_full$Popularity
      set.seed(input$seed)
      model <- cv.glmnet(x, y, alpha = 1)
    }
    return(model)
  })
  
  output$featurePlot <- renderPlot({
    df_point <- filteredData()
    correlation <- cor(df_point$Popularity, df_point[[input$feature]])
    ggplot(df_point, aes_string(x = "Popularity", y = input$feature)) +
      geom_jitter(alpha = 0.3) +
      geom_smooth(method = "loess", color = "blue") +
      labs(title = paste("Popularity vs", input$feature),
           caption = paste("Correlation:", round(correlation, 2))) +
      theme_minimal()
  })
  
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
    
    if (nrow(df_model) == 0) {
      ggplot() + labs(title = "No features selected by Lasso.") + theme_minimal()
    } else {
      ggplot(df_model, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste(input$model_type, "Regression Coefficients")) +
        theme_minimal() +
        scale_fill_manual(values = c("red", "blue"), guide = FALSE)
    }
  })
  
  output$correlationPlot <- renderPlot({
    df_cor <- if (input$genre_filter == "All Genres") filteredData() else filteredData() %>% filter(Genre == input$genre_filter)
    correlation_matrix <- cor(df_cor %>% select_if(is.numeric))
    ggcorrplot(correlation_matrix, lab = TRUE, type = "lower", colors = c("blue", "white", "red"))
  })
  
  output$modelInterpretation <- renderUI({
    model <- modelResult()
    if (input$model_type == "lm") {
      s <- summary(model)
      r2 <- s$adj.r.squared * 100
      coef_table <- as.data.frame(s$coefficients)[-1, ]
      interpretations <- apply(coef_table, 1, function(row) {
        feature_name <- rownames(row)
        est <- row["Estimate"]
        p_val <- row["Pr(>|t|)"]
        direction <- ifelse(est > 0, "positive", "negative")
        signif <- ifelse(p_val < 0.05, "significant", "not significant")
        paste0("The relationship between ", strong(feature_name), " and popularity is ", direction, " and ", signif, " (p = ", round(as.numeric(p_val), 4), ").")
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
      lambda_choice <- "lambda.1se"
      coefs <- coef(model, s = lambda_choice)
      selected <- rownames(coefs)[which(abs(coefs) > 1e-4)]
      selected <- selected[selected != "(Intercept)"]
      
      rmse <- round(sqrt(min(model$cvm)), 2)
      mse <- round(min(model$cvm), 2)
      lambda <- round(model$lambda.1se, 4)
      
      tagList(
        h3("Lasso Regression: Model Interpretation"),
        p("Lasso regression helps us identify only the most important features by penalizing those that contribute little to prediction accuracy."),
        br(),
        h4("Model Performance"),
        tags$ul(
          tags$li(HTML(paste("<strong>Root Mean Squared Error (RMSE):</strong> ", rmse))),
          tags$li(HTML(paste("<strong>Mean Squared Error (MSE):</strong> ", mse))),
          tags$li(HTML(paste("<strong>Optimal Lambda:</strong> ", lambda)))
        ),
        br(),
        h4("Selected Features"),
        if (length(selected) > 0) {
          tagList(
            p("The following features were selected by the model as having the strongest relationship with popularity:"),
            tags$ul(lapply(selected, function(feat) tags$li(HTML(paste("<strong>", feat, "</strong>")))))
          )
        } else {
          p("No features were selected. This may indicate that within the selected year range, none of the audio features provide a strong or consistent signal for predicting popularity.")
        },
        br(),
        p(em("Tip: Adjust the year range or seed value to explore how feature importance changes."))
      )
    }
  })
  
  output$lm_full_summary <- renderPrint({
    if (input$model_type == "lm") summary(modelResult())
  })
})