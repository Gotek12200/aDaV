library(shiny)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(ggcorrplot)

# Load data
data <- read.csv("Data/Spotify-2000.csv") %>%
  rename(Duration = Length..Duration., BPM = Beats.Per.Minute..BPM., Loudness = Loudness..dB.,
         Genre = Top.Genre, ID = Index) %>%
  mutate(Genre = as.factor(Genre), Duration = as.integer(gsub(",", "", Duration)))

shinyServer(function(input, output) {
  
  filteredData <- reactive({
    req(input$year_range)
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  # Interactive scatter plot
  output$featurePlot <- renderPlot({
    df <- filteredData()
    correlation <- cor(df$Popularity, df[[input$feature]])
    ggplot(df, aes_string(x = "Popularity", y = input$feature)) +
      geom_jitter(alpha = 0.4) +
      geom_smooth(method = "loess", color = "blue", se = TRUE) +
      labs(title = paste("Popularity vs", input$feature),
           x = "Popularity", y = input$feature,
           caption = paste("Feature Correlation:", round(correlation, 2))) +
      theme_minimal() +
      theme(text = element_text(size = 16), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0))
  })
  
  # Run the chosen regression model
  modelResult <- reactive({
    df <- filteredData() %>%
      select(Popularity, Danceability, Energy, Loudness, Valence, Acousticness, Speechiness) %>%
      na.omit()
    
    if (input$model_type == "lm") {
      model <- lm(Popularity ~ ., data = df)
    } else {
      x <- model.matrix(Popularity ~ ., df)[,-1]
      y <- df$Popularity
      model <- cv.glmnet(x, y, alpha = 1)  # Lasso regression
    }
    model
  })
  
  # Plot showcasing model coefficients
  output$modelPlot <- renderPlot({
    model <- modelResult()
    if (input$model_type == "lm") {
      coefs <- summary(model)$coefficients[-1, 1]
      df <- data.frame(Feature = names(coefs), Coefficient = coefs)
    } else {
      coefs <- as.matrix(coef(model, s = "lambda.min"))[-1, 1]
      df <- data.frame(Feature = names(coefs), Coefficient = coefs)
      df <- df[df$Coefficient != 0, ]
    }
    ggplot(df, aes(x = reorder(Feature, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste(input$model_type, "Regression Coefficients"),
           x = "Feature", y = "Coefficient",) +
      scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
      theme_minimal() +
      theme(text = element_text(size = 16), 
            plot.title = element_text(hjust = 0.5))
  })
  
  # Interactive correlation heatmap
  output$correlationPlot <- renderPlot({
    df <- filteredData() %>%
      select(Popularity, input$correlation_features)
    correlation_matrix <- cor(df)
    ggcorrplot::ggcorrplot(correlation_matrix, lab = TRUE, colors = c("#7F7FFF", "#EFEFEF", "#FF4C4C")) +
      labs(title = "Correlation Heatmap of Selected Features", x = "", y = "",) +
      theme_minimal() +
      theme(text = element_text(size = 16), 
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$modelSummary <- renderPrint({
    if (input$model_type == "lm") {
      summary(modelResult())
    } else {
      cat("Lasso Regression - Cross-Validated MSE:", min(modelResult()$cvm))
    }
  })
  
  # Reactive text for regression model results
  output$interpretation <- renderText({
    if (input$model_type == "lm") {
      r2 <- summary(modelResult())$r.squared
      paste("The linear model explains about", round(r2 * 100, 2), "% of the variance in popularity.")
    } else {
      paste("The lasso regression model achieved a minimum MSE of", round(min(modelResult()$cvm), 2))
    }
  })
})