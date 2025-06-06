library(shiny)
library(tidyverse)
library(ggplot2)
library(glmnet)

data <- read.csv("Data/Spotify-2000.csv") %>%
  rename(Duration = Length..Duration., BPM = Beats.Per.Minute..BPM., Loudness = Loudness..dB.,
         Genre = Top.Genre, ID = Index) %>%
  mutate(Genre = as.factor(Genre), Duration = as.integer(gsub(",", "", Duration)))

shinyServer(function(input, output) {
  
  filteredData <- reactive({
    req(input$year_range)
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  output$featurePlot <- renderPlot({
    ggplot(filteredData(), aes_string(x = "Popularity", y = input$feature)) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "loess") +
      labs(title = paste("Popularity vs", input$feature), x = "Popularity", y = input$feature)
  })
  
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
  
  output$modelPlot <- renderPlot({
    if (input$model_type == "lm") {
      plot(modelResult())
    } else {
      plot(modelResult())
    }
  })
  
  output$modelSummary <- renderPrint({
    if (input$model_type == "lm") {
      summary(modelResult())
    } else {
      cat("Lasso Regression - Cross-Validated MSE:", min(modelResult()$cvm))
    }
  })
  
  output$interpretation <- renderText({
    if (input$model_type == "lm") {
      r2 <- summary(modelResult())$r.squared
      paste("The linear model explains about", round(r2 * 100, 2), "% of the variance in popularity.")
    } else {
      paste("The lasso regression model achieved a minimum MSE of", round(min(modelResult()$cvm), 2))
    }
  })
})