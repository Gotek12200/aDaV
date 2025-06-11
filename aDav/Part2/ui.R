library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Spotify Popularity Analyzer"),
  
  p("This interactive application explores how audio features relate to song popularity on Spotify. You can filter songs by release year, compare the effectiveness of linear vs. Lasso regression models, and visualize key patterns through plots and heatmaps. The goal is to uncover what makes a song popular using data science techniques."),
  
  sidebarLayout(
    # Sidebar panel appears ONLY when the tab is not Preface
    conditionalPanel(
      condition = "input.main_tabs != 'Preface'",
      sidebarPanel(
        h4("Global Data Filter"),
        sliderInput("year_range", "Release Year Range:", min = 1955, max = 2020, value = c(2000, 2020)),
        h5(textOutput("n_tracks")),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Feature Plot'",
          selectInput("feature", "Select Feature to Visualize:",
                      choices = c("Danceability", "Energy", "Loudness", 
                                  "Valence", "Acousticness", "Speechiness"))
        ),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Correlation Heatmap'",
          uiOutput("genre_selector_ui")
        ),
        
        conditionalPanel(
          condition = "input.main_tabs == 'Model Coefficients' || input.main_tabs == 'Model Interpretation'",
          radioButtons("model_type", "Choose Regression Model:",
                       choices = c("Linear" = "lm", "Lasso" = "lasso")),
          conditionalPanel(
            condition = "input.model_type == 'lasso'",
            numericInput("seed", "Set Random Seed:", value = 123, min = 1, max = 9999),
            helpText("Change the seed to ensure reproducible results for Lasso regression.")
          )
          
        )
      )
    ),
    
    mainPanel(


      # We add an id here so the conditionalPanels can see which tab is active.
      tabsetPanel(
        id = "main_tabs", 
        tabPanel("Preface", uiOutput("preface")),
        tabPanel("Feature Plot", plotOutput("featurePlot")),
        tabPanel("Correlation Heatmap", plotOutput("correlationPlot")),
        tabPanel("Model Coefficients", plotOutput("modelPlot")),
        tabPanel("Model Interpretation", 
                 icon = icon("magnifying-glass-chart"),
                 uiOutput("modelInterpretation"))
      )
    )
  )
))
library(shiny)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(ggcorrplot)

# Load and clean dataset
data <- read.csv("Data/Spotify-2000.csv", fileEncoding = "UTF-8-BOM") %>% #issue with encoding used LLM to help me fix the issue and it got fixed by adding the fileEncoding = "UTF-8-BOM"
  rename(Duration = Length..Duration., 
         BPM = Beats.Per.Minute..BPM., 
         Loudness = Loudness..dB., 
         Genre = Top.Genre, 
         ID = Index) %>%  # Use Index instead of ï..Index
  mutate(Genre = as.factor(Genre), 
         Duration = as.integer(gsub(",", "", Duration)))


shinyServer(function(input, output) {
  
  # Preparations
  ## Reactive dataset based on year range input
  filteredData <- reactive({
    req(input$year_range)
    data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
  })
  
  ## Display number of tracks in range
  output$n_tracks <- renderText({
    paste(nrow(filteredData()), "tracks included")
  })
  
  ## calculate 10 most common genres within selected year range.
  ### this will be used for a more interactive correlation matrix
  ### "All Genres" included as the first option in the dropdown selection menu
  topGenres <- reactive({
    genre_counts <- filteredData() %>%
      count(Genre, sort = TRUE) %>%
      filter(n > 10) %>%
      slice_head(n = 10) %>%
      pull(Genre)
    c("All Genres", as.character(genre_counts))
  })
  
  ## genre selector UI for the correlation matrix dropdown menu
  output$genre_selector_ui <- renderUI({
    selectInput("genre_filter", 
                label = "Filter Heatmap by Genre:",
                choices = topGenres(),
                selected = "All Genres")
  })
  
  ## Preface
  output$preface <- renderText({paste("placeholder text")})
  
  # Regression and Lasso Model selection and training 
  modelResult <- reactive({
    # prepping dataset
    df_full <- filteredData()
    
    if (input$model_type == "lm") {
      # choosing num colomns + Popularity for lm
      df <- df_full %>%
        select(Popularity, Danceability, Energy, Loudness, Valence,
               Acousticness, Speechiness, BPM, Duration)
      
      # Fit Linear Model
      model <- lm(Popularity ~ ., data = df)
      
    } else {
      # for lasso: only num columns without popularity 
      df_x <- df_full %>%
        select(where(is.numeric)) %>%
        select(-Popularity)
      
      x <- model.matrix(~ ., data = df_x)[, -1]  # verwijder intercept
      y <- df_full$Popularity
      
      # Fit Lasso Model with cross-validation
      model <- cv.glmnet(x, y, alpha = 1)
    }
    
    return(model)
  })
  
  
  # Plots
  ## Feature vs Popularity scatter plot
  output$featurePlot <- renderPlot({
    df_point <- filteredData()
    correlation <- cor(df_point$Popularity, df_point[[input$feature]])
    ggplot(df_point, aes_string(x = "Popularity", y = input$feature)) +
      geom_jitter(width = 0.2, height = 0.2, alpha = 0.3) +
      geom_smooth(method = "loess", color = "blue", se = TRUE) +
      labs(title = paste("Popularity vs", input$feature),
           x = "Popularity", y = input$feature,
           caption = paste("Feature Correlation:", round(correlation, 2))) +
      theme_minimal() +
      theme(text = element_text(size = 16), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0))
  })
  
  ## Regression coefficients visualization for LM/Lasso
  output$modelPlot <- renderPlot({
    model <- modelResult()
    ### LM
    if (input$model_type == "lm") {
      coefs <- summary(model)$coefficients[-1, 1]
      df_model <- data.frame(Feature = names(coefs), Coefficient = coefs)
    } 
    ### Lasso
    else {
      coefs <- as.matrix(coef(model, s = "lambda.min"))[-1, 1]
      df_model <- data.frame(Feature = names(coefs), Coefficient = coefs)
      df_model <- df_model[df_model$Coefficient != 0, ]
    }
    ### CHECK for empty dataframe if no variables selected by Lasso
    if(nrow(df_model) == 0) {
      return(
        ggplot() + 
          labs(title = "Lasso Regression selected no features", 
               x = "", y = "") +
          theme_minimal()
      )
    }
    ### Generate plot
    ggplot(df_model, aes(x = reorder(Feature, Coefficient), 
                   y = Coefficient, 
                   fill = Coefficient > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste(input$model_type, "Regression Coefficients"),
           x = "Feature", y = "Coefficient") +
      scale_fill_manual(values = c("red", "blue"), guide = F) +
      theme_minimal() +
      theme(text = element_text(size = 16), 
            plot.title = element_text(hjust = 0.5))
  })
  
  ## Correlation heatmap
  output$correlationPlot <- renderPlot({
    df_cor <- filteredData()
    ### Based on selection made in UI
    if (input$genre_filter != "All Genres") {
      df_cor <- filteredData() %>% filter(Genre == input$genre_filter)}
    ### Numeric vars only
    correlation_matrix <- cor(df_cor[5:ncol(data)])
    ### dynamic title
    plot_title <- paste("Correlation Heatmap for", input$genre_filter)
    
    ### Generate plot
    ggcorrplot::ggcorrplot(correlation_matrix, 
                           lab = TRUE, 
                           colors = c("#7F7FFF", "#EFEFEF", "#FF4C4C"),
                           type = "lower",
                           lab_size = 4) +
      labs(title = plot_title, x = "", y = "") +
      theme_minimal() +
      theme(text = element_text(size = 16), 
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Reactive model interpretation
  output$modelInterpretation <- renderUI({
    model <- modelResult()
    
    if (input$model_type == "lm") { # for lm
      ## Extract metrics
      s <- summary(model)
      r2 <- s$r.squared
      adj_r2 <- s$adj.r.squared
      explained <- round(adj_r2 * 100, 2)
      
      ## Extract coefficients table
      coef_table <- as.data.frame(s$coefficients)
      coef_table <- coef_table[-1, ] # Remove intercept
      
      ## Generate a sentence for each predictor 
      ## (this section was made with the help of AI assistance - Mila)
      interpretations <- apply(coef_table, 1, function(row) {
        feature_name <- rownames(row)
        estimate <- as.numeric(row["Estimate"])
        p_value <- as.numeric(row["Pr(>|t|)"])
        direction <- ifelse(estimate > 0, "positive", "negative")
        significance <- ifelse(p_value < 0.05, "statistically significant", 
                               "not statistically significant")
        ## (end of section)
        
        paste0(
          "The relationship between ", strong(feature_name), 
          " and Popularity is ", strong(direction), " and ", 
          strong(significance), " (p-value = ", round(p_value, 4), ")."
        )
      })
      
      ## Combine everything
      tagList(
        h3("Linear Regression Model Summary"),
        tags$ul(tags$li(HTML(paste0(
          "The model explains about ", strong(explained), 
          "% of the variance in song popularity (Adjusted R-squared).")))),
        hr(),
        h4("Feature-by-Feature Interpretation"),
        p("This section describes how each audio feature relates to popularity,
          according to the model:"),
        ## (this section was made with the help of AI assistance - Mila)
        tags$ul(
          lapply(interpretations, function(interp) {
            tags$li(HTML(interp))
          })
          ## (end of section)
        ),
        hr(),
        h4("Full Model Output"),
        p("The full statistical summary is provided below:"),
        verbatimTextOutput("lm_full_summary")
      )
      
    } else { ## For Lasso
      ## Extract metrics
      min_mse <- min(model$cvm)
      min_rmse <- sqrt(min_mse)
      best_lambda <- model$lambda.min
      
      ## Get the coefficients for the best lambda
      active_coefs <- coef(model, s = "lambda.min")
      ## (this section was made with the help of AI assistance - Mila)
      selected_features <- active_coefs@Dimnames[[1]][which(active_coefs != 0)]
      selected_features <- selected_features[selected_features != "(Intercept)"]
      ## (end of section)
      
      tagList(
        h3("Lasso Regression Model Summary"),
        p("Lasso is a regression method that performs both variable 
          selection and regularization to enhance prediction 
          accuracy and interpretability."),
        hr(),
        
        h4("Model Performance"),
        tags$ul(tags$li(HTML(paste0(
          "The model's ", 
          strong("Root Mean Squared Error (RMSE)"), 
          " is ", 
          strong(round(min_rmse, 2)), 
                              
          ". This means the model's predictions are typically off by about ", 
          round(min_rmse, 2), " points on the popularity scale (0-100)."))),
          tags$li(HTML(paste0(
            "The underlying ", 
            strong("Mean Squared Error (MSE)"), 
            " was ", strong(round(min_mse, 2)), "."))),
          tags$li(HTML(paste0(
            "This performance was achieved with an optimal lambda 
            (regularization parameter) of ", 
            strong(round(best_lambda, 4)), ".")))
        ),
        
        hr(),
        h4("Feature Selection"),
        tags$ul(tags$li(HTML(paste0(
          "Lasso selected ", 
          strong(length(selected_features)), 
          " out of", length(active_coefs)-1 ,
          " available features as being the most important 
          predictors of popularity:")))),
        
        ## (this section was made with the help of AI assistance - Mila)
        if(length(selected_features) > 0) {
          tags$ul(lapply(selected_features, tags$li))
          ## (end of section)
        } else {
          p(strong("The model did not select any features, suggesting none 
                   have a strong, reliable linear relationship with popularity 
                   within this data slice."))
        }
      )
    }
  })
  
  # lm full model summary output
  output$lm_full_summary <- renderPrint({
    if(input$model_type == "lm") {
      summary(modelResult())
    }
  })
  
})
