library(shiny)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(ggcorrplot)
library(patchwork)
library(rsample) #for easy data splits
library(recipes) #for preventing data leakage and centering/scaling variables

# Load clean dataset
data <- readRDS("Data/spotify_cleaned.rds")

shinyServer(function(input, output) {
  
  # Very long string for a preface in the shiny app.  
  # Human written notes made more structured and coherent with the help of AI
  output$preface_content <- renderUI({
  preface_text <- "
  
  # Preface: Spotify Popularity Analyzer
  
Welcome to the Spotify Popularity Analyzer. This interactive application is 
designed to explore the relationship between the audio features of a song and 
its popularity on Spotify. It serves not only as a tool for data exploration 
but also as a demonstration of best practices in predictive modeling and data 
analysis.


## 1. The Goal

The primary objective is to build and evaluate statistical models that can 
predict a song's popularity score (a value from 0-100) based on quantitative 
features like Danceability, Energy, Loudness, and its Genre. The application 
allows for a side-by-side comparison of different regression techniques to 
understand their strengths and weaknesses.


## 2. Key Features and Methodological Steps

This application was built with a strong focus on methodological rigor to ensure
that our findings are both reliable and generalizable. The following steps and 
features are central to its design:


### A. Data Preparation and Splitting:

#### Initial Cleaning: 

The dataset was loaded and cleaned, with column names 
standardized and data types corrected.

#### Train-Test Split: 

To get an honest estimate of model performance on unseen data,
the dataset is first split into a training set (75%) and a test set (25%). 
The model is built only using the training data, and its final performance is 
evaluated only on the test data. This practice is crucial for preventing data 
leakage, where information from the test set inadvertently influences the model 
training process.

#### Stratified Sampling: 

The split is stratified by Popularity. This ensures that 
both the training and test sets have a similar distribution of popularity scores
, making the evaluation more reliable.


### B. Feature Engineering with recipes:

All data pre-processing is handled through a reproducible pipeline using the 
recipes package. This ensures that transformations are learned from the training
data and consistently applied to the test data.


#### Handling Categorical Features: 

When Genre is included as a predictor, we group 
the least common genres into a single \"Other\" category based on their 
frequency in the training set. This prevents issues with rare categories. 
We then convert these categories into numeric dummy variables for the models.

#### Feature Scaling: 

Regularized regression models (Lasso and Ridge) require 
predictors to be on a similar scale. For these models, we standardize the 
numeric features (scaling them to have a mean of 0 and a standard deviation of 
1). This is done by learning the mean and standard deviation from the training 
data only and applying that same transformation to the test data. Our standard 
\"Linear (Unscaled)\" model intentionally omits this step to preserve the direct
interpretability of its coefficients.


### C. Interactive Model Comparison:

The application allows you to train and evaluate four different regression 
models:


#### Linear (Unscaled): 

A standard OLS regression model. Its coefficients are easy to
interpret in their original units 
(e.g., \"a one-unit increase in Loudness...\").

#### Linear (Scaled): 

An OLS model trained on standardized data. This version is 
useful for comparing the relative importance of different features.

#### Lasso Regression: 

A regularized model that performs feature selection by 
shrinking the coefficients of unimportant features to exactly zero. It's 
excellent for creating simpler, more parsimonious models.

#### Ridge Regression: 

A regularized model that reduces model complexity by shrinking
coefficients towards zero, but it keeps all features. It is particularly 
effective at handling multicollinearity (when predictors are correlated with 
each other).


## 3. Model Evaluation and Interpretation

A single metric is not enough to judge a model. We provide a comprehensive suite
of out-of-sample performance metrics, all calculated on the held-out test set:


#### Test RMSE (Root Mean Squared Error): 

Measures the typical prediction error in 
the original units of popularity.

#### Test MAE (Mean Absolute Error): 

Another measure of average prediction error, 
less sensitive to large outliers than RMSE.

#### Test R-squared: 

Shows the percentage of variance in song popularity that the 
model can explain on unseen data. This is a crucial measure of a model's 
predictive power.

#### Baseline RMSE: 

The error a naive model would have if it simply guessed the 
average popularity for every song. A useful model should have an RMSE lower than
this baseline.

## 4. Interpreting the Results: 

#### Why are the models so similar?

As you explore the models, you may notice that their performance metrics (RMSE, 
R-squared) are often very similar. This is not an error in the code but rather 
an important finding about the data itself.


#### Low Signal-to-Noise Ratio: 

Our models explain roughly 20-30% of the variance in 
popularity. This suggests that while audio features are predictive, a large 
portion of what makes a song popular is determined by other factors not present 
in this dataset (e.g., artist fame, marketing, cultural trends). With a 
relatively weak signal, all the models tend to converge on a similar solution.


#### Why Lasso Doesn't Remove Many Features: 

Lasso regression only removes features 
that are either truly irrelevant or redundant. The fact that Lasso often keeps 
most of the features, even with a penalty, suggests that each audio feature 
provides at least a small amount of unique, non-redundant information. 
The cross-validation process determines that keeping these weak predictors is 
better for overall predictive accuracy than discarding them.


This application provides a robust framework for exploring the data and 
    demonstrates that a rigorous process is as important as the final results. 
    Enjoy your analysis" 
  markdown(preface_text)
  })
  
  # Numerical variables
  num_vars <- c("Popularity", "Year", "BPM", "Energy", "Danceability", 
                "Loudness", "Liveness", "Valence", 
                "Duration", "Acousticness", "Speechiness")
  
  # Data split on selected range of years using rsample package
  data_split <- reactive({
    
    req(input$year_range)
    df <- data %>% filter(
      Year >= input$year_range[1], 
      Year <= input$year_range[2])
    
    set.seed(input$seed) 
    
    split <- initial_split(df, prop = 0.75, strata = Popularity)
    
    list(
      train = training(split),
      test  = testing(split),
      full = df
    )
  })

  # top genres calculated based on the training set
  top_genres <- reactive({
    
    data_split()$train %>%
      count(Genre, sort = TRUE) %>%
      slice_head(n = input$genre_count) %>%
      pull(Genre) %>%
      as.character()
  })
  
  # top genres calculated based on the full set for cor matrix
  top_genres_full <- reactive({
    
    data_split()$full %>%
      count(Genre, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(Genre) %>%
      as.character()
  })
  
  # display the amount of tracks included based on the range of years selected
  output$n_tracks <- renderText({
    
    paste("No. Tracks: ",
          "Train = ", nrow(data_split()$train), 
          "Test = ", nrow(data_split()$test), 
          "Full = ", nrow(data_split()$full))
  })

  # dynamic dropdown menu, select a genre to filter the cor matrix. Top 10 only.
  output$genre_selector_ui <- renderUI({
    
    dropdown <- c("All Genres", top_genres_full())
    selectInput("genre_filter", 
                label = "Filter Heatmap by Genre:",
                choices = dropdown,
                selected = "All Genres")
  })
  
  # EDA plots based on full dataset
  # Plotting features against popularity
  output$featurePlot <- renderPlot({
    df_point <- data_split()$full
    
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
  
  # Correlation matrix, filter dropdown menu dynamically adjusts to year range
  output$correlationPlot <- renderPlot({
    
    if (input$genre_filter != "All Genres") {
      df_cor <- data_split()$full %>% filter(Genre == input$genre_filter)
    }
    
    else {
      df_cor <- data_split()$full
    }
    
    correlation_matrix <- cor(df_cor[, num_vars])
    
    plot_title <- paste("Correlation Heatmap for", input$genre_filter)
    
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
  
  # lasso, ridge, lm (scaled and unscaled) training
  model_result <- reactive({
    df_train <- data_split()$train
    df_test <- data_split()$test
    vars_keep <- num_vars 
    
    if (input$include_genre) {
      vars_keep <- c(vars_keep, "Genre")
      top_g <- df_train %>%
        count(Genre, sort = TRUE) %>%
        slice_head(n = input$genre_count) %>%
        pull(Genre) %>%
        as.character()
    }
    
    df_train_subset <- df_train %>% dplyr::select(all_of(vars_keep))
    df_test_subset  <- df_test  %>% dplyr::select(all_of(vars_keep))
    
    req(nrow(df_train_subset) > 10)
    
    # Building recipe for scaling/centering on based on training data only
    rec <- recipe(Popularity ~ ., data = df_train_subset)
    
    if (input$include_genre) {
      rec <- rec %>%
        step_mutate(Genre = forcats::fct_other(
          Genre, keep = top_g, other_level = "Other")) %>%
        step_dummy(all_nominal_predictors())
    }
    
    if (input$model_type != "lm") {
      rec <- rec %>% 
        step_normalize(all_numeric_predictors())
    }
    
    # "Baking" the train and test sets
    trained_rec <- prep(rec, training = df_train_subset)
    df_train_processed <- bake(trained_rec, new_data = NULL) 
    df_test_processed  <- bake(trained_rec, new_data = df_test_subset)
    
    if (input$model_type %in% c("lm", "lm_scaled")) {
      model <- lm(Popularity ~ ., data = df_train_processed)
    } 
    
    else {
      x_train <- model.matrix(Popularity ~ ., df_train_processed)[, -1]
      y_train <- df_train_processed$Popularity
      alpha_val <- ifelse(input$model_type == "lasso", 1, 0)
      req(ncol(x_train) > 0)
      model <- cv.glmnet(x_train, y_train, alpha = alpha_val)
    }
    
    list(
      model = model,
      test = df_test_processed
      )
  })

  model_performance <- reactive({
    results <- model_result()
    model <- results$model
    test_data <- results$test # correctly 'baked' test data
    
    if (inherits(model, "cv.glmnet")) { # Lasso and Ridge
      # need the formula from the recipe to predict correctly
      x_test <- model.matrix(Popularity ~ ., test_data)[, -1]
      predictions <- predict(model, newx = x_test, s = "lambda.min")
      
    } 
    
    else { # lm
      predictions <- predict(model, newdata = test_data)
    }

    test_rmse <- sqrt(mean((test_data$Popularity - predictions)^2))
    test_mae <- mean(abs(test_data$Popularity - predictions))
    
    # out-of-sample r-squared
    ss_total <- sum((test_data$Popularity - mean(test_data$Popularity))^2)
    ss_residual <- sum((test_data$Popularity - predictions)^2)
    test_r_squared <- 1 - (ss_residual / ss_total)
    
    # Baseline RMSE for comparison (!)
    # The baseline model always predicts average popularity from TRAINING set.
    mean_popularity_train <- mean(data_split()$train$Popularity)
    baseline_rmse <- sqrt(mean((test_data$Popularity - mean_popularity_train)^2))
    
    # Performance Metrics list
    list(
      test_rmse = test_rmse,
      test_mae = test_mae,
      test_r_squared = test_r_squared,
      baseline_rmse = baseline_rmse
    )
  })

  
  # histograms of coeficcients, separating genres from numerical variables
  output$modelPlot <- renderPlot({
      
    model <- model_result()$model
    
    # --- 1. Get Coefficients (same as before) ---
    if (input$model_type %in% c("lm", "lm_scaled")) { # lm (scaled/unscaled)
      coefs <- summary(model)$coefficients[-1, 1, drop = FALSE]
      df_model <- data.frame(Feature = rownames(coefs), Coefficient = coefs[,1])
    } 
    
    else { # Lasso, Ridge
      coefs <- as.matrix(coef(model, s = "lambda.min"))[-1, 1, drop = FALSE]
      df_model <- data.frame(Feature = rownames(coefs), Coefficient = coefs[,1])
      df_model <- df_model[df_model$Coefficient != 0, ]
    }
    
    # Edge case, no features selected
    if(nrow(df_model) == 0) {
      return(ggplot() + labs(title = "Model selected no features", 
                             x = "", y = "") + theme_minimal())
    }
    
    # split coefficients into numeric and genre
    genre_features <- df_model[grepl("Genre", df_model$Feature), ]
    numeric_features <- df_model[!grepl("Genre", df_model$Feature), ]
    
    # clean up genre feature names
    if(nrow(genre_features) > 0) {
      genre_features$Feature <- gsub("Genre_", "", genre_features$Feature)
    }
    
    # Numeric coef histogram
    plot_numeric <- ggplot(numeric_features, 
                           aes(x = reorder(Feature, Coefficient), 
                               y = Coefficient, fill = Coefficient > 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Numeric Feature Coefficients", 
           x = "", y = "Coefficient") +
      scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), 
                        guide = "none") +
      theme_minimal(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Genre coef histogram
    if(nrow(genre_features) > 0) {
      plot_genre <- ggplot(genre_features, 
                           aes(x = reorder(Feature, Coefficient), 
                               y = Coefficient, fill = Coefficient > 0)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Genre Coefficients", 
             subtitle = "(Relative to base level)", x = "", y = "Coefficient") +
        scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), 
                          guide = "none") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.subtitle = element_text(hjust = 0.5))
      
      # pathwork combo (thanks chatGPT this works great)
      plot_numeric + plot_genre
    } 
    
    else {plot_numeric}
    })
  
  # Reactive model interpretation text (made with some help from AI for HTML)
  output$modelInterpretation <- renderUI({
    results <- model_result()
    model <- results$model
    performance <- model_performance()

    performance_summary <- tagList(
      h4("Model Performance on Unseen Data (Test Set)"),
      tags$ul(
        tags$li(HTML(paste0(
          "<b>Test RMSE:</b> ", 
          strong(round(performance$test_rmse, 2)), 
          ". On average, the model's predictions are off 
          by this many points."))),
        tags$li(HTML(paste0(
          "<b>Test MAE:</b> ", 
          strong(round(performance$test_mae, 2)), 
          ". This is the Mean Absolute Error."))),
        tags$li(HTML(paste0(
          "<b>Test R-squared:</b> ", 
          strong(round(performance$test_r_squared * 100, 1)), 
          "%. The model explains this much of the variance in 
          popularity for unseen songs."))),
        tags$li(HTML(paste0(
          "<b>Baseline RMSE:</b> ", 
          strong(round(performance$baseline_rmse, 2)), 
          ". A naive model would have this error."))),
        ),
      hr()
      )
    
    # lm specific
    if (input$model_type %in% c("lm", "lm_scaled")) {
      s <- summary(model)
      adj_r2 <- s$adj.r.squared
      model_title <- ifelse(input$model_type == "lm", 
                            "Linear (Unscaled) Regression Model Summary", 
                            "Linear (Scaled) Regression Model Summary")
      tagList(
        h3(model_title),
        performance_summary,
        if (input$model_type == "lm_scaled") {
          p(HTML("<b>Note on Scaled Coefficients:</b> This model was trained on 
                 scaled (standardized) predictors. A coefficient's value 
                 represents the change in Popularity for a 
                 <b>one-standard-deviation</b> change in that predictor. 
                 This makes it easier to compare the relative importance of 
                 features."))
          } 
        
        else {
          p(HTML("<b>Note on Unscaled Coefficients:</b> Coefficients represent 
                 the change in Popularity for a <b>one-unit change</b> in that 
                 predictor (e.g., one more BPM, one more dB of loudness)."))
          },
        
        h4("In-Sample Fit (Training Data)"),
        p(HTML(paste0(
          "On the data it was trained on, the model explains ", 
          strong(round(adj_r2 * 100, 1)), 
          "% of the variance (Adjusted R-squared). "))),
        hr(),
        h4("Full Model Output (from Training Data)"),
        verbatimTextOutput("lm_full_summary"))
      } 
    
    else {
      # Lasso/Ridge specific
      best_lambda <- model$lambda.min
      active_coefs <- coef(model, s = "lambda.min")
      selected_features <- active_coefs@Dimnames[[1]][which(active_coefs != 0)]
      selected_features <- selected_features[selected_features != "(Intercept)"]
      
      if(input$model_type == "lasso") { # Lasso
        model_specific_text <- tagList(
          h4("Feature Selection with Lasso"),
          p(HTML("Lasso regression performs <b>feature selection</b> by 
                 shrinking the coefficients of less important features to 
                 exactly zero. This helps create a simpler, 
                 more interpretable model.")),
          p(paste("With the optimal lambda, Lasso selected", 
                  length(selected_features),
                  "features as being predictive of popularity.")),
          if (length(selected_features) > 0) {
            tags$ul(lapply(selected_features, tags$li))
            } 
          
          else {
            p(strong("The model did not select any features."))
          })
        } 
      
      else { # Ridge
        ridge_coefs <- coef(model, s = "lambda.min") %>%
          as.matrix() %>%
          as.data.frame() %>%
          rownames_to_column("Feature") %>%
          rename(Coefficient = s1) %>%
          filter(Feature != "(Intercept)") %>%
          mutate(Influence = abs(Coefficient)) %>%
          arrange(desc(Influence)) %>%
          slice_head(n = 10) # Get top 10 most influential
        
        model_specific_text <- tagList(
          h4("Coefficient Shrinkage with Ridge"),
          p(HTML("Ridge regression moderates feature influence by shrinking 
                 coefficients towards zero. Unlike Lasso, it keeps all 
                 features. Below are the <b>top 10 most influential features</b>
                 after regularization (based on the size of their final 
                 coefficients):")),
          tags$ol(
            # lapply for numbered list
            lapply(paste0(
              ridge_coefs$Feature, " (Coefficient: ", 
              round(ridge_coefs$Coefficient, 3), ")"), tags$li))
          )
        }
      
      tagList(
        h3(paste(tools::toTitleCase(input$model_type), 
                 "Regression Model Summary")),
        performance_summary,
        model_specific_text,
        tags$p(HTML(paste0("(Regularization was optimized with a lambda of ",
                           strong(round(best_lambda, 4)), ".)"))))
      }
    })
  output$lm_full_summary <- renderPrint({
    req(input$model_type %in% c("lm", "lm_scaled"))
    summary(model_result()$model)
    })
  })