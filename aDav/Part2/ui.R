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
      tabsetPanel(id = "main_tabs",
                  tabPanel("Preface", uiOutput("preface")),
                  tabPanel("Feature Plot", plotOutput("featurePlot")),
                  tabPanel("Correlation Heatmap", plotOutput("correlationPlot")),
                  tabPanel("Model Coefficients", plotOutput("modelPlot")),
                  tabPanel("Model Interpretation", uiOutput("modelInterpretation"))
      )
    )
  )
))
