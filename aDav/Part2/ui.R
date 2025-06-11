library(shiny)
library(shinythemes)

# UI definition for the Spotify Popularity Analyzer app
shinyUI(fluidPage(
  theme = shinytheme("flatly"), # Use a clean Bootstrap theme
  titlePanel("Spotify Popularity Analyzer"),
  
  # Introductory paragraph below the title
  p("This application explores how different audio features influence the 
  popularity of songs on Spotify. It allows users to visualize 
    relationships, compare regression models, and 
    interpret statistical results interactively."),
  
  # Layout with sidebar and main panel
  sidebarLayout(
    conditionalPanel(
      condition = "input.main_tabs != 'Preface'",  # hide on "Preface" tab
    sidebarPanel(
      h4("Global Data Filter"),
      
      # Year range slider
      sliderInput("year_range", 
                  "Release Year Range:", 
                  min = 1955, max = 2020, value = c(2000, 2020)),
      # Text output showing number of included tracks
      h5(textOutput("n_tracks")),
      
      hr(),
      
      # Feature Plot tab
      conditionalPanel(
        condition = "input.main_tabs == 'Feature Plot'",
        h4("Plot Controls"),
        selectInput("feature", "Select Feature to Visualize:",
                    choices = c("Danceability", "Energy", "Loudness", 
                                "Valence", "Acousticness", "Speechiness"),
                    selected = "Danceability")
      ),
      
      # Correlation Matrix/Heatmap tab
      conditionalPanel(
        condition = "input.main_tabs == 'Correlation Heatmap'",
        h4("Heatmap Controls"),
        uiOutput("genre_selector_ui")
      ),
      
      # Model tabs
      conditionalPanel(
        condition = "input.main_tabs == 'Model Coefficients' || 
        input.main_tabs == 'Model Interpretation'",
        h4("Model Controls"),
        numericInput("seed", "Set Random Seed:", 
                     value = 123, min = 1, max = 9999),
        helpText("Change the seed to ensure reproducible 
                 results."),
        
        # Model selection radio buttons
        radioButtons("model_type", "Choose Regression Model:",
                     choices = c("Linear (Unscaled)" = "lm",
                                 "Linear (Scaled)"   = "lm_scaled",
                                 "Lasso"             = "lasso", 
                                 "Ridge"             = "ridge"),
                     selected = "lm"),
        checkboxInput("include_genre", "Include Genre as a Predictor", 
                      value = TRUE),
        conditionalPanel(
          condition = "input.include_genre == true",
          numericInput("genre_count", "Number of Top Genres:", 
                       value = 5, min = 1, max = 20, step = 1)
          )
        )
      )
    ),
    
    # Main Panel with Tabs
    mainPanel(
      tabsetPanel(
        # Add id for referring to the active tab in conditionalPanels
        id = "main_tabs", 
        tabPanel("Preface", uiOutput("preface_content")),
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

