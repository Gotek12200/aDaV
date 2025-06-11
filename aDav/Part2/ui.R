library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("flatly"), # Optional: adds a nice theme
  titlePanel("Spotify Popularity Analyzer"),
  
  p("This application explores how different audio features influence the 
  popularity of songs on Spotify. It allows users to visualize 
    relationships, compare regression models, and 
    interpret statistical results interactively."),
  
  sidebarLayout(
    sidebarPanel(
      h4("Global Data Filter"),
      sliderInput("year_range", "Release Year Range:",
                  # Min should be 1955
                  min = 1955, max = 2020, value = c(2000, 2020), step = 1),
      # Adding a lil div for no of tracks.
      h5(textOutput("n_tracks")),

      # CONDITIONAL SIDE PANELS (cleann :) )
      ## Selecting inputs for the feature plot
      conditionalPanel(
        condition = "input.main_tabs == 'Feature Plot'",
        selectInput("feature", "Select Feature to Visualize:",
                    choices = c("Danceability", "Energy", "Loudness", 
                                "Valence", "Acousticness", "Speechiness"),
                    selected = "Danceability")
      ),
      
      ## Selecting inputs for the correlation heatmap
      conditionalPanel(
        condition = "input.main_tabs == 'Correlation Heatmap'",
        # use uiOutput, choices are generated serverside
        uiOutput("genre_selector_ui") 
      ),
      
      # MODEL TABS
      ## Using || this panel will appear for both the model coeff histogram tab, 
      ## and then model interpretation summary tab
      conditionalPanel(
        condition = "input.main_tabs == 'Model Coefficients' || 
        input.main_tabs == 'Model Interpretation'",
        radioButtons("model_type", "Choose Regression Model:",
                     choices = c("Linear" = "lm", "Lasso" = "lasso"),
                     selected = "lm")
      )
    ),
    
    mainPanel(
      # We add an id here so the conditionalPanels can see which tab is active.
      tabsetPanel(
        id = "main_tabs", 
        tabPanel("Preface", textOutput("preface")),
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
