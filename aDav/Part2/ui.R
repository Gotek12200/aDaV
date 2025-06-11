library(shiny)

shinyUI(fluidPage(
  titlePanel("Spotify Popularity Analyzer"),
  
  p("This application explores how different audio features influence the popularity of songs on Spotify.
    It allows users to visualize relationships, compare regression models, and interpret statistical results interactively."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Select Feature to Visualize:",
                  choices = c("Danceability", "Energy", "Loudness", "Valence", "Acousticness", "Speechiness"),
                  selected = "Danceability"),
      
      sliderInput("year_range", "Release Year Range:",
                  min = 1920, max = 2020, value = c(2000, 2020), step = 1),
      
      radioButtons("model_type", "Choose Regression Model:",
                   choices = c("Linear" = "lm", "Lasso" = "lasso"),
                   selected = "lm"),
      
      checkboxGroupInput("correlation_features", "Select Features for Correlation Heatmap:",
                         choices = c("Danceability", "Energy", "Loudness", "Valence", "Acousticness", "Speechiness"),
                         selected = c("Danceability", "Energy", "Loudness", "Valence", "Acousticness", "Speechiness"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Feature Plot", plotOutput("featurePlot")),
        tabPanel("Correlation Heatmap", plotOutput("correlationPlot")),
        tabPanel("Model Output", plotOutput("modelPlot")),
        tabPanel("Summary", verbatimTextOutput("modelSummary"))
      ),
      hr(),
      h4("Interpretation of Results"),
      textOutput("interpretation")
    )
  )
))
