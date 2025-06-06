library(shiny)

shinyUI(fluidPage(
  titlePanel("Spotify Popularity Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("feature", "Select Feature to Visualize:",
                  choices = c("Danceability", "Energy", "Loudness", "Valence", "Acousticness", "Speechiness"),
                  selected = "Danceability"),
      
      sliderInput("year_range", "Release Year Range:",
                  min = 1920, max = 2020, value = c(2000, 2020), step = 1),
      
      radioButtons("model_type", "Choose Regression Model:",
                   choices = c("Linear" = "lm", "Lasso" = "lasso"),
                   selected = "lm")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Feature Plot", plotOutput("featurePlot")),
        tabPanel("Model Output", plotOutput("modelPlot")),
        tabPanel("Summary", verbatimTextOutput("modelSummary"))
      ),
      hr(),
      h4("Interpretation of Results"),
      textOutput("interpretation")
    )
  )
))