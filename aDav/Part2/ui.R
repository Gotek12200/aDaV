library(shiny)
library(shinythemes)

# UI definition for the Spotify Popularity Analyzer app
shinyUI(fluidPage(
  theme = shinytheme("flatly"),  # Use a clean Bootstrap theme
  titlePanel("Spotify Popularity Analyzer"),
  
  # Introductory paragraph below the title
  p("This interactive application explores how audio features relate to song popularity on Spotify. 
     You can filter songs by release year, compare regression models, and visualize patterns. 
     The goal is to uncover what makes a song popular using data science techniques."),
  
  # Layout with sidebar and main panel
  sidebarLayout(
    
    # Sidebar to Only show when not on Preface tab
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
        
        # ----- Conditional Panel: Feature Plot tab -----
        conditionalPanel(
          condition = "input.main_tabs == 'Feature Plot'",
          selectInput("feature", "Select Feature to Visualize:",
                      choices = c("Danceability", "Energy", "Loudness", 
                                  "Valence", "Acousticness", "Speechiness"))
        ),
        
        # ----- Conditional Panel: Correlation Heatmap tab -----
        conditionalPanel(
          condition = "input.main_tabs == 'Correlation Heatmap'",
          uiOutput("genre_selector_ui")  # dynamically generated genre selector
        ),
        
        # ----- Conditional Panel: Model tabs -----
        conditionalPanel(
          condition = "input.main_tabs == 'Model Coefficients' || input.main_tabs == 'Model Interpretation'",
          
          # Model selection radio buttons
          radioButtons("model_type", "Choose Regression Model:",
                       choices = c("Linear" = "lm", "Lasso" = "lasso")),
          
          # Show seed input only when Lasso is selected
          conditionalPanel(
            condition = "input.model_type == 'lasso'",
            numericInput("seed", "Set Random Seed:", value = 123, min = 1, max = 9999),
            helpText("Change the seed to ensure reproducible results for Lasso regression.")
          )
        )
      )
    ),
    
    # Main Panel with Tabs
    mainPanel(
      # Add id so we can refer to the active tab in conditionalPanels
      tabsetPanel(
        id = "main_tabs",
        
        # Tab 1: Preface (intro text, no sidebar)
        tabPanel("Preface", uiOutput("preface")),
        
        # Tab 2: Scatterplot of feature vs popularity
        tabPanel("Feature Plot", plotOutput("featurePlot")),
        
        # Tab 3: Heatmap of correlations
        tabPanel("Correlation Heatmap", plotOutput("correlationPlot")),
        
        # Tab 4: Coefficient bar chart
        tabPanel("Model Coefficients", plotOutput("modelPlot")),
        
        # Tab 5: Model summary and interpretation
        tabPanel("Model Interpretation", 
                 icon = icon("magnifying-glass-chart"),
                 uiOutput("modelInterpretation"))
      )
    )
  )
))
