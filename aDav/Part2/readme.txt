Contributions

Jair (achternaam):
- Added a bunch of comments for clarification
- Reordered the code sections, the plot generation comes last
- Fixed some code styling, stick to 80 columns/character
- Removed na.omit() because there is no missing data in the dataset
- added a display of the amount of tracks included via the year range selector
- changed the interactivity of the correlation matrix, selecting different features doesn't change anything. Now you can filter based on the top 10 most numerous genres
- added a preface placeholder. Maybe it's easier to use a separate .Rmd for the support document though
- included all numerical variables rather than just Popularity, Danceability, Energy, Loudness, Valence, Acousticness and Speechiness. (We can reverse this if you guys want)
- made some changes to the dataframe object names (df = df_model, df_cor etc.)
- added a check for the regression coefficient histogram to display a message if no features are selected
- Correlation matrix/heatmap now only displays lower triangle
- Added reactive model summary output for LM and Lasso results. I had to use some AI to get it to work the way I wanted to, I'll report this alongside my contributions section
- separate helper function for the full lm model summary output

Calvin Boateng: 
- Implemented visualization of regression coefficients using ggplot2 bar charts to interpret model outputs
- Created the correlation heatmap using ggcorrplot, enabling intuitive understanding of feature relationships
- Introduced interactivity in visual components, allowing users to:
- Select features for scatter plots
- Filter the correlation matrix by genre
- Toggle between linear and Lasso regression models
- Designed layout logic so visual tabs adapt based on user input (e.g., conditional panels)
- Applied consistent styling and theme_minimal() to ensure clean and readable graphics
- Structured reactive plotting functions to ensure plots update dynamically with filtered data
- Enhanced accessibility of visuals by adding axis labels, titles, captions, and legends

Osaro Orebor:
- Structured the app into a modular Shiny framework using server.R and ui.R components
- Set up reactive expressions for dynamic data filtering based on user-selected year range
- Integrated interactive UI elements using sliderInput, selectInput, and conditionalPanel
- Designed the tabbed layout to organize visualizations, model outputs, and interpretations clearly
- Implemented server-side logic for toggling between linear regression and Lasso regression
- Connected user inputs to dynamic plot and model generation in real time
- Ensured responsiveness and reactivity of the dashboard for fluid user experience
- Validated data inputs and selections to prevent errors during runtime (e.g., missing data, empty results)
- Collaborated on model result rendering and formatted user-facing statistical summaries