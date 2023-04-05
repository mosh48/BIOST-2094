#load shiny and readxl packages
library(shiny)
library(readxl)

#load data
data <- read_excel("ProjectData.xlsx")

#define UI
ui <- fluidPage(
#app title
titlePanel("Drink Finder: A Personalized Cocktail Recommender"),

#input definitions
sidebarLayout(
  
  sidebarPanel(
    #alcohol type input
    checkboxGroupInput(inputId="alcohol",
                       label="Choose your alcohol preference(s)",
                       choices=c("Vodka", "Gin", "Rum", "Tequila", "Whiskey", "No Preference"))
  ),
  #main panel for displaying outputs
  mainPanel(
    
  )
)
)

#define server logic
server <- function(input, output) {
  
}

#Run the app
shinyApp(ui, server)
