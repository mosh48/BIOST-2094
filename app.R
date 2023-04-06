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
                       label="Choose your alcohol preference(s). If you have no preference, check all boxes:",
                       choices=c("Vodka", "Gin", "Rum", "Tequila", "Whiskey")),
    
    #sweetness slider
    sliderInput(inputId = "Sweetness",
                label = "Choose your preferred level of sweetness:",
                min = 1,
                max = 5,
                value = 3),
    
    #alcohol content slider
    sliderInput(inputId = "ABV",
                label = "Choose your preferred alcohol content:",
                min = min(data$ABV),
                max = max(data$ABV),
                value = 20), # might want to do intervals of 5 or something
    
    #flavors input
    checkboxGroupInput(inputId = "Flavors",
                       label = "Choose any flavors you would prefer:",
                       choices = unique(unlist(strsplit(data$Flavors, ", ")))),
                      #had to remove "," in column for each flavor
                      #list might be a bit long
      #maybe we could eliminate choices if there is not a drink that fits flavors
    
    #submit button that looks like martini glass
    submitButton(text = "Find Drinks", icon = icon(name = "martini-glass-citrus"))
    
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
