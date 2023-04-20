#load appropriate packages
library(shiny)
library(readxl)
library(tidyverse)
library(jpeg)
library(DT)

#load data
data <- read_excel("ProjectData.xlsx")
Flavors <- unique(unlist(strsplit(data$Flavors, ", ")))

#define UI
ui <- fluidPage(
#app title
titlePanel("Drink Finder: A Personalized Cocktail Recommender"),

#input definitions
sidebarLayout(
  
  sidebarPanel(
    #alcohol type input
    checkboxGroupInput(inputId="Alcohol",
                       label="Choose your alcohol preference(s). If you have no preference, check all boxes:",
                       choices=c("Vodka", "Gin", "Rum", "Tequila", "Whiskey")),
                       #could add unique(data$Alcohol)
    
    #sweetness button
    actionButton(inputId = "SweetnessButton", label = "Select a sweetness Level"),
    
    #sweetness slider
    uiOutput("SweetnessSlider"),
    
    #flavors button
    uiOutput("FlavorsButton"),
    #actionButton(inputId = "FlavorsButton", label = "Select a flavor"),
    
    #flavors input
    uiOutput("FlavorsRadiobox"),
   
    #sweetness slider
    #sliderInput(inputId = "Sweetness",
    #            label = "Choose your preferred level of sweetness:",
    #            min = 1,
    #            max = 5,
    #            value = 3,), #step = 1
    #uiOutput("Sweetness"),
    
    #flavors input
    #checkboxGroupInput(inputId = "Flavors",
    #                   label = "Choose any flavors you would prefer:",
    #                   choices = unique(unlist(strsplit(data$Flavors, ", ")))),
                      #had to remove "," in column for each flavor
    #uiOutput("Flavors"),
    
    #submit button that looks like martini glass
    uiOutput("SubmitButton")
    #submitButton(text = "Find Drinks", icon = icon(name = "martini-glass-citrus"))
    
  ),
  #main panel for displaying outputs
  mainPanel(
    #imageOutput("Images")
    #tableOutput("Table")
    
    DT::dataTableOutput("Table")
  )
)
)

# Define server function
server <- function(input, output) {
  
  # Present sweetness slider when the SweetnessButton is clicked
  observeEvent(input$SweetnessButton, {
    # Filter data based on selected alcohol
    filtered_data <- data
    if (!is.null(input$Alcohol)) {
      filtered_data <- filtered_data %>% filter(Alcohol %in% input$Alcohol)
    }
    
    # Update sweetness slider
    output$SweetnessSlider <- renderUI({
      sliderInput(inputId = "SweetnessSlider",
                  label = "Choose your preferred level of sweetness:",
                  min = min(filtered_data$Sweetness),
                  max = max(filtered_data$Sweetness),
                  value = round(mean(range(filtered_data$Sweetness))),
                  step = 1)
    })
    
    # present flavors button
    output$FlavorsButton <- renderUI({
      actionButton(inputId = "FlavorsButton", label = "Select a flavor")
    })
  })
  
  # Update available flavors when flavors button is chosen
  observeEvent(input$FlavorsButton, {
    
    # Filter available flavors based on selected alcohol and sweetness
    filtered_data <- data
    if (!is.null(input$Alcohol)) {
      filtered_data <- filtered_data %>% filter(Alcohol %in% input$Alcohol)
    }
    
    # Sweetness + or - 1
    if (!is.null(input$SweetnessSlider)) {
      filtered_data <- filtered_data[filtered_data$Sweetness >= (input$SweetnessSlider - 1) & filtered_data$Sweetness <= (input$SweetnessSlider + 1),]
    }
    
    # Get list of available flavors based on filtered data
    flavors <- unique(unlist(strsplit(filtered_data$Flavors, ", ")))
    
    output$FlavorsRadiobox <- renderUI({
      radioButtons("FlavorsRadiobox", "Choose any flavors you would prefer:", choices = flavors)
    }) # maybe do drop down menu
    
    output$SubmitButton <- renderUI({
      submitButton(text = "Find Drinks", icon = icon(name = "martini-glass-citrus"))
    })
  })
  
  
  drink_filter <- reactive({
    #eventReactive(input$SubmitButton, {
  
    # Initialize reactive values to store input values
    #vals <- reactiveValues(Alcohol = NULL, Sweetness = NULL, Flavors = NULL)
    
    # Filter cocktails based on user preferences
    filtered <- data
    if (!is.null(input$Alcohol)) {
      filtered <- filtered[filtered$Alcohol %in% input$Alcohol,]
    }
    
    if (!is.null(input$SweetnessSlider)) {
      filtered <- filtered[filtered$Sweetness >= (input$SweetnessSlider - 1) & filtered$Sweetness <= (input$SweetnessSlider + 1),]
    }
    
    if (!is.null(input$FlavorsRadiobox)) {
      flavors <- unlist(strsplit(as.character(filtered$Flavors), ","))
      filtered <- filtered[apply(sapply(input$FlavorsRadiobox, function(x) grepl(x, Flavors)), 1, any),]
    }
    #Adds column of image path to remaining drinks
    filtered$image <- paste0("www/", filtered$Name, ".jpg")
    
    filtered
    #return(filtered)
    
    # Problem: Does not return drinks after flavor picked
    
  })
  
  # Update input values when submit button is clicked
  #observeEvent(input$SubmitButton, {
    #vals$Alcohol <- input$Alcohol
    #vals$Sweetness <- input$Sweetness
    #vals$Flavors <- input$Flavors
  #})
  
  output$Table <- DT::renderDataTable({
    drink_filter()
  })
  
  #output$Table <- DT::renderDataTable({
   # if (!is.null(input$SubmitButton)) {
    #  datatable(drink_filter())
    #}
  #})
}

# Run the app
shinyApp(ui = ui, server = server)

## Steps to do in code
# 1. Fix table output for flavors radio box
# 2. Verify output is filtered accurately
# 3. Set up table to only show when submit button is clicked
# 4. Transition table output to image output
