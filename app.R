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
    
    #flavors input
    uiOutput("FlavorsRadiobox"),
    
    #submit button that looks like martini glass
    uiOutput("SubmitButton")
    
  ),
  #main panel for displaying outputs
  mainPanel(
    #imageOutput("Images")
    
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
    
    # present submit button
    output$SubmitButton <- renderUI({
      actionButton(inputId = "SubmitButton", label = "Find Drinks", icon = icon(name = "martini-glass-citrus"))
    })
  })
  
  
  drink_filter <- reactive({
    
    #Adds column of image path to remaining drinks
    #data$image <- paste0("www/", data$Name, ".jpg")
    
    # Filter cocktails based on user preferences
    filtered <- data
    if (!is.null(input$Alcohol)) {
      filtered <- filtered[filtered$Alcohol %in% input$Alcohol,]
    }
    
    if (!is.null(input$SweetnessSlider)) {
      filtered <- filtered[filtered$Sweetness >= (input$SweetnessSlider - 1) & filtered$Sweetness <= (input$SweetnessSlider + 1),]
    }
    
    if (!is.null(input$FlavorsRadiobox)) {
      filtered <- filtered[str_detect(filtered$Flavors, input$FlavorsRadiobox),]
    }
    
    #Adds column of image path to remaining drinks
    #filtered$image <- paste0("www/", filtered$Name, ".jpg")
    
    #filtered <- data[data$image != "", ]
    filtered
    
  })
  
  # output table when submit button clicked
  observeEvent(input$SubmitButton, {
    output$Table <- DT::renderDataTable({
      drink_filter()
    })
  })

  
  # Output images based on filtered data
  #output$Images <- renderImage({
    # Get filtered drinks data
   # filtered_data <- drink_filter()
    #images_list <- list()
    #for (i in 1:nrow(filtered_data)) {
      # Add image to images list for each row
     # image_path <- filtered_data[i, "image"]
    #  image <- list(src = image_path, width = 400, height = 400)
    #  images_list[[i]] <- image
    #}
    # Return images list
    #images_list
  #}, deleteFile = FALSE)
  

  

  
}

# Run the app
shinyApp(ui = ui, server = server)

## Steps to do in code
# 1. Fix table output for flavors radio box (complete)
# 2. Verify output is filtered accurately (complete)
# 3. Set up table to only show when submit button is clicked (complete)
# 4. Transition table output to image output
