#BIOST 2094 Final Project
#Drink Finder
#Li Fan, Emma Lipinski, Nick Moshgat, Nuo Wei

#load packages
library(shiny)
library(readxl)
library(tidyverse)
library(DT)

#set working directory
#setwd("E:/University of Pittsburgh/BIOST 2094 Advanced R Computing/Project/")

#load data and add row numbers
data <- read_excel("ProjectData.xlsx")
data <- data %>% mutate(row_number = row_number())


#load flavors options
Flavors_options <- unlist(strsplit(data$Flavors, ", "))
Flavors_options <- c("No Preference" , Flavors_options)


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
                         choices=unique(data$Alcohol)),
      
      #sweetness button
      actionButton(inputId = "SweetnessButton", label = "Next: Select a Sweetness Level"),
      
      #sweetness slider
      uiOutput("SweetnessSlider"),
      
      #flavors button
      uiOutput("FlavorsButton"),
      
      #flavors input
      uiOutput("FlavorsCheckbox"),
      
      #submit button that looks like martini glass
      uiOutput("SubmitButton")
      
    ),
    #main panel for displaying outputs
    mainPanel(
      
      DT::dataTableOutput("Table"),
      uiOutput("Images")
      
    )
  )
)

#define server logic
server <- function(input, output) {
  
  # Present sweetness slider when the SweetnessButton is clicked
  observeEvent(input$SweetnessButton, {
    
    # Filter data based on selected alcohol
    if (!is.null(input$Alcohol)) {
      data <- data %>% filter(Alcohol %in% input$Alcohol)
    }
    
    # Update sweetness slider
    output$SweetnessSlider <- renderUI({
      sliderInput(inputId = "SweetnessSlider",
                  label = "Choose your preferred level of sweetness:",
                  min = min(data$Sweetness),
                  max = max(data$Sweetness),
                  value = round(mean(range(data$Sweetness))),
                  step = 1)
    })
    
    # present flavors button
    output$FlavorsButton <- renderUI({
      actionButton(inputId = "FlavorsButton", label = "Next: Select Flavors")
    })
    
    
  })
  
  
  # Update available flavors when flavors button is chosen
  observeEvent(input$FlavorsButton, {
    
    # Filter available flavors based on selected alcohol and sweetness
    if (!is.null(input$Alcohol)) {
      data <- data %>% filter(Alcohol %in% input$Alcohol)
    }
    
    # Sweetness + or - 1
    if (!is.null(input$SweetnessSlider)) {
      data <- data[data$Sweetness >= (input$SweetnessSlider - 1) & data$Sweetness <= (input$SweetnessSlider + 1),]
    }
    
    # Get list of available flavors based on filtered data
    flavors <- unique(unlist(strsplit(data$Flavors, ", ")))
    
    output$FlavorsCheckbox <- renderUI({
      checkboxGroupInput("FlavorsCheckbox", "Choose flavors that you prefer:", choices = flavors)
    }) 
    
    # present submit button
    output$SubmitButton <- renderUI({
      actionButton(inputId = "SubmitButton", label = "Find Drinks", icon = icon(name = "martini-glass-citrus"))
    })
  })
  
  
  drink_filter <- reactive({
    
    # Filter cocktails based on user preferences
    if (!is.null(input$Alcohol)) {
      data <- data[data$Alcohol %in% input$Alcohol,]
    }
    
    if (!is.null(input$SweetnessSlider)) {
      data <- data[data$Sweetness >= (input$SweetnessSlider - 1) & data$Sweetness <= (input$SweetnessSlider + 1),]
    }
    
    if (!is.null(input$FlavorsCheckbox)) {
      # get list of marked flavors from user
      selected_flavors <- input$FlavorsCheckbox
      
      # split comma-separated flavors into separate strings
      split_flavors <- str_split(data$Flavors, ",\\s*")
      
      # check if any of the selected flavors are present in each row
      selected_rows <- sapply(split_flavors, function(flavors) any(flavors %in% selected_flavors))
      
      # filter dataset to exclude selected rows
      data <- data[selected_rows, ]
    }
    data
  })
  
  # output table when submit button clicked
  observeEvent(input$SubmitButton, {
    final_data <- drink_filter()
    table <- final_data %>% select(-row_number)
    output$Table <- renderDataTable({
      table
    })
  })
  
  
  
  observeEvent(input$SubmitButton, {
    for (i in drink_filter()$row_number){
      local({
        
        Images = paste0(i, ".jpg")
        
        output[[Images]] <- renderImage({
          
          list(src = file.path('www', Images), 
               width=500, height=300)
        }, deleteFile=FALSE)
        
      })
    }
    
    
  })
  
  output$Images <- renderUI({
    
    image_output_list <- 
      lapply(drink_filter()$row_number,
             function(i){
               imagename = paste0(i, ".jpg")
               imageOutput(imagename)
             })
    
    do.call(tagList, image_output_list)
  })
}


shinyApp(ui = ui, server = server)

