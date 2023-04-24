#load appropriate packages
library(shiny)
library(readxl)
library(tidyverse)
library(jpeg)
library(DT)

#load data
data <- read_excel("ProjectData.xlsx")
data$image <- rownames(data)
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
    
    output$FlavorsCheckbox <- renderUI({
      checkboxGroupInput("FlavorsCheckbox", "Choose any flavors you would prefer:", choices = flavors)
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
    
    if (!is.null(input$FlavorsCheckbox)) {
      # get list of marked flavors from user
      selected_flavors <- input$FlavorsCheckbox
      
      # split comma-separated flavors into separate strings
      split_flavors <- str_split(filtered$Flavors, ",\\s*")
      
      # check if any of the selected flavors are present in each row
      selected_rows <- sapply(split_flavors, function(flavors) any(flavors %in% selected_flavors))
      
      # filter dataset to include selected rows
      filtered_data <- filtered[selected_rows, ]
      filtered_data <- filtered_data #%>% select(-image)
    }
    
    #Adds column of image path to remaining drinks
    #filtered$image <- paste0("www/", filtered$Name, ".jpg")
    
    #filtered <- data[data$image != "", ]
    filtered_data
    
  })
  
  # output table when submit button clicked
  observeEvent(input$SubmitButton, {
    final_data <- drink_filter()
    table <- final_data %>% select(-image)
    output$Table <- DT::renderDataTable({
      table
    })
    
    output$Images <- renderUI({
      image_output_list <- lapply(final_data$image, function(i) {
        imagename <- paste0(i, ".jpg")
        imageOutput(imagename)
      })
      
      do.call(tagList, image_output_list)
  })
  
    
    for (i in final_data$image){
      local({
        j <- i
        Images = paste0(j, ".jpg")
        #print(Images)
        
        output[[Images]] <- renderImage({
          
          list(src = file.path('wwww', Images), 
               width=500, height=300)
        }, deleteFile=FALSE)
        
      })
  
}
  })
}

# Run the app
shinyApp(ui = ui, server = server)

