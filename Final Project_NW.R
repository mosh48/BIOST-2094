#load packages
library(openxlsx)
library(shiny)
library(readxl)
library(dslabs)
library(tidyverse)

#images
setwd("E:/University of Pittsburgh/BIOST 2094 Advanced R Computing/Project/")

#load data
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
    
    #checkboxGroupInput(inputId="alcohol",
    #                   label="Choose your alcohol preference(s):",
    #                   choices=c("Vodka", "Gin", "Rum", "Tequila", "Whiskey", "No Preference")),
    
    
    selectInput(inputId="alcohol_input",
                label="Choose your alcohol preference(s):",
                choices = data$Alcohol),
    
    
    #sweetness slider
    sliderInput(inputId = "Sweetness_input",
                label = "Choose your preferred level of sweetness:",
                min = 1,
                max = 5,
                value = 1),
    
    #alcohol content slider
    sliderInput(inputId = "ABV_input",
                label = "Choose your preferred alcohol content:",
                min = 9,
                max = 45,
                value = 9), # might want to do intervals of 5 or something
    
    
    #flavors input
    #checkboxGroupInput(inputId = "Flavors",
    #                   label = "Choose any flavors you would prefer:",
    #                   choices = unique(unlist(strsplit(data$Flavors, ", ")))),
                      #had to remove "," in column for each flavor
                      #list might be a bit long
      #maybe we could eliminate choices if there is not a drink that fits flavors
    
    
    
    
    
    selectInput(inputId="Flavors_input",
                label = "Choose any flavors you would prefer:",
                choices = Flavors_options),
    
    
    
    #submit button that looks like martini glass
    submitButton(text = "FindDrinks", icon = icon(name = "martini-glass-citrus"))
    
  ),
  
  
  #main panel for displaying outputs
  mainPanel(

    tableOutput("Drink1"),
    textOutput("Drink_image")
    #imageOutput("Drink_image")
    #tags$img(src = "image2.jpg")
  )
  
)
)


#define server logic
server <- function(input, output) {
  
  
  alcoholfilter <- reactive({
    
    if(input$Flavors_input == "No Preference"){
      data %>% filter(Alcohol == input$alcohol_input &
                                        Sweetness <= input$Sweetness_input &
                                        `ABV (%)` <= input$ABV_input)
    } else {
    
      data %>% filter(Alcohol == input$alcohol_input &
                                        Sweetness <= input$Sweetness_input &
                                        `ABV (%)` <= input$ABV_input & 
                                        str_detect(Flavors, input$Flavors_input))
    }
    
  })
  
  
  output$Drink1 <- renderTable({
    alcoholfilter()
  })
  
  
  output$Drink_image <- renderText({
    
    alcoholfilter()$row_number
    
  })
  
  #for (i in 1:10) {
  # Construct the filename
  #filename <- paste0("image", i, ".png")}

  
  
}



#Run the app
shinyApp(ui, server)




