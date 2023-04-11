#load shiny, readxl, and tidyverse packages
library(shiny)
library(readxl)
library(tidyverse)

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
    
    #For now, this will only let you choose one alcohol type
    #selectInput(inputID = "Alcohol",
                #label="Choose your alcohol preference:",
                #choices=unique(data$Alcohol)),
    
    
    #sweetness slider
    sliderInput(inputId = "Sweetness",
                label = "Choose your preferred level of sweetness:",
                min = 1,
                max = 5,
                value = 3),
    
    #alcohol content slider. commented out for now for simplicity
    #sliderInput(inputId = "ABV",
                #label = "Choose your preferred alcohol content:",
                #min = min(data$ABV),
                #max = max(data$ABV),
                #value = 20), # might want to do intervals of 5 or something
    
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
    imageOutput("Images")
    
  )
)
)

#define server logic
server <- function(input, output) {
  
  output$Images <- renderImage({
    
    #alcohol type filter
    data <- ifelse(!"Vodka"%in%input$Alcohol, data %>% filter(!grepl("Vodka", Alcohol)), data)
    data <- ifelse(!"Gin"%in%input$Alcohol, data %>% filter(!grepl("Gin", Alcohol)), data)
    data <- ifelse(!"Rum"%in%input$Alcohol, data %>% filter(!grepl("Rum", Alcohol)), data)
    data <- ifelse(!"Tequila"%in%input$Alcohol, data %>% filter(!grepl("Tequila", Alcohol)), data)
    data <- ifelse(!"Whiskey"%in%input$Alcohol, data %>% filter(!grepl("Whiskey", Alcohol)), data)
    
    #sweetness filter
    data <- ifelse(input$Sweetness==1, data %>% filter(Sweetness<3), data)
    data <- ifelse(input$Sweetness==2, data %>% filter(Sweetness<4), data)
    data <- ifelse(input$Sweetness==3, data %>% filter(Sweetness!=1 | Sweetness!=5), data)
    data <- ifelse(input$Sweetness==4, data %>% filter(Sweetness>3), data)
    data <- ifelse(input$Sweetness==5, data %>% filter(Sweetness>4), data)
    
    #flavor filter
    data <- ifelse(!"Orange"%in%input$Flavors, data %>% filter(!grepl("Orange", Flavors)), data)
    data <- ifelse(!"Lime"%in%input$Flavors, data %>% filter(!grepl("Lime", Flavors)), data)
    data <- ifelse(!"Lemon"%in%input$Flavors, data %>% filter(!grepl("Lemon", Flavors)), data)
    data <- ifelse(!"Cranberry"%in%input$Flavors, data %>% filter(!grepl("Cranberry", Flavors)), data)
    data <- ifelse(!"Tomato"%in%input$Flavors & !"Hot Sauce"%in%input$Flavors & !"Pepper"%in%input$Flavors, 
                   data %>% filter(!grepl("Tomato", Flavors)) %>% filter(!grepl("Hot Sauce", Flavors)) %>% filter(!grepl("Pepper", Flavors)), data)
    data <- ifelse(!"Ginger"%in%input$Flavors, data %>% filter(!grepl("Ginger", Flavors)), data)
    data <- ifelse(!"Coffee"%in%input$Flavors & !"Chocolate"%in%input$Flavors, 
                   data %>% filter(!grepl("Coffee", Flavors)) %>% filter(!grepl("Chocolate", Flavors)), data)
    data <- ifelse(!"Grapefruit"%in%input$Flavors, data %>% filter(!grepl("Grapefruit", Flavors)), data)
    data <- ifelse(!"Herbal"%in%input$Flavors, data %>% filter(!grepl("Herbal", Flavors)), data)
    data <- ifelse(!"Cherry"%in%input$Flavors, data %>% filter(!grepl("Cherry", Flavors)), data)
    data <- ifelse(!"Bitter"%in%input$Flavors, data %>% filter(!grepl("Bitter", Flavors)), data)
    data <- ifelse(!"Spiced"%in%input$Flavors, data %>% filter(!grepl("Spiced", Flavors)), data) 
    data <- ifelse(!"Violet"%in%input$Flavors, data %>% filter(!grepl("Violet", Flavors)), data)
    data <- ifelse(!"Banana"%in%input$Flavors, data %>% filter(!grepl("Banana", Flavors)), data)
    data <- ifelse(!"Blackberry"%in%input$Flavors, data %>% filter(!grepl("Blackberry", Flavors)), data)
    data <- ifelse(!"Pineapple"%in%input$Flavors, data %>% filter(!grepl("Pineapple", Flavors)), data)
    data <- ifelse(!"Coconut"%in%input$Flavors, data %>% filter(!grepl("Coconut", Flavors)), data)
    data <- ifelse(!"Almond"%in%input$Flavors, data %>% filter(!grepl("Almond", Flavors)), data)
    data <- ifelse(!"Mint"%in%input$Flavors, data %>% filter(!grepl("Mint", Flavors)), data)
    data <- ifelse(!"Pomegranite"%in%input$Flavors, data %>% filter(!grepl("Pomegranite", Flavors)), data)
    data <- ifelse(!"Licorice"%in%input$Flavors, data %>% filter(!grepl("Licorice", Flavors)), data)
    data <- ifelse(!"Honey"%in%input$Flavors, data %>% filter(!grepl("Honey", Flavors)), data)
    
    #build output list for cocktail images
    cocktails <- list()
    
    cocktails <- ifelse("Cosmopolitan"%in%data$Name, append(cocktails,"image1.png"), cocktails)
    cocktails <- ifelse("Bloody Mary"%in%data$Name, append(cocktails,"image2.png"), cocktails)
    cocktails <- ifelse("Moscow Mule"%in%data$Name, append(cocktails,"image3.png"), cocktails)
    cocktails <- ifelse("Lemon Drop Martini"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-3.png"), cocktails)
    cocktails <- ifelse("Espresso Martini"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-4.png"), cocktails)
    cocktails <- ifelse("Sea Breeze"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-5.png"), cocktails)
    
    cocktails <- ifelse("Gimlet"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-6.png"), cocktails)
    cocktails <- ifelse("Last Word"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-7.png"), cocktails)
    cocktails <- ifelse("Negroni"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-8.png"), cocktails)
    cocktails <- ifelse("French 75"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-9.png"), cocktails)
    cocktails <- ifelse("Aviation"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-10.png"), cocktails)
    cocktails <- ifelse("Tom Collins"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-11.png"), cocktails)
    
    cocktails <- ifelse("Dark and Stormy"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-12.png"), cocktails)
    cocktails <- ifelse("Rum Runner"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-13.png"), cocktails)
    cocktails <- ifelse("Daiquiri"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-14.png"), cocktails)
    cocktails <- ifelse("Pina Colada"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-15.png"), cocktails)
    cocktails <- ifelse("Mai Tai"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-16.png"), cocktails)
    cocktails <- ifelse("Mojito"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-17.png"), cocktails)
    
    cocktails <- ifelse("Margarita"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-18.png"), cocktails)
    cocktails <- ifelse("Paloma"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-19.png"), cocktails)
    cocktails <- ifelse("Tequila Sunrise"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-20.png"), cocktails)
    cocktails <- ifelse("El Diablo"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-21.png"), cocktails)
    cocktails <- ifelse("Naked and Famous"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-22.png"), cocktails)
    cocktails <- ifelse("Bloody Maria"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-23.png"), cocktails)
    
    cocktails <- ifelse("Manhattan"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-24.png"), cocktails)
    cocktails <- ifelse("Old Fashioned"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-25.png"), cocktails)
    cocktails <- ifelse("Sazerac"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-26.png"), cocktails)
    cocktails <- ifelse("Mint Julep"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-27.png"), cocktails)
    cocktails <- ifelse("Penicillin"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-28.png"), cocktails)
    cocktails <- ifelse("Paper Plane"%in%data$Name, 
                        append(cocktails,"p1gtketd9rtqdqmj1rar8m41qh54-29.png"), cocktails)
    
  })

}

#Run the app
shinyApp(ui, server)
