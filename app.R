#load appropriate packages
library(shiny)
library(readxl)
library(tidyverse)
library(jpeg)

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
    
    #submit button that looks like martini glass
    submitButton(text = "Find Drinks", icon = icon(name = "martini-glass-citrus"))
    
  ),
  #main panel for displaying outputs
  mainPanel(
    imageOutput("Images")
    #tableOutput("Table")
    
  )
)
)

#define server logic
server <- function(input, output) {
    
    reactive({
      
    #alcohol type filter
    data <- if(!"Vodka"%in%input$Alcohol){
      data <- data %>% filter(!grepl("Vodka", Alcohol))
    }else{data <- data}
    data <- if(!"Gin"%in%input$Alcohol){
      data <- data %>% filter(!grepl("Gin", Alcohol))
    }else{data <- data}
    data <- if(!"Rum"%in%input$Alcohol){
      data <- data %>% filter(!grepl("Rum", Alcohol))
    }else{data <- data}
    data <- if(!"Tequila"%in%input$Alcohol){
      data <- data %>% filter(!grepl("Tequila", Alcohol))
    }else{data <- data}
    data <- if(!"Whiskey"%in%input$Alcohol){
      data <- data %>% filter(!grepl("Whiskey", Alcohol))
    }else{data <- data}
    
    #sweetness filter
    data <- if(input$Sweetness==1){
      data <- data %>% filter(Sweetness<3)
    }else{data <- data}
    data <- if(input$Sweetness==2){
      data <- data %>% filter(Sweetness<4)
    }else{data <- data}
    data <- if(input$Sweetness==3){
      data <- data %>% filter(Sweetness!=1 | Sweetness!=5)
    }else{data <- data}
    data <- if(input$Sweetness==4){
      data <- data %>% filter(Sweetness>3)
    }else{data <- data}
    data <- if(input$Sweetness==5){
      data <- data %>% filter(Sweetness>4)
    }else{data <- data}
    
    
    #flavor filter
    data <- if(!"Orange"%in%input$Flavors){
      data <- data %>% filter(!grepl("Orange", Flavors))
    }else{data <- data}
    data <- if(!"Lime"%in%input$Flavors){
      data <- data %>% filter(!grepl("Lime", Flavors))
    }else{data <- data}
    data <- if(!"Lemon"%in%input$Flavors){
      data <- data %>% filter(!grepl("Lemon", Flavors))
    }else{data <- data}
    data <- if(!"Cranberry"%in%input$Flavors){
      data <- data %>% filter(!grepl("Cranberry", Flavors))
    }else{data <- data}
    data <- if(!"Tomato"%in%input$Flavors & !"Hot Sauce"%in%input$Flavors & !"Pepper"%in%input$Flavors){
      data <- data %>% filter(!grepl("Tomato", Flavors)) %>% filter(!grepl("Hot Sauce", Flavors)) %>% filter(!grepl("Pepper", Flavors))
    }else{data <- data}
    data <- if(!"Ginger"%in%input$Flavors){
      data <- data %>% filter(!grepl("Ginger", Flavors))
    }else{data <- data}
    data <- if(!"Coffee"%in%input$Flavors & !"Chocolate"%in%input$Flavors){
      data <- data %>% filter(!grepl("Coffee", Flavors)) %>% filter(!grepl("Chocolate", Flavors))
    }else{data <- data}
    data <- if(!"Grapefruit"%in%input$Flavors){
      data <- data %>% filter(!grepl("Grapefruit", Flavors))
    }else{data <- data}
    data <- if(!"Herbal"%in%input$Flavors){
      data <- data %>% filter(!grepl("Herbal", Flavors))
    }else{data <- data}
    data <- if(!"Cherry"%in%input$Flavors){
      data <- data %>% filter(!grepl("Cherry", Flavors))
    }else{data <- data}
    data <- if(!"Bitter"%in%input$Flavors){
      data <- data %>% filter(!grepl("Bitter", Flavors))
    }else{data <- data}
    data <- if(!"Spiced"%in%input$Flavors){
      data <- data %>% filter(!grepl("Spiced", Flavors))
    }else{data <- data}
    data <- if(!"Violet"%in%input$Flavors){
      data <- data %>% filter(!grepl("Violet", Flavors))
    }else{data <- data}
    data <- if(!"Banana"%in%input$Flavors){
      data <- data %>% filter(!grepl("Banana", Flavors))
    }else{data <- data}
    data <- if(!"Blackberry"%in%input$Flavors){
      data <- data %>% filter(!grepl("Blackberry", Flavors))
    }else{data <- data}
    data <- if(!"Pineapple"%in%input$Flavors){
      data <- data %>% filter(!grepl("Pineapple", Flavors))
    }else{data <- data}
    data <- if(!"Coconut"%in%input$Flavors){
      data <- data %>% filter(!grepl("Coconut", Flavors))
    }else{data <- data}
    data <- if(!"Almond"%in%input$Flavors){
      data <- data %>% filter(!grepl("Almond", Flavors))
    }else{data <- data}
    data <- if(!"Mint"%in%input$Flavors){
      data <- data %>% filter(!grepl("Mint", Flavors))
    }else{data <- data}
    data <- if(!"Pomegranite"%in%input$Flavors){
      data <- data %>% filter(!grepl("Pomegranite", Flavors))
    }else{data <- data}
    data <- if(!"Licorice"%in%input$Flavors){
      data <- data %>% filter(!grepl("Licorice", Flavors))
    }else{data <- data}
    data <- if(!"Honey"%in%input$Flavors){
      data <- data %>% filter(!grepl("Honey", Flavors))
    }else{data <- data}
    
    })
    
    #output$Table <- renderTable({
      #drink_filter()
    #})
    
    
    #build output list for cocktail images
    
    cocktails <- list()
    
    cocktails <- if("Cosmopolitan"%in%data$Name){
      cocktails <- append(cocktails,"www/image1.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Bloody Mary"%in%data$Name){
      cocktails <- append(cocktails,"www/image2.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Moscow Mule"%in%data$Name){
      cocktails <- append(cocktails,"www/image3.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Lemon Drop Martini"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-3.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Espresso Martini"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-4.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Sea Breeze"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-5.jpg")
    }else{cocktails <- cocktails}
    
    cocktails <- if("Gimlet"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-6.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Last Word"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-7.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Negroni"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-8.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("French 75"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-9.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Aviation"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-10.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Tom Collins"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-11.jpg")
    }else{cocktails <- cocktails}
    
    cocktails <- if("Dark and Stormy"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-12.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Rum Runner"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-13.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Daiquiri"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-14.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Pina Colada"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-15.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Mai Tai"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-16.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Mojito"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-17.jpg")
    }else{cocktails <- cocktails}
    
    cocktails <- if("Margarita"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-18.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Paloma"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-19.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Tequila Sunrise"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-20.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("El Diablo"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-21.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Naked and Famous"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-22.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Bloody Maria"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-23.jpg")
    }else{cocktails <- cocktails}
    
    cocktails <- if("Manhattan"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-24.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Old Fashioned"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-25.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Sazerac"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-26.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Mint Julep"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-27.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Penicillin"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-28.jpg")
    }else{cocktails <- cocktails}
    cocktails <- if("Paper Plane"%in%data$Name){
      cocktails <- append(cocktails,"www/p1gtketd9rtqdqmj1rar8m41qh54-29.jpg")
    }else{cocktails <- cocktails}
    
    
    #observe({
    #for (i in 1:length(cocktails)){
      #local({
      #j <- i
     # Images = cocktails[[j]]
    #  output[[Images]] <- renderImage({
   #   list(src=cocktails[[j]], width=500, height=300)
  #}, deleteFile=FALSE)
     # })
    #}
    #})
    
    
    #output$Images <- renderUI({
     # imageList <-
    #    lapply(1:length(cocktails),
     #          function(i){
      #           Images = cocktails[[i]]
       #          imageOutput(Images)
        #       })
      #do.call(tagList, imageList)
    #})
    
    #i <- 1
    for (i in 1:length(cocktails)){
      output$Images <- renderImage({
        list(src=cocktails[[i]], width=550, height=330)
      }, deleteFile=FALSE)
    }
    
    
    
  
}

  
#Run the app
shinyApp(ui, server)
