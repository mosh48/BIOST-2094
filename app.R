#load shiny, readxl, and tidyverse packages
library(shiny)
library(readxl)
library(tidyverse)

#load data
data <- read_excel("ProjectData.xlsx")
Flavors <- unique(unlist(strsplit(data$Flavors, ", ")))
data2 <- data %>% filter(!grepl("Ginger", Flavors))

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
    uiOutput("Cocktails")
    
  )
)
)

#define server logic
server <- function(input, output) {
  
  drinkFilter <- reactive({
    #alcohol type filter
    if(input$Alcohol!="Vodka"){
      data %>% filter(Alcohol!="Vodka")}
    if(input$Alcohol!="Rum"){
      data %>% filter(Alcohol!="Rum")}
    if(input$Alcohol!="Whiskey"){
      data %>% filter(Alcohol!="Whiskey")}
    if(input$Alcohol!="Tequila"){
      data %>% filter(Alcohol!="Tequila")}
    if(input$Alcohol!="Gin"){
      data %>% filter(Alcohol!="Gin")}
    
    #sweetness filter
    if(input$Sweetness==1){
      data %>% filter(Sweetness<3)}
    if(input$Sweetness==2){
      data %>% filter(Sweetness<4)}
    if(input$Sweetness==3){
      data %>% filter(Sweetness!=1 | Sweetness!=5)}
    if(input$Sweetness==4){
      data %>% filter(Sweetness>3)}
    if(input$Sweetness==5){
      data %>% filter(Sweetness>4)}
    
    #flavor filter
    if(!"Orange"%in%input$Flavors){
      data %>% filter(!grepl("Orange", Flavors))}
    if(!"Lime"%in%input$Flavors){
      data %>% filter(!grepl("Lime", Flavors))}
    if(!"Lemon"%in%input$Flavors){
      data %>% filter(!grepl("Lemon", Flavors))}
    if(!"Cranberry"%in%input$Flavors){
      data %>% filter(!grepl("Cranberry", Flavors))}
    if(!"Tomato"%in%input$Flavors & !"Hot Sauce"%in%input$Flavors & !"Pepper"%in%input$Flavors){
      data %>% filter(!grepl("Tomato", Flavors)) %>% filter(!grepl("Hot Sauce", Flavors)) %>% filter(!grepl("Pepper", Flavors))}
    if(!"Ginger"%in%input$Flavors){
      data %>% filter(!grepl("Ginger", Flavors))}
    if(!"Coffee"%in%input$Flavors & !"Chocolate"%in%input$Flavors){
      data %>% filter(!grepl("Coffee", Flavors)) %>% filter(!grepl("Chocolate", Flavors))}
    if(!"Grapefruit"%in%input$Flavors){
      data %>% filter(!grepl("Grapefruit", Flavors))}
    if(!"Herbal"%in%input$Flavors){
      data %>% filter(!grepl("Herbal", Flavors))}
    if(!"Cherry"%in%input$Flavors){
      data %>% filter(!grepl("Cherry", Flavors))}
    if(!"Bitter"%in%input$Flavors){
      data %>% filter(!grepl("Bitter", Flavors))}
    if(!"Spiced"%in%input$Flavors){
      data %>% filter(!grepl("Spiced", Flavors))}
    if(!"Violet"%in%input$Flavors){
      data %>% filter(!grepl("Violet", Flavors))}
    if(!"Banana"%in%input$Flavors){
      data %>% filter(!grepl("Banana", Flavors))}
    if(!"Blackberry"%in%input$Flavors){
      data %>% filter(!grepl("Blackberry", Flavors))}
    if(!"Pineapple"%in%input$Flavors){
      data %>% filter(!grepl("Pineapple", Flavors))}
    if(!"Coconut"%in%input$Flavors){
      data %>% filter(!grepl("Coconut", Flavors))}
    if(!"Almond"%in%input$Flavors){
      data %>% filter(!grepl("Almond", Flavors))}
    if(!"Mint"%in%input$Flavors){
      data %>% filter(!grepl("Mint", Flavors))}
    if(!"Pomegranite"%in%input$Flavors){
      data %>% filter(!grepl("Pomegranite", Flavors))}
    if(!"Licorice"%in%input$Flavors){
      data %>% filter(!grepl("Licorice", Flavors))}
    if(!"Honey"%in%input$Flavors){
      data %>% filter(!grepl("Honey", Flavors))}
  })

}

#Run the app
shinyApp(ui, server)
