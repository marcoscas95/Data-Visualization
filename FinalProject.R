#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
library(ggthemes)
library(plotly)

sales <- read.csv("business.retailsales.csv", stringsAsFactors = T)

# User Interface
ui <- fluidPage(
  
  titlePanel("Retail Sales"),
  
  checkboxGroupInput("ProducType", label = h3("Product Type"),
               choices = list("Basket" = "Basket", "Art & Sculpture" = "Art.&.Sculpture", "Accessories" = "Accessories", "Christmas" = "Christmas","Easter" = "Easter", "Fair Trade Gifts" = "Fair.Trade.Gifts", 
                           "Furniture" = "Furniture", "Gift Baskets" = "Gift.Baskets", "Home Decor" = "Home.Decor", "Jewelry" = "Jewelry", "Kids" = "Kids", "Kitchen" = "Kitchen",
                           "Music" = "Music", "One-of-a-Kind" = "One-of-a-Kind", "Recycled Art" = "Recycled.Art", "Skin Care" = "Skin.Care", "Soapstone" = "Soapstone", "Textiles" = "Textiles"), 
               selected = "Basket"),
  
  sliderInput(inputId = "Discounts",
              label = "Discounts:",
              min = -600,
              max = 0,
              value = -88),
  
  sliderInput(inputId = "Returns",
              label = "Returns:",
              min = -1609,
              max = 0,
              value = -81),
  
  sliderInput(inputId = "Net.Quantity",
              label = "Net Quantity:",
              min = 0,
              max = 100,
              value = 50),
  
  plotlyOutput("myPlot"),
  plotlyOutput("myplot2"),
)

# Server Side (Back-end)
server <- function(input, output){
  
  output$myPlot <- renderPlotly({
    
    # Creating Linear Model
    lm_model <- lm(Gross.Sales ~ Net.Quantity + Discounts + Returns, data = sales)
    
    # Generating new data frame from input data
    new_data <- data.frame(Net.Quantity = input$Net.Quantity, 
                           Discounts = input$Discounts,
                           Returns = input$Returns)
    
    # Prediction
    prediction <- predict(lm_model, newdata = new_data)
    
    # Create Data frame for prediction point
    new_point <- data.frame(Gross.Sales = prediction, 
                            Net.Quantity = input$Net.Quantity, 
                            Discounts = input$Discounts,
                            Returns = input$Returns)
    
    
    plot <- sales %>% filter( Product.Type == input$ProducType)%>%
      ggplot() +
      geom_point(mapping = aes(x = Gross.Sales, y = Discounts,
                               text = paste(
                                 "Gross Sales:", dollar(Gross.Sales),
                                 "\nNet Quantity:", Net.Quantity,
                                 "\nDiscounts:", dollar(Net.Quantity),
                                 "\nReturns:", dollar(Returns)
                               )), alpha = 0.5)+
    
    # Add Regression Line
    geom_smooth(mapping = aes(x = Gross.Sales, y = Discounts),  
                              method =  "lm", se = F, color = "black") +
      labs(title = paste("Gross Sales vs Discounts | Gross Sales:", dollar(prediction))) +
      
    # Add Predicted Point
    geom_point(data = new_point, mapping = aes(x = Gross.Sales, y = Discounts,
                                                 text = paste(
                                                   "Gross Sales:", dollar(Gross.Sales),
                                                   "\nNet Quantity:", Net.Quantity,
                                                   "\nDiscounts:", dollar(Net.Quantity),
                                                   "\nReturns:", dollar(Returns)
                                                 )), color = "green")
    
    ggplotly(plot, tooltip = "text")
    
  })
  
  output$myPlot2 <- renderPlotly({
    
    plot2 <- sales %>% filter( Product.Type == input$ProducType)%>%
      ggplot() +
      geom_point(mapping = aes(x = Returns, y = Discounts,
                               text = paste(
                                 "Gross Sales:", dollar(Gross.Sales),
                                 "\nNet Quantity:", Net.Quantity,
                                 "\nDiscounts:", dollar(Net.Quantity),
                                 "\nReturns:", dollar(Returns)
                               )), alpha = 0.5)+
      
      # Add Regression Line
      geom_smooth(mapping = aes(x = Returns, y = Discounts),  
                  method =  "lm", se = F, color = "black") +
      labs(title = paste("Returns vs Discounts | Gross Sales:", dollar(prediction))) +
      
      # Add Predicted Point
      geom_point(data = new_point2, mapping = aes(x = Returns, y = Discounts,
                                                 text = paste(
                                                   "Gross Sales:", dollar(Gross.Sales),
                                                   "\nNet Quantity:", Net.Quantity,
                                                   "\nDiscounts:", dollar(Net.Quantity),
                                                   "\nReturns:", dollar(Returns)
                                                 )), color = "green")
    
    ggplotly(plot2, tooltip = "text")
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)