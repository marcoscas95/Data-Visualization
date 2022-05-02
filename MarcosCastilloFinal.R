library(shiny)
library(tidyverse)
library(scales)
library(ggthemes)
library(plotly)

happiness <- read.csv("world-happiness-report-2021.csv", stringsAsFactors = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("World Happiness Report 2021"),
  
  selectInput(inputId = "region",
              label = "Select Region:",
              choices = c("Western.Europe" = "Western.Europe", "North America and ANZ" = "North.America.and.ANZ", 
                          "Middle East and North Africa" = "Middle.East.and.North.Africa", 
                          "Latin America and Caribbean" = "Latin.America.and.Caribbean", 
                          "Central and Eastern Europe" = "Central.and.Eastern.Europe", "East Asia" = "East.Asia")),
  
  sliderInput(inputId = "gdp",
              label = "GDP Per Capita:",
                  min = 6.635,
                  max = 11.647,
                  value = 6.635),
  
  plotlyOutput("Plot1"),
  plotlyOutput("Plot2"),
  plotlyOutput("Plot3"),
  plotlyOutput("Plot4")
)

# Server Side (Back-end)
server <- function(input, output){
  
  output$Plot1 <- renderPlotly({
    plot1 <- happiness %>% filter(Regional.indicator == input$region,
                                  Logged.GDP.per.capita >= input$gdp)%>%
      
      ggplot () +
      geom_bar (mapping = aes(x = Regional.indicator, y = Ladder.score,
                               text = paste(
                                 "Region:", Regional.indicator,
                                 "\nHappiness Score:", Ladder.score
                               )), color = "green")
    
    ggplotly(plot1, tooltip = "text")
    
  })
  
  output$Plot2 <- renderPlotly({
    plot2 <- happiness %>% filter(Regional.indicator == input$region,
                                  Logged.GDP.per.capita >= input$gdp)%>%
                                           
    ggplot() +
      geom_point(mapping = aes(x = Logged.GDP.per.capita, y = Ladder.score,
                               text = paste(
                                 "GDP Per Capita:", Logged.GDP.per.capita,
                                 "\nHappiness Score:", Ladder.score
                               )), color = "black")
    
    ggplotly(plot2, tooltip = "text")
    
  })
    
    output$Plot3 <- renderPlotly({
      plot3 <- happiness %>% filter(Logged.GDP.per.capita >= input$gdp, 
                                    Regional.indicator == input$region)%>%
        
        ggplot() +
        geom_point(mapping = aes(x = Freedom.to.make.life.choices , y = Perceptions.of.corruption,
                                 size = Ladder.score,
                                 text = paste(
                                   "Freedom to Make Life Choices:", Freedom.to.make.life.choices,
                                   "\nPerceptions of Corruptions:", Perceptions.of.corruption
                                 )), color = "black")
      
      ggplotly(plot3, tooltip = "text")
  })
    
    output$Plot4 <- renderPlotly({
      plot4 <- happiness %>% filter(Logged.GDP.per.capita >= input$gdp, 
                                    Regional.indicator == input$region)%>%
        
        ggplot() +
        geom_point(mapping = aes(x = Healthy.life.expectancy , y = Ladder.score,
                                 size = Social.support,
                                 text = paste(
                                   "Life Expectancy:", Freedom.to.make.life.choices,
                                   "\nHappiness score:", Ladder.score
                                 )), color = "black")
      
      ggplotly(plot4, tooltip = "text")
    })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)