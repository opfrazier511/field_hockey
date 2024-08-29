###Hockey Shot Plotting App

library(shiny)
library(ggplot2)
library(dplyr)
library(aymR)
library(tidyverse)


#UI
ui <- fluidPage(
  titlePanel("Iowa Reactive Shot Plot"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Shot Details",
                 selectInput("playerInput", "Player Name", choices = c("Rachel Herbine", "Alex Wesneski", "Lieve van Kessel", "Tess Reed"
                                                                       , "Gia Whalen", "Miranda Jackson", "Sofie Stribos", "Dionne van Aaslum",
                                                                       "Sammy Freeman", "Milly Short", "Lauren DeRose",
                                                                       "Esme Gibson", "MJ McNary", "Lieve Schalk", "Ella Wareham",
                                                                       "Kelly Rose", "Harper Dunne", " Annika Herbine", "Hillary Cox",
                                                                       "Jacey Wittel","Celine De Witte", "Opponent")),
                 selectInput("teamInput", "Team", choices = c("Home", "Away")),
                 selectInput("goalInput", "Outcome", choices = c("Goal", "No Goal")),
                 selectInput("cornerInput", "Corner", choices = c("Not Corner", "Corner"))
        )
      ),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      plotOutput("shotPlot", click = "plot_click")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Store the shot data
  shot_data <- reactiveValues(data = data.frame(x = numeric(), y = numeric(), player = character(), team = character(), goal = character(),corner = character()))
  
  #Update Data on plot click
  observeEvent(input$plot_click, {
    point <- input$plot_click
    player <- isolate(input$playerInput)
    team <- isolate(input$teamInput)
    goal <- isolate(input$goalInput)
    corner <- isolate(input$cornerInput)
    shot_data$data <- rbind(shot_data$data, data.frame(x = point$x, y = point$y, player, team, goal,corner))
    })

    
  #Render shot plot
  output$shotPlot <- renderPlot({
    p <- ggplot(data = shot_data$data, aes(x, y)) +
      hockey_field_top_circle("#7CA867", "#7CA867", "#ffffff", "#ffffff", 2, 2, 1)
    
    # Add point layer for plot clicks
    if (!is.null(input$plot_click)) {
      click_data <- data.frame(x = input$plot_click$x, y = input$plot_click$y, goal = input$goalInput)
      
      # Check if the number of columns match
      if (ncol(click_data) == ncol(shot_data$data)) {
        shot_data$data <- rbind(shot_data$data, click_data)
      }
    }
    
    p <- p + geom_point(data = shot_data$data, aes(x, y, color = goal), size = 3) +
      scale_color_manual(values = c("green", "red"), labels = c("Goal", "No Goal")) +
      theme(legend.background = element_rect(fill = "lightgray"),
            legend.text = element_text(color = "black")) + labs(color = "Outcome")
    #as of right now only classifies as right color point if goal is first, if no goal is 
    #clicked first its the wrong color at first and then switches
    p
  }, height = 500) 
  
  
  
  # Download shot data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      "shot_data.csv"
    },
    content = function(file) {
      write.csv(shot_data$data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui,server)
