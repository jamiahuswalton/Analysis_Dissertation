library(shiny)
library(rsconnect)
library(tidyverse)

aggregate_data <- read.csv("data/team_player_aggragate_stats.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Team Data Distrabution"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "DependentVariables", 
                  label = "Select the dependent variable you want to view", 
                  choices = c("Time Remaining", "Total Errors"))
      ),
    
    mainPanel(plotOutput("distrabution"))
    )
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distrabution <- renderPlot({
    
    # Functions used is documents ----
    remove_measures_with_given_value <- function(data_set, col_name, value){
      rows_to_move <- which(as.vector(data_set[,col_name]) == value) 
      
      return(data_set[-rows_to_move,])
    }
    
    # Factor columns that need it.
    re_factor_columns <- function(userData, columnNames){
      factorData <- userData
      for(column in columnNames){
        print(column)
        factorData[,column] <- factor(factorData[,column])
      }
      return(factorData)
    }
    
    # Clean data
    clean_aggregate <- remove_measures_with_given_value(data_set =  aggregate_data, col_name = "Condition", value = "A") # without none condition
    
    # Re factor the columns
    columns_to_refactor <- c("SessionOrder", 
                             "Team", 
                             "Player_ID", 
                             "Condition", 
                             "Dominate.Strategy", 
                             "Condition", 
                             "Target",
                             "Confident_team_comm_important_details_quickly")
    clean_aggregate_data_stats <- re_factor_columns(clean_aggregate, columns_to_refactor)
    
    
    # Team data set ----
    team_data <- clean_aggregate %>%
      filter(Player == 1)
    
    # Dependent variable
    dependent_response_team <- switch (input$DependentVariables,
      "Time Remaining" = "timeRemaining_team",
      "Total Errors" = "ERROR_team_total"
    )
    
    #dependent_response_team <- "timeRemaining_team"
    y_label_team <- "Count"
    x_label_team <- input$DependentVariables
    title_response_team <- "Distribution of Time Remaining (Team)"
    plot_name <- "Histogram_TimeRemaining_Team.png"
    #setwd(figure_directory)
    
    plot_data_team <- team_data %>%
      select(dependent_response_team, Target)
    
    ggplot(data = plot_data_team) + 
      geom_histogram(aes_string(x = dependent_response_team), bins = 30) +
      labs(title = title_response_team, x = x_label_team, y = y_label_team) 
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)