library(shiny)
library(rsconnect)

source("helpers.R")
counties <- readRDS("data/counties.rds")
library(maps)
library(mapproj)

# Left off on https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("censusVis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with
               information from the 2010 US Census."),

      selectInput("var",
                  label = "Choose a variable to display",
                  choices = c("Percent White", "Percent Black",
                              "Percent Hispanic", "Percent Asian"),
                  selected = "Percent White"),

      sliderInput("range",
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      ),

    mainPanel(plotOutput("map"))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$map <- renderPlot({
    
    data <- switch (input$var,
                    "Percent White" = counties$white,
                    "Percent Black" = counties$black,
                    "Percent Hispanic" = counties$hispanic,
                    "Percent Asian" = counties$asian)
    
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var, 
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend)
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)