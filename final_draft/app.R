library(shiny)
library(tidyverse)
library(shinythemes)

uah <- read_delim("UAH-lower-troposphere-long.csv.bz2")

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Global Temperature",
             tabPanel("About",
                      p("The University of Alabama in Huntsville (UAH) uses data collected by satellites to get",
                      strong("precise and accurate global temperature readings from all over the world.")),
                      p("Regions that are difficult to collect data on, such as deserts, oceans, and rainforests, are made possible with the satellites."),
                      p("Once this data is collected, it is provided for data scientists from all over the world to analyze."),
                      p("The dataset contains",
                      em("14310 observations and 4 variables.")),
                      p("Here is a small (random) sample of data:"),
                      mainPanel(tableOutput("about"))),
             tabPanel("Plot",
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            column(6,
                                   radioButtons("color", "Choose color",
                                                choices = c("black","skyblue", "lawngreen", "orangered",
                                                                     "purple", "gold"))
                            )
                          ),
                          radioButtons("year", "Select the Year:",
                                       choices = unique(uah$year),
                                       selected = 1978)
                        ),
                        mainPanel( plotOutput("plot"),
                                   textOutput("result")
                        )
                      )
             ),
             tabPanel("Table",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("region",
                                      "Select Region:", choices = c("All", unique(uah$region)),
                                      selected = NULL,
                                      multiple = FALSE
                          )
                        ),
                        mainPanel(
                          textOutput("table_text"),
                          tableOutput("table")
                        )
                      )
             )
  )
)
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  sample <- reactive({
    uah %>% 
      filter(year %in% input$year)
  })
  
  output$about <- renderTable({
    uah %>% 
      sample_n(5)
  })
  
  output$plot <- renderPlot({
    data <- sample() %>%
      group_by(month) %>% 
      filter(!is.na(month)) %>% 
      summarise(avg_year = sum(temp)/n()) 
    ggplot(data, aes(month, avg_year)) +
      geom_point(col=input$color) +
      geom_line(col=input$color) +
      ylim(-1, 1) +
      scale_x_continuous(breaks = 1:12) +
      labs(title = "Average Temperature of Each Month by Year", x = "Month",
           y = "Average Temperature")
  })
  
  output$result <- renderText({
    num_obvs <- nrow(sample())
    paste(input$year, "has", num_obvs, "observations")
  })
  
## Where the table is being created
  sample1 <- reactive({
    if (input$region == "All") {
      uah
    }else{
      uah[uah$region == input$region,]
    }
  })
  
  output$table <- renderTable({
    data <- sample1() 
    data 
  })
  
  output$table_text <- renderText({
    num_obvs <- nrow(sample1())
    paste(input$region, "has", num_obvs, "observations")
  })
  
} 

# Run the application 
shinyApp(ui = ui, server = server)



