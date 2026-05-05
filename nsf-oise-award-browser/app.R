library(shiny)
library(shinydashboard)
library(shinyBS)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(data.table)
library(ggplot2)
library(scales)

# Load clean data
awards <- read.csv('oiseAwards.csv')
assign("awards", awards, envir = .GlobalEnv)

# Convert dates to date
awards$endDate <- as.Date(awards$endDate, format="%m/%d/%Y")
awards$lastAmendmentDate <- as.Date(awards$lastAmendmentDate, format="%m/%d/%Y")
awards$endDate <- as.Date(awards$endDate, format="%m/%d/%Y")

# Remove dollar symbol from column
awards$awardedAmountToDate <- as.numeric(gsub("[\\$,]", "", awards$awardedAmountToDate))

# User interface logic
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Adds vertical space below each fluidRow */
      .row {
        margin-bottom: 40px;
      }
      /* Adds vertical space below the title panel */
      .shiny-title-panel {
        margin-bottom: 40px;
      }
      /* Adds vertical space below each input group */
      .form-group {
        margin-bottom: 40px;
      }
    "))
  ),
  
  titlePanel(
    tags$h2("NSF Office of International Science & Engineering Awards", 
      style = "color: #005ea2;")
  ),
  
  # Input columns
  fluidRow(
    column(4,  
      selectInput("year", "Select Year:", 
        choices = c("All Years" = "All Years", sort(unique(format(awards$endDate, "%Y")))),
        selected = "All Years"),
      actionButton("info_year", label = "Year Info", style = "background-color: #005ea2; color: #ffffff;")
    ),
    column(4,
      selectInput("program", "Select Program:", 
        choices = c("All Programs" = "All Programs", unique(awards$program)),
        selected = "All Programs"),
      actionButton("info_program", label = "Program Info", style = "background-color: #005ea2; color: #ffffff;")
    ),
    column(4,
      selectInput("epscor", "EPSCoR Status:",
        choices = c("All" = "All", "EPSCoR" = "EPSCoR", "Not EPSCoR" = "notEPSCoR"),
        selected = "All"),
      actionButton("info_epscor", label = "EPSCoR Info", style = "background-color: #005ea2; color: #ffffff;")
    )
  ),
  
  # Main panel
  fluidRow(
    column(12,
      tabsetPanel(
        tabPanel("Map", 
          div(style = "height: 80vh;", plotlyOutput("mapOutput", height = "100%", width = "100%")),
          div(style = "color: #005ea2; padding: 10px; font-style: bold;", 
            "Explore the interactive map by clicking on a point for more information. The map will rescale according to user filters. Three awards to University of Alaska Fairbanks and two awards to University of Hawaii not shown."
          )
        ),
        tabPanel("Plot", 
          div(style = "height: 80vh;", plotlyOutput("plotOutput", height = "100%", width = "100%")),
          div(style = "color: #005ea2; padding: 10px; font-style: bold;", 
            "Explore the interactive plot by clicking on a point for specific data. Type of Award: Standard awards are funded in full at time of award; continuing awards are funded incrementally over more than one year."
          )
        ),
        tabPanel("Line Chart", 
          div(style = "height: 80vh;", plotOutput("lineChartOutput", height = "100%", width = "100%")),
          div(style = "color: #005ea2; padding: 10px; font-style: bold;", 
            "Explore programmatic trends in funding across the years. Filter by program to reset scale by budget and award sizes."
          )
        ),
        tabPanel("Table",
          DTOutput("tableOutput"),
          div(style = "color: #005ea2; padding: 10px; font-style: bold;", 
            "Filter the table by the selected criteria from the sidebar and sort columns by using the arrows in the header row. Standard awards are funded in full at time of award; continuing awards are funded incrementally over more than one year."
          )
        )
      )
    )
  )
)


# Server logic
server <- function(input, output, session) {
  source('helper.R')
  
  observeEvent(input$info_year, {
    showModal(modalDialog(
      title = "About the Years",
      HTML("There are no AccelNet awards prior to 2019.<br/>
                There are no Global Centers awards prior to 2022.<br/>
                There are no data for interim years between PIRE programs."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_program, {
    showModal(modalDialog(
      title = "About the Programs",
      HTML("AccelNet funds international networking activities to advance science.<br/>
            IRES funds international research experiences for students<br/>
            Global Centers is a multilateral use-inspired topic-based funding program.<br/>
            PIRE funds partnerships in international research and education."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$info_epscor, {
    showModal(modalDialog(
      title = "About EPSCoR Status",
      HTML("EPSCoR status filters the data based on whether the projects are part of the EPSCoR program. There are currently 26 EPSCoR jurisdictions identified as targets for enhanced research competitiveness."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Reactive data for map, includes all states
  filteredData <- reactive({
    data <- awards
    
    if (input$year != "All Years") {
      data <- data %>% filter(format(endDate, "%Y") == input$year)
    }
    
    if (input$program != "All Programs") {
      data <- data %>% filter(program == input$program)
    }
    
    if (input$epscor != "All") {
      data <- data %>% filter(EPSCoR == input$epscor)
    }
    
    data %>% mutate(popupContent = paste(
      "Award ID: ", awardID, "<br>",
      "Program: ", program, "<br>", 
      "Track: ", as.factor(track), "<br>", 
      "Amount Received: ", scales::dollar(awardedAmountToDate), "<br>",
      "Awardee: ", organization))
  })
  
  output$mapOutput <- renderPlotly({
    generatePlotlyJitterMap(filteredData())
  })
  
  output$plotOutput <- renderPlotly({
    generatePlot(filteredData())
  })
  
  output$lineChartOutput <- renderPlot({
    generateLineChart(filteredData())
  })
  
  output$tableOutput <- renderDT({
    generateTable(filteredData())
  })
}

shinyApp(ui = ui, server = server)
