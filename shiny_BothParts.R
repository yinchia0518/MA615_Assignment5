library(tidyverse)
#Part1 data
annaulTemprature <- read_csv("annaulTemprature.csv")
annaulTemprature <- annaulTemprature[,-1]

#Part2 data
ex9 <- read_csv("ex9.csv")
ex9 <- subset(ex9[,-1])
tox1 <- read_csv("tox1.csv")
tox1 <- subset(tox1[,-1])

#SHINY DASHBOARD
##using ex10 data


library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Tidyverse Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Buoy 46035", tabName = "temp", icon = icon("th")),
      menuItem("Vegetables", icon = icon("th"), tabName = "veg"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "veg",
              # h3("Chemical Dashboard"),
    # Boxes need to be put in a row (or column)
              fluidRow(
                h4("Chemical Label Distribution and Toxicity"),
                column(width = 6,
                       box(width = NULL,
                           selectInput(inputId = "xcol",
                                       label = "X Variable for the histogram:",
                                       choices = c("Year","Commodity"),
                                       selected = "Commodity")),
                       box(width = NULL,
                           # width=4,
                           h5("Histogram"),
                           plotOutput(outputId = "main_plot", height = "330px"),
                           hr(),
                           helpText("NA observations neglected."))),
                column(width = 6,
                       box(width = NULL,
                           # width=5,
                           h5("Chemical Toxicity Plot"),
                           plotOutput(outputId = "plot2"),
                           hr(),
                           helpText("Chemical 'PRONAMIDE' belongs to Type 'HERBICIDE'.",
                                    br(),
                                    "All other chemicals belong to Type 'INSECTICIDE'."),
                           helpText("All results are assessed as 'acute risk' with priority level 6.")))
                
              ),
              fluidRow(
                h4("View the Dataset"),
                box(width=3,
                    selectInput(inputId = "com",
                                label = "Commodity Type",
                                choices = unique(ex9$Commodity),
                                selected = "CAULIFLOWER"),
                    checkboxGroupInput("show_vars", "Columns in the dataset to show:",
                                       c("Year","Region","Measurement","Info on Measurement","Label","Type","Name","ID","Value"), selected = c("Year","Info on Measurement","Name","Value"))
                ),
                
                box(width=9,
                    style = 'overflow-x: scroll',
                    DT::dataTableOutput("mytable1"))
              )
      ),
    
    #part 1
    tabItem(tabName = "temp",
            # h3("Buoy Dashboard"),
            fluidRow(
              box(width= 8,
                  h4("Summary"),
                  verbatimTextOutput("summary"),
                  # Include clarifying text ----
                  helpText("Note: while the data view will show only the specified",
                           "number of observations, the summary will still be based",
                           "on the full dataset of annual mean temperature.")),
              infoBoxOutput("sourcebox")
            ),
            fluidRow(
              box(
                h4("Bar Plot"),
                width = 8,
                plotOutput("plot1")),
              box(width = 4,
                  h5("Control"),
                  sliderInput("years", "Year Range:", 1988, 2017, c(1995,2010)),
                  radioButtons("type", "Substance Type:",
                               c("Air Temperature" = "Air Temperature",
                                 "Water Temperature" = "Water Temperature")),
                  hr(),
                  helpText("Data from  the NOAA National Data Buoy Center.")
              )
            )
            
      )
    )
  )
)




server <- function(input, output) {
  #Part 1
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- annaulTemprature
    summary(dataset)
  })
  
  output$plot1 <- renderPlot({
    dataset <- as.data.frame(annaulTemprature)
    rownames(dataset) <- dataset[,1]
    dataset <- dataset[as.character(c(input$years[1]:input$years[2])),input$type]
    barplot(dataset,
            main=paste("Annual Mean",input$type,sep=" "),
            ylab="Degrees Celsius",
            xlab="Year",
            names.arg = c(input$years[1]:input$years[2]))
  })
  output$sourcebox <- renderInfoBox({
    value <- 
      infoBox(
        "Data Source", "Collected by NOAA Weather Station buoy 46035 at 57.026 N 177.738 W. Published in the NOAA National
        Data Buoy Center.",
        icon = icon("list"),
        color = "blue", fill = FALSE
      )
  })
  
  
  #Part 2  
  output$main_plot <- renderPlot({
    data <- ex9[,c("Label",input$xcol)]
    if (input$xcol == "Commodity") {
      data <- data%>% filter(Commodity != "VEGETABLE TOTALS")
    }
    data %>% group_by("Label")
    if (input$xcol == "Commodity"){
      ggplot(data,aes(x= Commodity,color=Label,fill=Label)) + geom_histogram(stat="count") + theme(axis.text.x=element_text(size=7),legend.position = "top")
    }
    else if (input$xcol == "Year"){
      ggplot(data,aes(x= Year,color=Label,fill=Label)) + geom_histogram(stat="count")
    }
  })
  
  output$mytable1 <- DT::renderDataTable(
    DT::datatable(ex9 %>% filter(Commodity == input$com) %>% select(input$show_vars),
                  options = list(autowidth = TRUE,scrollX = TRUE,searching = FALSE))
  )
  
  output$plot2 <- renderPlot({
    data <- as.data.frame(tox1)
    rownames(data) <- data[,1]
    barplot(data[,5],
            border= c(1:12),
            ylab="Values for LD50 on rats",
            names.arg = rownames(data),
            cex.names=0.7,
            las=2)
  })
  
}

shinyApp(ui, server)