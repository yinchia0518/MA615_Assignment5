library(tidyverse)

ex9 <- read_csv("ex9.csv")
ex9 <- subset(ex9[,-1])
tox1 <- read_csv("tox1.csv")
tox1 <- subset(tox1[,-1])
######################################################################################################################

#SHINY DASHBOARD
##using ex10 data


library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Chemical Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      h3("Chemical Label Distribution and Toxicity"),
      column(width = 6,
        box(width = NULL,
          selectInput(inputId = "xcol",
                    label = "X Variable for the histogram:",
                    choices = c("Year","Commodity"),
                    selected = "Commodity")),
        box(width = NULL,
          # width=4,
          h4("Histogram"),
          plotOutput(outputId = "main_plot", height = "330px"),
          hr(),
          helpText("NA observations neglected."))),
      column(width = 6,
        box(width = NULL,
          # width=5,
            h4("Chemical Toxicity Plot"),
            plotOutput(outputId = "plot2"),
            hr(),
            helpText("Chemical 'PRONAMIDE' belongs to Type 'HERBICIDE'.",
                      br(),
                     "All other chemicals belong to Type 'INSECTICIDE'."),
            helpText("All results are assessed as 'acute risk' with priority level 6.")))
        
    ),
    fluidRow(
      h3("View the Dataset"),
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
  )
)




server <- function(input, output) {
  
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