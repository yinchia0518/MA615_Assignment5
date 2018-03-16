library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
veg.1 <- read_xlsx("veg1.xlsx")

cnames.1 <- colnames(veg.1)

## try
n_distinct(veg.1[,1])

n_distinct(veg.1[,2])

#unique(veg.1[,2])

## now get the count for each column

c <- apply(veg.1, 2, n_distinct)
#c


#c[c>1]


d <- names(c[c==1])
#d

e <- names(c[c>1])
#e


veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)
#cnames.2

apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)
#cnames.3

#veg.3

unique(veg.3[,"Commodity"])

unique(veg.3[,"Data"]) %>% print(n=60)

unique(veg.3[,"Domain"])

unique(veg.3[,"Category"])

unique(veg.3[,"Value"])

yy <- separate(veg.3, Category, into = c("label", "quant"), sep=",")
# 
n_distinct(yy[,2])
# 
# 
unique(yy[,"label"]) %>% print(n=30)

ru <- filter(yy, label=="RESTRICTED USE CHEMICAL")

ru1 <- ru %>% select(label, quant) %>% unique()

ru1 %>% print(n=30)



## get CAS #

## find info at https://cfpub.epa.gov/ecotox/  (go to beta)
## or
## https://comptox.epa.gov/dashboard

##Toxicity effect levels

#

#Let's try to do something on veg.3

n_distinct(veg.3[,"Geo"])

unique(veg.3[,"Geo"])

filter(veg.3, Geo == "STATE") %>% print(n=800)

#get different datasets by commodity

veg_brocc <- filter(veg.3, Commodity== "BROCCOLI") %>% print(n=800)

#View(veg_brocc)

brocc_lb <- filter(veg_brocc, Data == "BROCCOLI - APPLICATIONS, MEASURED IN LB")

#brocc_lb
#View(brocc_lb)

################################################

ex1 <- separate(veg.3,Category,into = c("Label", "Quant"), sep=",")

#View(ex1)

ex2 <- separate(ex1, Quant, into=c("Type","Name"), sep = ":")

#View(ex2)

ex3 <- select(ex2, -Domain)
#View(ex3)

ex4 <- ex3 %>% separate(Label, into = c("Label","Type1"), by=":")

#View(ex4)

unique(ex4[,"Label"])

unique(ex3[,"Label"])



ex4 <- mutate(ex4, Type1 = replace(Type1, Type1 == "USE", NA))

ex4 <- mutate(ex4, Type1 = replace(Type1, Type1 == "SPECIFIED" , NA))

ex4$Type <- with(ex4,ifelse(is.na(Type),Type1,Type))

ex4 <- select(ex4, -Type1)



unique(ex4[,"Label"])

ex4 <- mutate(ex4, Label = replace(Label, Label == "RESTRICTED", "RESTRICTED USE CHEMICAL"))

ex4 <- mutate(ex4, Label = replace(Label, Label == "NOT", "NOT SPECIFIED"))


#View(ex4)

#LET'S TIDY UP A BIT THE DATA COLUMN

ex5 <- ex4 %>% separate(Data, into = c("name", "Measurement"), sep = "-")

ex6 <- ex5 %>% separate(Measurement, into = c("Measurement", "Info on Measurement"), sep = ",")

#View(ex6)


ex7 <-rename(ex6, "Info on Commodity"=name)



#View(ex7)

ex8 <- ex7 %>% separate(Name, into = c("Name", "ID"), sep = "=") %>% 
  separate(Name, into = c("Delete", "Name"), sep = "[()]") %>%
  separate(ID, into = c("ID", "Delete1"), sep = "[)]") %>%
  select(-Delete, -Delete1)

ex9 <- ex8 %>% mutate( Value = replace(Value, Value == "(NA)", NA))


#View(ex9)


#DATA EXPLORATION

ex10 <- ex9 %>% filter(!Value %in% c("(D)", "(Z)", NA))

#View(ex10)

#unique(ex10[,"Label"])


#View(ex11)



unique(ex9[,"Value"]) %>% print(n = 300)


unique(ex5[,"name"])

unique(ex5[,"Commodity"])

#View(unique(ex4[,"Data"]))



ex11 <- ex10 %>% filter(Label == "RESTRICTED USE CHEMICAL")

Br1 <- ex10 %>% filter(Commodity == "BROCCOLI", `Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

Br1$Value <- as.numeric(Br1$Value)

graph <- Br1 %>% group_by(Name) %>% summarize(mean(Value))

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)` )) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals in broccoli ") 


#View(Br1)
unique(ex10[,"Commodity"])

Cal1 <- ex10 %>% filter(Commodity == "CAULIFLOWER", `Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

Cal1$Value <- as.numeric(Cal1$Value)

graph <- Cal1 %>% group_by(Name) %>% summarize(mean(Value))

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)` )) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals in calliflowers ")


VegT <- ex10 %>% filter(  Label == "RESTRICTED USE CHEMICAL") 

Cal1$Value <- as.numeric(Cal1$Value)

graph <- Cal1 %>% group_by(Name) %>% summarize(mean(Value))

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)` )) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals in calliflowers ")


######################################################################################################################

#SHINY DASHBOARD
##using ex10 data


library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Temperature Dashboard"),
  dashboardSidebar(),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(h4("Summary"),
          verbatimTextOutput("summary"),
          )),
    fluidRow(
      box(width=7,
          plotOutput("plot1"),
          helpText("Data generated using the USDA QuickStats system.")),
      
      #controls
      box(width=5, 
        # Input: Select X and Y ----
        selectInput("xcol", "X Variable:",
                    choices = c("Year", "Geo", "Region","Commodity","Label","Type","Name")),
        selectInput("ycol", "Y Variable:",
                    choices = c("Commodity","Label", "Type", "Name","Value")),
        # Input: Select coloring variable ----
        selectInput("color", "Color-Grouping Variable:",
                    choices = c("None", "Geo", "Region","Commodity","Label","Type","Name")),
        
        # Include clarifying text ----
        helpText("Choose different X Variable and Coloring Variable for the best visual."),
        
        # Input: actionButton() to defer the rendering of output ----
        # until the user explicitly clicks the button (rather than
        # doing it immediately when inputs change). This is useful if
        # the computations required to render output are inordinately
        # time-consuming.
        actionButton("update", "Update View")      ))
    )
  )
)


server <- function(input, output) {
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    ex10[, c(input$xcol, input$ycol)]
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  ####################Not Done
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = input$color,
         pch = 20, cex = 3)
    points(input$ycol, pch = 4, cex = 4, lwd = 4)
  })
}

shinyApp(ui, server)