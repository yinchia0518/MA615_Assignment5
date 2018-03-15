## station 46035 has missing data in 2012 and 2013
## we replaced the data of these two years with nearby buoy(46070)
library(tidyverse)
library(stringr)

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46070h"
url3 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1988:2017)
urls1 <- str_c(url1, years, url3, sep = "")
urls2 <- str_c(url2, years, url3, sep = "")
filenames <- str_c("mr", years, sep = "")
N <- length(years)

#create files for each year 
i<-1
while (i <= N){
  
  if(i==25|i==26){
    suppressMessages(
      assign(filenames[i], read_table(urls2[i], col_names = TRUE)))
  }
  else{
    suppressMessages(
      assign(filenames[i], read_table(urls1[i], col_names = TRUE)))
  }
  file <- get(filenames[i])
  
  #add 19 in front of 2 digits year(from 1988 to 1998)
  colnames(file)[1] <-"YYYY"
  if(i>=1 && i<=11){
    file$YYYY<-paste("19", file$YYYY, sep = "")
  }
  #print(file)
  #only select time, air temp and water temp
  file <- subset(file, select=c("YYYY", "MM","DD","hh","ATMP","WTMP"))
  file$ATMP<-as.numeric(file$ATMP)
  file$WTMP<-as.numeric(file$WTMP)
  file <- subset(file, ATMP!= 999.0 &ATMP!= 99.0)
  file <- subset(file, WTMP!= 999.0 &WTMP!= 99.0)
  # file <- file[-outliersIndex(file$ATMP),]
  # file <- file[-outliersIndex(file$WTMP),]
  annualMean_AT <- mean(file$ATMP, na.rm=TRUE)
  annualMean_WT <- mean(file$WTMP, na.rm=TRUE)
  
  if(i==1){
    MR <- file
    annualMeans_AT <- annualMean_AT
    annualMeans_WT <- annualMean_WT
  }
  else{
    MR <- rbind.data.frame(MR, file)
    annualMeans_AT <- rbind.data.frame(annualMeans_AT, annualMean_AT)
    annualMeans_WT <- rbind.data.frame(annualMeans_WT, annualMean_WT)
  }
  #print(MR)
  i<-i+1
}

#combine all dates and times into one column for creating time series graph
MR$FullTime <-strptime(with(MR, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
MR$FullTime

#only select daily noon data 
MR_DailyNoon <- subset(MR, hh==12)

library("reshape2")
library("ggplot2")

ggplot(MR_DailyNoon, aes(x = FullTime)) + 
  geom_line(aes(y = ATMP), colour="blue", size = 0.5) + 
  geom_line(aes(y = WTMP), colour = "grey", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")


outliersIndex <- function(data) {
  q1 <- quantile(data)[2]
  q3 <- quantile(data)[4]
  iqr <- IQR(data)
  
  threshold.upper = (iqr * 3) + q3
  threshold.lower = q1 - (iqr * 3)
  index <- which(data > threshold.upper | data < threshold.lower) #retrun positions that are TRUE in the vector
  return(index) 
}

#create annual temprature table, which containing means of temperatures in each year
annaulTemprature <- data.frame(c(1988:2017), annualMeans_AT, annualMeans_WT)
colnames(annaulTemprature) <- c("Year", "Air Temperature", "Water Temperature")


ggplot(annaulTemprature, aes(x = Year)) + 
  geom_line(aes(y = annualMeans_AT), colour="blue", size = 0.5) + 
  geom_line(aes(y = annualMeans_WT), colour = "red", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")

######################################################################################################################

#Statistics

##1. Test the difference based on choice of time of the day

MR_Daily20 <- subset(MR, hh==20)[,-c(4,7)]
  ##obtaining same format data for daily tmp at 20:00
colnames(MR_Daily20) <- c("YYYY","MM","DD","ATMP20","WTMP20")
MR_NoonVS20 <- as.data.frame(left_join(MR_DailyNoon[,-c(4,7)],MR_Daily20))
t.test(MR_NoonVS20["ATMP"],MR_NoonVS20["ATMP20"])    ##test on air tmp
  ##p=0.3262 > 0.05, we cannot reject the null hypothesis that daily noon and 17:00
  ##air tmp have the same mean
t.test(MR_NoonVS20["WTMP"],MR_NoonVS20["WTMP20"])    ##test on water tmp
  ##p=0.7733 > 0.05, we cannot reject the null hypothesis that daily noon and 17:00
  ##water tmp have the same mean

##Choice of the hour during a day does not make a difference

##2. Test the difference from 1988 to 2017

MR_1988 <- subset(MR, YYYY==1988)[,-c(1,7)]  
MR_2017 <- subset(MR, YYYY==2017)[,-c(1,7)]
colnames(MR_2017) <- c("MM","DD","hh","ATMP17","WTMP17")
##obtaining same format data for daily tmp at 17:00
MR_1988VS2017 <- as.data.frame(left_join(MR_1988,MR_2017))
t.test(MR_1988VS2017["ATMP"],MR_1988VS2017["ATMP17"])    ##test on air tmp
##p=0 < 0.05, we can reject the null hypothesis that in 1988 and 2017
##air tmp have the same mean
t.test(MR_1988VS2017["WTMP"],MR_1988VS2017["WTMP17"])    ##test on water tmp
##p=0 > 0.05, we can reject the null hypothesis that in 1988 and 2017
##water tmp have the same mean

##There are significant changes in the past 30 years

######################################################################################################################

#SHINY DASHBOARD
##Need to fix year-range slider input


library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Temperature Dashboard"),
  dashboardSidebar(),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1")),
    
      #control years
      box(
        title = "Controls",
        sliderInput("years", "Year Range:", 1988, 2017, c(1995,2010)),
        radioButtons("type", "Substance Type:",
                     c("Air Temperature" = "Air",
                       "Water Temperature" = "Water")),
        hr(),
        helpText("Data from  the NOAA National Data Buoy Center."),
        actionButton("update", "Update View")
        )
      )
    )
)


server <- function(input, output) {
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- eventReactive(input$update, {
    histdata <- switch(input$type,
                   Air = annaulTemprature[,c("Year","Air Temperature")],
                   Water = annaulTemprature[,c("Year","Water Temperature")],
                   annaulTemprature[,1:2])
    histdata <- histdata[input$years[1]-1987:input$years[2]-1987,]
    rownames(histdata) <- histdata[,1]
    histdata <- histdata[,-1]
  }, ignoreNULL = FALSE)
  
  output$plot1 <- renderPlot({
    barplot(d(),
            main=input$type,
            ylab="Degrees Celsius",
            xlab="Year")
  })
}

shinyApp(ui, server)