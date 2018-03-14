http://www.ndbc.noaa.gov/view_text_file.php?filename=46070h2012.txt.gz&dir=data/historical/stdmet/
  
library(tidyverse)
library(stringr)

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46070h"
url3 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1988:2017)
urls1 <- str_c(url1, years, url3, sep = "")
urls2 <- str_c(url2, years, url3, sep = "")
filenames <- str_c("mr", years, sep = "")
N <- length(urls)

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
  #only select time, air temp and water temp
  file <- subset(file, select=c("YYYY", "MM","DD","hh","ATMP","WTMP"))
  file$ATMP<-as.numeric(file$ATMP)
  file$WTMP<-as.numeric(file$WTMP)
  
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
  geom_point(aes(y = ATMP), colour="blue", size = 0.5) + 
  geom_point(aes(y = WTMP), colour = "grey", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")


#create annual temprature table, which containing means of temperatures in each year
annaulTemprature <- data.frame(c(1988:2017), annualMeans_AT, annualMeans_WT)
colnames(annaulTemprature) <- c("Year", "Air Temperature", "Water Temperature")

ggplot(annaulTemprature, aes(x = Year)) + 
  geom_line(aes(y = annualMeans_AT), colour="blue", size = 0.5) + 
  geom_line(aes(y = annualMeans_WT), colour = "red", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")