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
    MR_DailyNoon <- subset(file, hh==12)
    annualMeans_AT <- annualMean_AT
    annualMeans_WT <- annualMean_WT
  }
  else{
    MR <- rbind.data.frame(MR, file)
    MR_DailyNoon <- rbind.data.frame(MR_DailyNoon, subset(file, hh==12))
    annualMeans_AT <- rbind.data.frame(annualMeans_AT, annualMean_AT)
    annualMeans_WT <- rbind.data.frame(annualMeans_WT, annualMean_WT)
  }
  #print(MR)
  i<-i+1
}
  

#combine all dates and times into one column for creating time series graph
MR$FullTime <-strptime(with(MR, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
MR$FullTime
MR_DailyNoon$FullTime <-strptime(with(MR_DailyNoon, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
MR_DailyNoon$FullTime

# #only select daily noon data 
# MR_DailyNoon <- subset(MR, hh==12)

library("reshape2")
library("ggplot2")

ggplot(MR_DailyNoon, aes(x = FullTime)) + 
  geom_line(aes(y = ATMP), colour="blue", size = 0.5) + 
  geom_line(aes(y = WTMP), colour = "grey", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")

MR_1988_DailyNoon <- subset(MR_DailyNoon, YYYY==1988)[,c(2:6)]  
MR_2017_DailyNoon <- subset(MR_DailyNoon, YYYY==2017)[,c(2:6)]
colnames(MR_2017_DailyNoon) <- c("MM","DD","hh","ATMP2017","WTMP2017")
colnames(MR_1988_DailyNoon) <- c("MM","DD","hh","ATMP1988","WTMP1988")

#combine 1988 and 2017 into one table
MR_1988VS2017_DailyNoon <- as.data.frame(left_join(MR_1988,MR_2017))
typeof(MR_1988VS2017_DailyNoon)
#visualize the data
ggplot(MR_1988VS2017_DailyNoon, aes(x = FullTime)) + 
  geom_line(aes(y = ATMP, colour ="Air Temperature")) + 
  geom_line(aes(y = WTMP, colour = "Water Temperature")) +
  scale_colour_manual("", 
                      breaks = c("Air Temperature", "Water Temperature"),
                      values = c("pink", "blue"))+
  xlab("Time") +
  scale_y_continuous("Temperatura (C)", limits = c(0,10)) + 
  labs(title="A time series composed of 30 years of daily Air Temperature
       and Sea Temperature readings recorded at noon")

# outliersIndex <- function(data) {
#   q1 <- quantile(data)[2]
#   q3 <- quantile(data)[4]
#   iqr <- IQR(data)
#   
#   threshold.upper = (iqr * 3) + q3
#   threshold.lower = q1 - (iqr * 3)
#   index <- which(data > threshold.upper | data < threshold.lower) #retrun positions that are TRUE in the vector
#   return(index) 
# }

#create annual temprature table, which containing means of temperatures in each year
annualTemprature <- data.frame(c(1988:2017), annualMeans_AT, annualMeans_WT)
colnames(annualTemprature) <- c("Year", "Air Temperature", "Water Temperature")


ggplot(annualTemprature, aes(x = Year)) + 
  geom_line(aes(y = annualMeans_AT), colour="blue", size = 0.5) + 
  geom_line(aes(y = annualMeans_WT), colour = "red", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")