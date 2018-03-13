library(tidyverse)
library(stringr)

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1988:2017)

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("mr", years, sep = "")


N <- length(urls)

for (i in 1:25){
  suppressMessages(
    assign(filenames[i], read_table(urls[i], col_names = TRUE)))
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  if(i>=1 && i<=11){
    file$YYYY<-paste("19", file$YYYY, sep = "")
  }
  file <- subset(file, select=c("YYYY", "MM","DD","hh","ATMP","WTMP"))
  if(i == 1){
    MR <- file
  }
  else{
    MR <- rbind.data.frame(MR, file)
  }
}  

for (i in 27:N){
  suppressMessages(
    assign(filenames[i], read_table(urls[i], col_names = TRUE)))
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  file <- subset(file, select=c("YYYY", "MM","DD","hh","ATMP","WTMP"))
  MR <- rbind.data.frame(MR, file)
}  

MR_DailyNoon <- subset(MR, hh==12)

MR$FullTime <-strptime(with(MR, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
MR$FullTime

library("reshape2")
library("ggplot2")




ggplot(MR_DailyNoon, aes(x = FullTime)) + 
  geom_point(aes(y = ATMP), colour="blue", size = 0.5) + 
  geom_point(aes(y = WTMP), colour = "grey", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")


# ggplot(MR_DailyNoon, aes(x = FullTime)) + 
#   geom_point(aes(y = ATMP), colour="blue", size = 0.5) + 
#   geom_point(aes(y = WTMP), colour = "grey", size = 0.5) +
#   labs(title = "Temperatures\n", x = "Time", y = "Temperature[°C]", colour = "Legend Title\n") +
#   scale_color_manual(labels = c("Air", "Water"), values = c("blue", "gray")) 





