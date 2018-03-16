library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
library(munsell)


veg.1 <- read_xlsx("veg1.xlsx")


## now get the count for each column

c <- apply(veg.1, 2, n_distinct)

e <- names(c[c>1])



veg.2 <- select(veg.1, e)



veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)



#let's separate Quant into Type and Name


ex1 <- separate(veg.3,Category,into = c("Label", "Quant"), sep=",")

utils::View(ex1)
#let's separate Quant into Type and Name
ex2 <- separate(ex1, Quant, into=c("Type","Name"), sep = ":")

ex3 <- select(ex2, -Domain)

ex4 <- ex3 %>% separate(Label, into = c("Label","Type1"), sep=":")


ex4$Name <- with(ex4,ifelse(is.na(Type),Type1,Name))

ex4 <- select(ex4, -Type1)





utils::View(ex4)

#LET'S TIDY UP A BIT THE DATA COLUMN

ex5 <- ex4 %>% separate(Data, into = c("name", "Measurement"), sep = "-")

ex6 <- ex5 %>% separate(Measurement, into = c("Measurement", "Info on Measurement", "Stats"), sep = ",")

utils::View(ex7.2)

ex7 <- ex6 %>% rename("Info on Commodity"=name)

ex7.1 <- ex7 %>% separate(Geo, into= c("Geo", "MultiState")) 

##??!??!?!?!?!?!?!

ex7.2 <- ex7.1 %>%  mutate(Geo == replace(Geo, Geo == "REGION", "MULTI-STATE")) %>%
  select(-MultiState)


utils::View(ex7.2)

#Let's split the "Name" column into Name and ID


ex8 <- ex7 %>% separate(Name, into = c("Name", "ID"), sep = "=") %>% 
  separate(Name, into = c("Delete", "Name"), sep = "[()]") %>%
  separate(ID, into = c("ID", "Delete1"), sep = "[)]") %>%
  select(-Delete, -Delete1)



ex9[,"Name"] %>% unique() %>% print(n=1000)

#DATA EXPLORATION

ex9 <- ex8%>% filter(!Value %in% c("(D)", "(Z)", "(NA)", NA)) %>%
  filter(!Name %in% c("TOTAL")) %>%
  filter(Stats %in% NA) %>%
  select(-Stats)



View(ex9)

unique(ex10[,"Label"])


View(ex11)



unique(ex9[,"Value"]) %>% print(n = 300)


unique(ex5[,"name"])

unique(ex5[,"Commodity"])

View(unique(ex4[,"Data"]))



ex11 <- ex10 %>% filter(Label == "RESTRICTED USE CHEMICAL")

#Analysis on Restricted Chemicals on Broccoli on Average

Br1 <- ex10 %>% filter(Commodity == "BROCCOLI", `Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

Br1$Value <- as.numeric(Br1$Value)
Br1$Year <- as.numeric(Br1$Year)
Br1$Year <- as.factor(Br1$Year)

graph <- Br1 %>% group_by(Name) %>% summarize(mean(Value))

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)` )) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals in broccoli ") 





#Analysis on Restricted Chemicals on Broccoli during the year 2016 and 2016

cl <- mnsl(c("5R 2/4", "5R 7/10"))

ggplot(Br1, aes( x=Name, y= Value, fill=Year))+
  geom_bar( position = "dodge",stat="identity") + coord_flip() +  scale_fill_manual(values = c("yellow","orange", "red", "brown")) 


#ggplot(Br1, aes( x=Name, y= Value, fill=Year))+
#  geom_bar( position=position_dodge(preserve="single"),stat="identity")

#This above doesn't run, to ask professor.

#https://github.com/tidyverse/ggplot2/blob/master/R/position-dodge.r
#http://ggplot2.tidyverse.org/reference/position_dodge.html#examples

View(Br1)
unique(Br1[,"Year"])







Cal1 <- ex10 %>% filter(Commodity == "CAULIFLOWER", `Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 
View(Cal1)

count(Cal1)

Cal1$Value <- as.numeric(Cal1$Value)

View(Cal1)
Cal1$Year <- as.numeric(Cal1$Year)
Cal1$Year <- as.factor(Cal1$Year)



graph <- Cal1 %>% group_by(Name) %>% summarize(mean(Value))

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)` )) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals in calliflowers ")


ggplot(Cal1, aes( x=Name, y= Value, fill=Year))+
  geom_bar( position = "dodge",stat="identity") + coord_flip() +  scale_fill_manual(values = c("yellow","orange", "red", "brown")) 

Sprouts <- ex10 %>% filter(`Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

utils::View(Sprouts)

Cal1


#Total Average Between Broccoli and Calliflowers

BrCal <- ex10 %>% filter(Commodity == "CAULIFLOWER" || Commodity == "BROCCOLI",`Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

BrCal$Value <- as.numeric(BrCal$Value)

grapht <- BrCal %>% group_by(Name) %>% summarize(mean(Value))

View(grapht)

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)`)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals")


#Total Average Between Broccoli and Calliflowers DODGED GRAPH

BrCal1 <- rbind(Br1,Cal1)

View(BrCal1)

grapht <- BrCal %>% group_by(Commodity, Name) %>% summarize(mean(Value))
ggplot(grapht, mapping=aes(x= Name, y=`mean(Value)`, fill = Commodity)) + 
  geom_bar(position="dodge",stat = "identity") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals")

# BETA-CYFLUTHRIN
# BIFENTHRIN 
# ESFENVALERATE
# IMIDACLOPRID
# LAMBDA-CYHALOTHRIN  
# METHOMYL
# NALED
# PERMETHRIN
# ZETA-CYPERMETHRIN           
# CHLORANTRANILIPROLE
# CHLORPYRIFOS
# DIAZINON
# PRONAMIDE
# DISULFOTON
# EMAMECTIN BENZOATE


df2 = read.csv("2.csv",sep = ";")
df3 = read.csv("3.csv",sep = ";")
df4 = read.csv("4.csv",sep = ";")
df5= read.csv("5.csv",sep = ";")
df6 = read.csv("6.csv",sep = ";")
df7 = read.csv("7.csv",sep = ";")
df8 = read.csv("8.csv",sep = ";")
df10 = read.csv("10.csv",sep = ";")
df11 = read.csv("11.csv",sep = ";")
df12 = read.csv("12.csv",sep = ";")
df13 = read.csv("13.csv",sep = ";")
df14 = read.csv("14.csv",sep = ";")
df15 = read.csv("15.csv",sep = ";")


as.tibble(df2)
as.tibble(df3)
as.tibble(df4)
as.tibble(df5)
as.tibble(df6)
as.tibble(df7)
as.tibble(df8)
as.tibble(df10)
as.tibble(df11)
as.tibble(df12)
as.tibble(df13)
as.tibble(df14)
as.tibble(df15)


df2 <- df2 %>% mutate( Name= "BIFENTHRIN") %>% filter(SPECIES == "rat")
df3 <- df3 %>% mutate( Name= "ESFENVALERATE") %>% filter(SPECIES == "rat")
df4 <- df4 %>% mutate( Name= "IMIDACLOPRID") %>% filter(SPECIES == "rat")
df5 <- df5 %>% mutate( Name= "LAMBDA-CYHALOTHRIN") %>% filter(SPECIES == "rat")
df6 <- df6 %>% mutate( Name= "METHOMYL") %>% filter(SPECIES == "rat")
df7 <- df7 %>% mutate( Name= "NALED") %>% filter(SPECIES == "rat")
df8 <- df8 %>% mutate( Name= "PERMETHRIN") %>% filter(SPECIES == "rat")
df10 <- df10 %>% mutate( Name= "CHLORANTRANILIPROLE") %>% filter(SPECIES == "rat")
df11 <- df11 %>% mutate( Name= "CHLORPYRIFOS") %>% filter(SPECIES == "rat")
df12 <- df12%>% mutate( Name= "DIAZINON") %>% filter(SPECIES == "rat")
df13 <- df13 %>% mutate( Name= "PRONAMIDE") %>% filter(SPECIES == "rat")
df14 <- df14 %>% mutate( Name= "DISULFOTON") %>% filter(SPECIES == "rat")
df15 <- df15 %>% mutate( Name= "EMAMECTIN BENZOATE") %>% filter(SPECIES == "rat")


tox <- rbind(df2,df3,df4,df5,df6,df7,df8,df10,df11,df12,df13,df14,df15)

tox1 <- tox %>% filter(UNITS  == "mg/kg") %>% 
  group_by(Name,RISK_ASSESSMENT,UNITS) %>%
  summarize(round(mean(PRIORITY)),mean(VALUES)) %>%
  rename("Risk Assessment on rats" = RISK_ASSESSMENT,
         "Values for LD50 on rats" = `mean(VALUES)`, 
         "Priority" = `round(mean(PRIORITY))`, 
         "Units" = UNITS)

View(tox1)


ggplot(tox1, aes(x=Name, y=`Values for LD50 on rats`)) + 
  geom_bar(stat="identity",fill="steelblue") + coord_flip() +
  labs(x="Chemical",title = "Aver. Quantity of chemicals (mg/kg) causing mortality in rats")

restr <- ex10 %>% filter(Label == "RESTRICTED USE CHEMICAL")
#???
View(full_join(restr, tox1,by="Name"))

help("inner_join")
