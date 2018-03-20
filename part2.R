
library(tidyverse)
library(readxl)
library(stringr)
library(munsell)
library(ggplot2)







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



#let's separate Category into Label and Quant

ex1 <- separate(veg.3,Category,into = c("Label", "Quant"), sep=",")

#let's separate Quant into Type and Name

ex2 <- separate(ex1, Quant, into=c("Type","Name"), sep = ":")

ex3 <- select(ex2, -Domain)

#Let's separate and tidy up the fertilizer's names.

ex4 <- ex3 %>% separate(Label, into = c("Label","Type1"), sep=":")

ex4$Name <- with(ex4,ifelse(is.na(Type),Type1,Name))

ex4 <- select(ex4, -Type1)



#Let's focus now on tidying up the "Data" column
#The data column gives info on the kind of measurement
#that took place for a given observation

ex5 <- ex4 %>% separate(Data, into = c("name", "Measurement"), sep = "-")

ex6 <- ex5 %>% separate(Measurement, into = c("Measurement", "Info on Measurement", "Stats"), sep = ",")

#We now can discern the observation based on the measurement and its unit

#Let's rename the column Name:
#this column is equal to commodity for some observations, but not for all, so we shouldn't drop it
#as it may add some useful information
#ex7[,"Info on Commodity"] %>% unique()

ex7 <- ex6 %>% rename("Info on Commodity"=name)

ex7.1 <- ex7 %>% mutate(Geo = replace(Geo, Geo =="REGION : MULTI-STATE", "MULTI-STATE"))

#Let's split the "Name" column into Name and ID and delete the brackets

ex8 <- ex7.1 %>% separate(Name, into = c("Name", "ID"), sep = "=") %>% 
  separate(Name, into = c("Delete", "Name"), sep = "[()]") %>%
  separate(ID, into = c("ID", "Delete1"), sep = "[)]") %>%
  select(-Delete, -Delete1)

#Finally, let's replace the values in Value such as (D),(Z) and (NA) with NA

ex9 <- ex8%>% filter(!Value %in% c("(D)", "(Z)", "(NA)", NA)) %>%
  filter(!Name %in% c("TOTAL")) %>%
  filter(Stats %in% NA) %>%
  select(-Stats)

#Let's now focus our Analysis on the observation that involved the use of Restricted Chemicals

ex11 <- ex9 %>% filter(Label == "RESTRICTED USE CHEMICAL")


#Analysis on Restricted Chemicals on Broccoli on Average

Br1 <- ex9 %>% filter(Commodity == "BROCCOLI", `Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 
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

#The graph below presents different widths accoring to the number of years where each chemical appears
#ggplot(Br1, aes( x=Name, y= Value, fill=Year))+
#  geom_bar( position = "dodge",stat="identity") + coord_flip() +  scale_fill_manual(values = c("yellow","orange", "red", "brown")) 


library(devtools)

dev_mode(on=T)

install_github("hadley/ggplot2",force = TRUE)

ggplot(Br1, aes( x=Name, y= Value, fill=Year))+
  geom_bar( position=position_dodge(preserve="single"),stat="identity") +
  coord_flip() +  scale_fill_manual(values = c("yellow","orange", "red", "brown")) 

#Sprouts do not contain restricted chemicals


#Analysis on Restricted Chemicals on Cauliflower on Average

Cal1 <- ex9 %>% filter(Commodity == "CAULIFLOWER", `Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

Cal1$Value <- as.numeric(Cal1$Value)
Cal1$Year <- as.numeric(Cal1$Year)
Cal1$Year <- as.factor(Cal1$Year)



graph <- Cal1 %>% group_by(Name) %>% summarize(mean(Value))

ggplot(graph, mapping=aes(x= Name, y=`mean(Value)` )) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals in calliflowers ")

#plot with normal version of ggplot2
#ggplot(Cal1, aes( x=Name, y= Value, fill=Year))+
#  geom_bar( position = "dodge",stat="identity") + coord_flip() +  scale_fill_manual(values = c("yellow","orange", "red", "brown")) 

#plot with development version of ggplot2
ggplot(Cal1, aes( x=Name, y= Value, fill=Year))+
  geom_bar( position=position_dodge(preserve="single"),stat="identity") +
  coord_flip() +  scale_fill_manual(values = c("yellow","orange", "red", "brown"))


#Total Average Between Broccoli and Calliflowers

BrCal <- ex9 %>% filter(Commodity == "CAULIFLOWER" || Commodity == "BROCCOLI",`Info on Measurement`==" MEASURED IN LB" , Label == "RESTRICTED USE CHEMICAL") 

BrCal$Value <- as.numeric(BrCal$Value)

grapht <- BrCal %>% group_by(Name) %>% summarize(mean(Value))


ggplot(graph, mapping=aes(x= Name, y=`mean(Value)`)) + 
  geom_bar(stat = "identity", fill="steelblue") + 
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals")


#Total Average Between Broccoli and Calliflowers DODGED GRAPH

BrCal1 <- rbind(Br1,Cal1)


grapht <- BrCal1 %>% group_by(Commodity, Name) %>% summarize(mean(Value))
ggplot(grapht, mapping=aes(x= Name, y=`mean(Value)`, fill = Commodity)) + 
  geom_bar( position=position_dodge(preserve="single"),stat="identity") +
  coord_flip() +
  labs(y = "Values (LB) ", title = "Average values of restricted chemicals")

dev_mode(FALSE)

#Let's create a table of the retricted chemicals properties

#Data collected from https://comptox.epa.gov/dashboard
# BETA-CYFLUTHRIN and ZETA-CYPERMETHRIN  
#did not have any data downloadble regarding toxicity information


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


df2 <- as.tibble(df2)
df3 <- as.tibble(df3)
df4 <- as.tibble(df4)
df5 <- as.tibble(df5)
df6 <- as.tibble(df6)
df7 <- as.tibble(df7)
df8 <- as.tibble(df8)
df10 <- as.tibble(df10)
df11 <- as.tibble(df11)
df12 <- as.tibble(df12)
df13 <- as.tibble(df13)
df14 <- as.tibble(df14)
df15 <- as.tibble(df15)

df2 <- df2 %>% mutate( Name= "BIFENTHRIN") %>% filter(SPECIES == "earthworm" | SPECIES == "rat" )
df3 <- df3 %>% mutate( Name= "ESFENVALERATE") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df4 <- df4 %>% mutate( Name= "IMIDACLOPRID") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df5 <- df5 %>% mutate( Name= "LAMBDA-CYHALOTHRIN") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df6 <- df6 %>% mutate( Name= "METHOMYL") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df7 <- df7 %>% mutate( Name= "NALED") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df8 <- df8 %>% mutate( Name= "PERMETHRIN") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df10 <- df10 %>% mutate( Name= "CHLORANTRANILIPROLE") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df11 <- df11 %>% mutate( Name= "CHLORPYRIFOS") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df12 <- df12%>% mutate( Name= "DIAZINON") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df13 <- df13 %>% mutate( Name= "PRONAMIDE") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df14 <- df14 %>% mutate( Name= "DISULFOTON") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")
df15 <- df15 %>% mutate( Name= "EMAMECTIN BENZOATE") %>% filter(SPECIES == "earthworm" | SPECIES == "rat")


tox <- rbind(df2,df3,df4,df5,df6,df7,df8,df10,df11,df12,df13,df14,df15)

tox <- tox %>% filter(TYPE=="LC50" | TYPE == "LD50")


tox1 <- tox %>% filter(UNITS  == "mg/kg") %>% 
  group_by(Name,SPECIES,UNITS) %>%
  summarize(round(mean(PRIORITY)),mean(VALUES)) %>%
  rename( "Values" = `mean(VALUES)`, 
          "Priority" = `round(mean(PRIORITY))`, 
          "Units" = UNITS)

tox1 <- ungroup(tox1)

tox1$SPECIES <- as.character(tox1$SPECIES)
tox1$Units <- as.character(tox1$Units)
tox1$Values <- as.numeric(tox1$Values)



tox1 <- spread(tox1, key= SPECIES, value = Values )


tox1 <- tox1 %>% rename("Values for LD50 on rats" = rat, "Values for LC50 on earthworms"=earthworm)

tox1 %>% filter(!is.na(`Values for LD50 on rats`)) %>% ggplot(aes(x=Name, y=`Values for LD50 on rats`)) + 
  geom_bar(stat="identity",fill="steelblue") + coord_flip() +
  labs(x="Chemical",title = "Aver. Quantity of chemicals (mg/kg) causing mortality in rats")

tox1 %>% filter(!is.na(`Values for LC50 on earthworms`)) %>% ggplot(aes(x=Name, y=`Values for LC50 on earthworms`)) + 
  geom_bar(stat="identity",fill="steelblue") + coord_flip() +
  labs(x="Chemical",title = "Aver. Quantity of chemicals causing mortality in earthworms")

ID <- c(128825, 90100, 59101, 57801, 32501,122806, 109303, 129099, 128897, 90301,34401, 109701,101701,118831,129064)


#data for Beta-Cyfluthrin were found in 
#https://cfpub.epa.gov/ecotox/advanced_query.htm

dfBeta = read.csv("Beta.csv",sep = ";")

vec <- dfBeta[,"Conc.1..Author." ]

vec <- mean(vec, na.rm=TRUE)

left1 <- c("BETA-CYFLUTHRIN", "mg/kg", NA,NA,vec)

#data for Zeta-Cypermethrin was found in
#http://pmep.cce.cornell.edu/profiles/extoxnet/carbaryl-dicrotophos/cypermet-ext.html
#Let's take the website as reliable
#The website gives different ranges of mortality for rats depending on the cis/trans-isomers present in the chemical 
#Let's calculate the mean

ran1ma <- c(187,326)
ran1fe <- c(150,500)
ran2ma <- c(357,2000)
ran2fe <- c(82,779)

vec1 <- c(mean(ran1ma), mean(ran1fe), mean(ran2ma), mean(ran2fe))

vec1 <- mean(vec1)

left2 <-c("ZETA-CYPERMETHRIN","mg/kg",NA,NA,vec1)



tox1[nrow(tox1) + 1,] <- left1
tox1[nrow(tox1) + 1,] <- left2


tox1 <- add_column(tox1, ID)

coeff <- read.csv("Assig5.csv",sep=";")
coeff <- as.tibble(coeff)

coeff <- coeff %>% rename("Soil Adsorption coeff"=Soil.Absorbed.Coeff..L.Kg.) %>%
  select(Name, `Soil Adsorption coeff`)

tox1 <- inner_join(tox1,coeff)

#Let's join tox1 with the veg table with observations related to restricted use chemicals.

restr <- ex9 %>% filter(Label == "RESTRICTED USE CHEMICAL") %>% group_by(Name)

tox1$ID <- as.numeric(tox1$ID)
restr$ID <- as.numeric(restr$ID)



tab1 <- left_join(tox1,restr, by= "ID")

tab1 <- select(tab1, - Name.y , -ID)

tab1 <- tab1 %>% rename("Name" = Name.x)

tab1 <- tab1[,c(7:14,2,15,1,16,17,3,4,5,6)]

View(tab1)


