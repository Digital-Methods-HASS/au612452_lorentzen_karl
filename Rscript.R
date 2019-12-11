#First, I load tidyverse, allowing the use of more tools in RStudio.
#I will also load my two datasets, the total world population (TotalWorldPop.csv) and conflict catalogue (Wars.csv). In my case, both are saved in the data folder in my Digital Methods folder.
library(tidyverse)
TotalPop <- read_csv2("data/TotalWorldPop.csv")
Wars <- read_csv2("data/Wars.csv")
#Fixing column names
colnames(Wars)
names(Wars)[names(Wars) == "Common Name"] <- "CommonName"
#Grouping the the total population by year and calculating the mean.
TotalPop %>%
  group_by(Year) %>% 
  summarize(MeanPop=mean(`World Population over 12000 years (people)`))
#Need to assign the years to certain intervals ("binning")
century <- 100*TotalPop$Year %/%100
?cbind
TotalPopC <- cbind(TotalPop, century)
#Grouping the population stats by century
MeanPopCentury <- TotalPop %>%
  group_by(as.factor(TotalPopC$century)) %>%
  summarize(MeanPop=mean(`World Population over 12000 years (people)`))
#Binned the dates, attached the dates to original population, summarized average global population by century in groups
#Dividing the wars by century
century <- 100*Wars$EndYear %/%100
Wars <- cbind(Wars,century)
#In order to make RStudio understand the numbers in the table as actual numeric values, I have to change them to just that.
names(MeanPopCentury)[names(MeanPopCentury) == "as.factor(TotalPopC$century)"] <- "TotalPopCentury"
MeanPopCentury <- MeanPopCentury %>% 
  mutate(Century=as.numeric(as.character(TotalPopCentury)))
#Combining the two tables to compare casualty rates in wars to population in respective century
Wars <- Wars %>% 
  left_join(select(MeanPopCentury, MeanPop, Century), by = c("century" = "Century"))
#Now I will use the mutate function command to create a column showing the war-related fatalities by percentage of the total world population in the respective century. 
Wars <- Wars %>% 
  mutate(FatsByPercentage = (TotalFatalities/MeanPop)*100)
#To provide a better overview, I am choosing to show only the most relevant columns in the table.
Wars <- Wars%>% 
  select(CommonName, Name, MilFatalities, TotalFatalities, StartYear, EndYear, century, MeanPop, FatsByPercentage)
