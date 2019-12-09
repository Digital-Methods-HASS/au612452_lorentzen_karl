#First, I load tidyverse, allowing the use of more tools in RStudio.
#I will also load my two datasets, the total world population (TotalWorldPop.csv) and conflict catalogue (Wars.csv). In my case, both are saved in the data folder in my Digital Methods folder.
library(tidyverse)
TotalPop <- read_csv2("data/TotalWorldPop.csv")
Wars <- read_csv2("data/Wars.csv")
#Fixing column names
colnames(MeanPopCentury)
names(MeanPopCentury)[names(MeanPopCentury) == "as.factor(TotalPopC$century)"] <- "TotalPopCentury"
#Changing the column's name problaby needs to happen further down in the script
colnames(Wars)
names(Wars)[names(Wars) == "Common Name"] <- "CommonName"
#Grouping the the total population by year and calculating the mean.
TotalPop %>%
  group_by("Year") %>% 
  summarize(MeanPop=mean(`World Population over 12000 years (people)`))
#Need to assign the years to certain intervals ("binning")
century <- 100*TotalPop$Year %/%100
TotalPopC <- aggregate(TotalPop, data.frame(century), mean)
#ERROR: Aggregate command does not seem to work, argument is not numeric or logical
#Did not initially believe aggregate to have given correct result, using group.by gives same result, which proved correct after all
?cbind
TotalPopC <- cbind(TotalPop, century)
names(TotalPopC)
#Grouping the population stats by century
MeanPopCentury <- TotalPop %>%
  group_by(as.factor(TotalPopC$century)) %>%
  summarize(MeanPop=mean(`World Population over 12000 years (people)`))
#Binned the dates, attached the dates to original population, summarized average global population by century in groups
#Dividing the wars by century
century <- 100*Wars$EndYear %/%100
Wars <- cbind(Wars,century)
#Factor section lesson 3
MeanPopCentury <- MeanPopCentury %>% 
  mutate(Century=as.numeric(as.character(TotalPopCentury)))
#ERROR: "TotalPopCentury" not found as object
#Combining the two tables to compare casualty rates in wars to population in respective century
Wars <- Wars %>% 
  left_join(select(MeanPopCentury, MeanPop, Century), by = c("century" = "Century"))
#"Century" not found as object
#Now I will use the mutate function command to create a column showing the war-related fatalities by percentage of the total world population in the respective century. 
Wars <- Wars %>% 
  mutate(FatsByPercentage = (TotalFatalities/MeanPop.y)*100)
#ERROR: "MeanPop.y" not found as object
#To provide a better overview, I am choosing to show only the most relevant columns in the table.
Wars <- Wars%>% 
  select(CommonName, Name, MilFatalities, TotalFatalities, StartYear, EndYear, century, MeanPop.y, FatsByPercentage)
#ERROR: "MeanPop.y" not found as object
