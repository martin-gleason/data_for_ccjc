# Arrest means 1) was a  crime reported. If true, an arrest was made. if false, no arrest was made

library(tidyverse)
library(kableExtra)
library(lubridate)
library(RSocrata)
cpd_location <- "/Users/marty/Dropbox\ (Personal)/Coding Projects/DataSets/Crimes_-_2001_to_present.csv"

crimes2001_present <- read.csv(cpd_location, stringsAsFactors = FALSE)
crimes2001_present$Date <- mdy_hm(crimes2001_present$Date)


year(crimes2001_present$Date)
earliest <- min(crimes2001_present$Date)
latest <- max(crimes2001_present$Date)

arrests_made <- crimes2001_present %>% filter(Arrest == TRUE)


nrow(crimes2001_present) - nrow(arrests_made)

nrow(arrests_made)/nrow(crimes2001_present)

crimes2001_present %>% group_by(Date, Arrest) %>%
  summarise(Total = n()) %>% 
  ggplot(aes(x = Date, y = Total, col = Arrest)) + 
  geom_line(stat = "identity") + 
  scale_x_datetime(date_labels = "%Y")
  

crimes2001_present %>% group_by(Date, Arrest) %>%
  summarise(Total = n()) %>% 
  ggplot(aes(x = Date, y = Total, col = Arrest)) + 
  geom_line(stat = "identity") + 
  scale_x_datetime(date_labels = "%Y", date_minor_breaks = "1 year", limits = c(earliest, latest))

crimes2001_present %>% group_by(year(Date), Arrest, District) %>% 
  summarize("Total Arrest Per District" = n())
                                                                      
