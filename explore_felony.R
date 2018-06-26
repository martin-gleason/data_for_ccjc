###Packages
library(tidyverse)
library(kableExtra)

def_topline <- "This data report is based on Count 1 for each case, which is generally the most serious charge in a given case, and is commonly referred to as the topline charge."

felonies_location <- "/Users/marty/Dropbox\ (Personal)/Coding Projects/DataSets/State_s_Attorney_Felony_Cases_-_Sentences_By_Offense_Type_and_Defendant_Race.csv"
felonies <- read_csv(felonies_location, col_names = TRUE)

max(felonies$YEAR)

felonies_year_offense_race <- felonies %>% group_by(YEAR, `OFFENSE TYPE`, RACE)

felonies_year_offense_race  %>% summarise(total = n())

null_race <- sum(is.na(felonies$RACE))/nrow(felonies)

ggplot(felonies_year_offense_race, aes(x = YEAR)) + 
  geom_histogram(stat = "count", aes(fill = `OFFENSE TYPE`)) + 
  facet_grid( ~ RACE)

felonies %>% group_by(`OFFENSE TYPE`, RACE) %>% 
  summarize("Total" = n()) %>% 
  filter(`OFFENSE TYPE` == "Retail Theft")
  arrange(desc(Total))

felonies %>% group_by(`OFFENSE TYPE`) %>% 
  mutate("Total" = n()) %>% 
  ungroup() %>%
  ggplot(aes(x = reorder(`OFFENSE TYPE`, -Total), y = Total)) + 
  geom_bar(stat = "identity", aes(fill = RACE)) + 
  coord_flip() +
  facet_grid( ~ YEAR)

