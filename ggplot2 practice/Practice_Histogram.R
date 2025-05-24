
library(tidyverse)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

data <- read_csv("Current_World_AQI_Ranking2.csv") %>%
  mutate(City = str_extract(Country, "^[^,]+"),
         Country = str_extract(Country, "(?<=, )[^,]+$")) %>%
  relocate(City, .before = Country)

histData = ne_countries(scale = 50, returnclass = "sf") %>%
  select(name, continent) %>%
  right_join(data, by = c("name" = "Country")) %>%
  mutate(
    continent = case_when(
      is.na(continent) & name == "USA" ~ "North America",
      is.na(continent) & name %in% c("Bosnia Herzegovina", "Czech Republic") ~ "Europe",
      is.na(continent) & str_detect(name, "Hong Kong") ~ "Asia",
      is.na(continent) & name %in% c("Democratic Republic of the Congo") ~ "Africa",
      TRUE ~ continent # Else condition
    )
  ) %>%
  arrange(asc(continent), asc(name))
  rename(Country = name,
         Continent = continent) %>%
  relocate(c(Country, Continent), .after = City)

print(histData %>%
        select(Country, Continent = NA))
  
View(histData)

createHistogram("AQI");

createHistogram <- function(type = ""){
   if (type == "AQI"){
     p <- ggplot(histData, aes(text = paste(
       "<strong>Country: </strong> " = Country,
       "<br>Air Quality Index: ", AQI_Index,
       "<br>Class: ", AQI_Category
     ))) +
       geom_histogram(aes(AQI_Index,
                          fill = Continent),
                          binwidth = 25) +
       labs(
         title = "Air Quality Index (AQI) Among Continents",
         x = "AQI Distribution",
         y = "Frequency"
        )
     
     return(ggplotly(p, tooltip = "text"))
   }
   
}
