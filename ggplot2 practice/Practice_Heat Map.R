library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)


# Importing the Data
data <- read_csv("Current_World_AQI_Ranking2.csv")
View(data)
summary(data)

# Gets the data per country
aqi_data <- data %>%
  select(Country, AQI_Index) %>%
  mutate(Country = str_extract(Country, "(?<=, )[^,]+$"))  %>%
  group_by(Country) %>%
  summarize(Average_AQI = mean(AQI_Index)) %>%
  mutate(Average_AQI = as.integer(Average_AQI))
View(aqi_data)

# Prepares the world map.
world <- ne_countries(scale = 50, returnclass = "sf")
head(world$name)

# Joins the data of the world map and the aqi per country
world_aqi <- left_join(world, aqi_data, by = c("name" = "Country"))
View(world_aqi)

# Generates the country heat map
# Options have specific color maps from A to H
