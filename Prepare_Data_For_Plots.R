
# Instructions for running the things,
# Initialize the Data by adding all functions to the envi

library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

# Getting the data from the CSV
data <- read_csv("Current_World_AQI_Ranking2.csv") %>%
  mutate(City = str_extract(Country, "^[^,]+"),
         Country = str_extract(Country, "(?<=, )[^,]+$")) %>%
  relocate(City, .before = Country)

# ==============================================================================
# Preparing the data for the World Map

# Template world map data (MUST HAVE).
template <- ne_countries(scale = 50, returnclass = "sf")

# Prepares the data for the AQI
summarized_data <- data %>%
  group_by(Country) %>%
  summarize(Average_AQI = mean(AQI_Index),
            Average_Dew = mean(DewPoint_Index),
            Average_Heat = mean(Heat_Index)) %>%
  mutate(Average_AQI = as.integer(Average_AQI))



# Prepares the data to be fed to the ggplot and geom_sf()
world_data <- left_join(template, summarized_data, by = c("name" = "Country"))

# ==============================================================================

# Prepares the data for the histograms
histData = template %>%
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
  arrange(continent, name) %>%
  rename(Country = name,
       Continent = continent) %>%
  relocate(c(Country, Continent), .after = City)

# ==============================================================================
# Prepares plots

# Prepares the heat maps
get_AQI_heat_map = function(){
  p <- ggplot(data = world_data) +
    geom_sf(aes(fill = Average_AQI,), 
            color = "black", 
            size = 0.1) +
    scale_fill_viridis_c(option = "H", name = "Average AQI", na.value = "white") +
    labs(
      title = "Global Air Quality Index Distribution by Country",
      subtitle = "Aggregated average AQI from selected cities"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgray", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(ggplotly(p, tooltip = "text"))
}

get_Heat_Heat_Map = function(){
  ggplot(data = world_data) +
    geom_sf(aes(
      fill = Average_Heat,
    ), color = "black", size = 0.1) +
    scale_fill_viridis_c(option = "H", name = "Average Heat", na.value = "white") +
    labs(
      title = "Global Heat Index by Country",
      subtitle = "Aggregated average Heat among cities"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgray", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

get_Dew_Point_Heat_Map = function(){
  ggplot(data = world_data) +
    geom_sf(aes(
      fill = Average_Dew,
    ), color = "black", size = 0.1) +
    scale_fill_viridis_c(option = "H", name = "Average Dew Index", na.value = "white") +
    labs(
      title = "Global Dew Point Index by Country",
      subtitle = "Aggregated average Dew Point among cities"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "lightgray", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# Prepares the Histograms
get_AQI_Histogram = function(){
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

get_Heat_Histogram = function(){
  p <- ggplot(histData, aes(text = paste(
    "<strong>Country: </strong> " = Country,
    "<br>Heat Index: ", Heat_Index,
    "<br>Class: ", Heat_Category
  ))) +
    geom_histogram(aes(Heat_Index,
                       fill = Continent),
                   binwidth = 5) +
    labs(
      title = "Air Quality Index (AQI) Among Continents",
      x = "AQI Distribution",
      y = "Frequency"
    )
  
  return(ggplotly(p, tooltip = "text"))
}

get_Dew_Point_Histogram = function(){
  p <- ggplot(histData, aes(text = paste(
    "<strong>Country: </strong> " = Country,
    "<br>Dew Point Index: ", DewPoint_Index,
    "<br>Class: ", DewPoint_Category
  ))) +
    geom_histogram(aes(DewPoint_Index,
                       fill = Continent),
                   binwidth = 5) +
    labs(
      title = "Air Quality Index (AQI) Among Continents",
      x = "AQI Distribution",
      y = "Frequency"
    )
  
  return(ggplotly(p, tooltip = "text"))
}

# ==============================================================================
# Helper Methods

get_AQI_CATEGORY = function(AQI_Index){
  if (AQI_Index < 51){category = "Good"}
  else if (AQI_Index < 101){category = "Moderate"}
  else if (AQI_Index < 151) {category = "Unhealthy for Sensitive Groups" }
  else if (AQI_Index < 201) {category = "Unhealthy"}
  else if (AQI_Index < 301){category = "Very Unhealthy"}
  else {category = "Hazardous"}
  
  return(category)
  
}

get_HEAT_CATEGORY = function(heat){
  if (heat <= 26){category = "Safe"}
  else if (heat <= 32){category = "Caution"}
  else if (heat <= 40) {category = "Extreme Caution" }
  else if (heat <= 51) {category = "Danger"}
  else {category = "Extreme Danger"}
  
  return(category)
}

get_WIND_CATEGORY = function(wind){
  if (wind <= 2){category = "Calm"}
  else if (wind <= 5){category = "Light Air"}
  else if (wind <= 11) {category = "Light Breeze"}
  else if (wind <= 19) {category = "Gentle Breeze"}
  else if (wind <= 28) {category = "Moderate Breeze"}
  else if (wind <= 38) {category = "Fresh Breeze"}
  else if (wind <= 49) {category = "Strong Breeze"}
  else if (wind <= 61) {category = "Near Gale"}
  else if (wind <= 74) {category = "Fresh Gale"}
  else if (wind <= 88) {category = "Strong Gale"}
  else if (wind <= 102) {category = "Storm"}
  else if (wind <= 117) {category = "Violent Storm"}
  else {category = "Hurricane Force"}
  
  return(category)
}

get_DEW_CATEGORY = function(dew){
  if (dew <= 4){category = "Very Dry"}
  else if (dew <= 10){category = "Dry"}
  else if (dew <= 15) {category = "Comfortable" }
  else if (dew <= 20) {category = "Somewhat Muggy"}
  else if (dew <= 24) {category = "Muggy"} 
  else {category = "Oppresive"}
  
  return(category)
}





