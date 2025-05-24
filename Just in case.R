library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(maps)
library(viridis)
library(ggthemes)
library(shinyBS)
library(tidyverse)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
source("ParticleCards_valueBox.r")

# Custom CSS for styling
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@400;600&display=swap');
body, .content-wrapper, .right-side, .box, .navbar, .tab-content, .value-box, .box-title, .box-header, .form-control, .selectize-input, .selectize-dropdown, .shiny-input-container {
  font-family: 'Poppins', 'Roboto', 'Lato', Arial, sans-serif !important;
}
.navbar {
  background-color: white !important;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
.navbar-default .navbar-nav > li > a {
  color: black !important;
  font-weight: 500;
  font-size: 16px;
  padding: 15px 20px;
}
.navbar-default .navbar-nav > li > a:hover {
  color: #666 !important;
}
.navbar-brand img {
  filter: brightness(0) invert(1);
  background-color: white;
}
.box {
  border-radius: 10px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.07);
  margin-bottom: 24px;
  background-color: #f9f9fb;
}
.box-header {
  border-bottom: 1px solid #f0f0f0;
  padding: 15px;
}
.box-title {
  font-weight: 600;
  font-size: 16px;
}
.value-box {
  border-radius: 10px;
  margin-bottom: 32px !important;
}
.tab-content {
  padding: 24px;
}
body, .content-wrapper, .right-side {
  background-color: #f4f6fa !important;
}
.dashboard-body {
  background-color: #f7f9fb !important;
}
.section-row {
  margin-bottom: 32px;
}
.section-title {
  font-size: 1.5em;
  font-weight: 600;
  margin-bottom: 16px;
}
.aqi-predictor-result {
  background: #fff;
  border-radius: 16px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.07);
  padding: 40px 24px;
  text-align: center;
  margin-top: 0;
  min-height: 200px;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}
.aqi-predictor-result h4 {
  font-size: 1.3em;
  font-weight: 600;
  margin-bottom: 12px;
}
.aqi-predictor-result .aqi-value {
  font-size: 4.5em;
  font-weight: 800;
  margin-bottom: 12px;
}
.aqi-predictor-result .aqi-class {
  font-size: 2.2em;
  font-weight: 700;
  margin-top: 12px;
}
"

# ==============================================================================
# Preparing the Data

# DATA (Part 1): Prepares the data from the dataset
data <- read_csv("Current_World_AQI_Ranking2.csv") %>%
  mutate(City = str_extract(Country, "^[^,]+"),
         Country = str_extract(Country, "(?<=, )[^,]+$")) %>%
  relocate(City, .before = Country)

# DATA (Part 2): Template world map data (MUST HAVE).
template <- ne_countries(scale = 50, returnclass = "sf")

# DATA (Part 3): Aggregates template and wrangled data
summarized_data <- data %>%
  group_by(Country) %>%
  summarize(Average_AQI = mean(AQI_Index),
            Average_Dew = mean(DewPoint_Index),
            Average_Heat = mean(Heat_Index)) %>%
  mutate(Average_AQI = as.integer(Average_AQI))

# DATA (Part 4): Joins data for the Heat Map
world_data <- left_join(template, summarized_data, by = c("name" = "Country")) %>%
  rename(Country = name)

# DATA (Part 5): Prepares data for the Histograms
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
# Preparing the Dashboard

ui <- tagList(
  tags$head(
    tags$title("AirLoom Dashboard")
  ),
  dashboardPage(
    dashboardHeader(
      title = tags$div(
        tags$img(src = "airloom_logo.png", height = "60px")
      ),
      tags$li(class = "dropdown", tags$a(HTML(paste0(icon("globe"), " Global")), href = "#shiny-tab-global")),
      tags$li(class = "dropdown", tags$a(HTML(paste0(icon("flag"), " Countries")), href = "#shiny-tab-countries")),
      tags$li(class = "dropdown", tags$a(HTML(paste0(icon("info-circle"), " About AirLoom")), href = "#shiny-tab-about"))
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tags$head(
        tags$style(HTML(custom_css))
      ),
      tabsetPanel(
        id = "tabs",
        tabPanel(tagList(icon("globe"), "Global"), value = "global",
                 fluidRow(class = "section-row",
                          column(4, valueBoxOutput("num_countries", width = NULL)),
                          column(4, valueBoxOutput("avg_aqi", width = NULL)),
                          column(4, valueBoxOutput("overall_particles", width = NULL))
                 ),
                 fluidRow(class = "section-row",
                          column(6, box(title = "Top 10 Countries", width = NULL, 
                                        tableOutput("top_countries"), 
                                        style = "height: 400px; overflow-y: auto;")),
                          column(6, box(title = "Global Heatmap", width = NULL, 
                                        plotOutput("global_heatmap", height = "400px")))
                 ),
                 fluidRow(class = "section-row",
                          column(4, box(title = "AQI Distribution", width = NULL, 
                                        plotOutput("aqi_hist", height = "300px"))),
                          column(4, box(title = "Temperature Distribution", width = NULL, 
                                        plotOutput("heat_hist", height = "300px"))),
                          column(4, box(title = "Dew Point Distribution", width = NULL, 
                                        plotOutput("dew_hist", height = "300px")))
                 ),
                 fluidRow(class = "section-row",
                          column(12, box(title = HTML("AQI Calculator<br><small>Input particle values.</small>"), width = NULL,
                                         fluidRow(
                                           column(6,
                                                  sliderInput("pred_pm25", "PM2.5 (µg/m³)", min = 0, max = 500, value = 10, step = 0.1),
                                                  sliderInput("pred_pm10", "PM10 (µg/m³)", min = 0, max = 600, value = 20, step = 0.1),
                                                  sliderInput("pred_co", "CO (ppm)", min = 0, max = 50, value = 1, step = 0.01)
                                           ),
                                           column(6,
                                                  sliderInput("pred_o3", "O3 (ppm)", min = 0, max = 0.5, value = 0.05, step = 0.001),
                                                  sliderInput("pred_so2", "SO2 (ppb)", min = 0, max = 1000, value = 10, step = 1),
                                                  sliderInput("pred_no2", "NO2 (ppb)", min = 0, max = 2000, value = 20, step = 1)
                                           )
                                         ),
                                         div(style = "text-align:center; margin-top: 24px;",
                                             actionButton("predict_aqi_btn", "Predict AQI", icon = icon("calculator"), style = "margin-bottom:24px; font-size:1.2em; padding:10px 32px;"),
                                             uiOutput("predicted_aqi_result")
                                         )
                          ))
                 )
        ),
        tabPanel(tagList(icon("flag"), "Countries"), value = "countries",
                 fluidRow(
                   column(12, box(selectInput("country", "Select Country", 
                                              choices = NULL, 
                                              width = "100%"), 
                                  width = NULL))
                 ),
                 
                 fluidRow(
                   column(3, valueBoxOutput("country_aqi", width = NULL)),
                   column(3, valueBoxOutput("country_heat", width = NULL)),
                   column(3, valueBoxOutput("country_wind", width = NULL)),
                   column(3, valueBoxOutput("country_dew", width = NULL))
                 ),
                 fluidRow(
                   column(6, box(
                     selectInput("country_hourly_var", "Select Variable (Hourly)", 
                                 choices = c("AQI", "Temperature", "Wind", "Humidity"), 
                                 selected = "AQI", width = "100%"),
                     plotOutput("country_hourly_line", height = "300px"),
                     width = NULL
                   )),
                   column(6, box(
                     selectInput("country_daily_var", "Select Variable (Daily)", 
                                 choices = c("AQI", "Temperature", "Wind", "Humidity"), 
                                 selected = "Temperature", width = "100%"),
                     plotOutput("country_daily_line", height = "300px"),
                     width = NULL
                   ))
                 ),
                 fluidRow(
                   column(12, box(title = "Particle Composition (Details)", width = NULL, collapsible = TRUE, collapsed = FALSE,
                                  particle_cards_valueBox_ui("particle_cards")))
                 )
        ),
        tabPanel(tagList(icon("info-circle"), "About AirLoom"), value = "about",
                 fluidRow(
                   column(12, box(title = "Dataset Details", width = NULL, 
                                  uiOutput("dataset_details")))
                 ),
                 fluidRow(
                   column(12, box(title = "Dashboard Purpose", width = NULL, 
                                  uiOutput("dashboard_purpose")))
                 ),
                 fluidRow(
                   column(12, box(title = "Definition of Terms", width = NULL, 
                                  uiOutput("definition_terms")))
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Generate mock data
  set.seed(123)
  n_countries <- 20
  countries <- c("USA", "China", "India", "UK", "Germany", "France", "Japan", "Brazil", 
                 "Canada", "Australia", "Mexico", "Italy", "Spain", "Russia", "South Korea",
                 "Netherlands", "Switzerland", "Sweden", "Norway", "Denmark")
  
  # Create mock dataset (7 days x 24 hours per country)
  n_days <- 7
  mock_data <- data.frame(
    country = rep(countries, each = n_days * 24),
    day = rep(rep(1:n_days, each = 24), times = n_countries),
    hour = rep(1:24, times = n_countries * n_days),
    aqi = rnorm(n_countries * n_days * 24, mean = 50, sd = 15),
    temperature = rnorm(n_countries * n_days * 24, mean = 20, sd = 5),
    dew_point = rnorm(n_countries * n_days * 24, mean = 10, sd = 3),
    wind_speed = rnorm(n_countries * n_days * 24, mean = 15, sd = 5),
    humidity = rnorm(n_countries * n_days * 24, mean = 60, sd = 10),
    pm2_5 = rnorm(n_countries * n_days * 24, mean = 25, sd = 8),
    pm10 = rnorm(n_countries * n_days * 24, mean = 40, sd = 12),
    no2 = rnorm(n_countries * n_days * 24, mean = 30, sd = 10),
    co = rnorm(n_countries * n_days * 24, mean = 5, sd = 2),
    o3 = rnorm(n_countries * n_days * 24, mean = 0.06, sd = 0.02),
    so2 = rnorm(n_countries * n_days * 24, mean = 40, sd = 15)
  )
  
  # Value boxes for global tab
  output$num_countries <- renderValueBox({
    valueBox(
      n_countries,
      "Total Countries",
      icon = icon("globe"),
      color = "blue"
    )
  })
  
  get_aqi_color <- function(aqi) {
    if (aqi < 51) {
      return("green")      # Good
    } else if (aqi <= 101) {
      return("yellow")     # Moderate
    } else if (aqi <= 151) {
      return("orange")     # Unhealthy for Sensitive Groups
    } else if (aqi <= 201) {
      return("red")        # Unhealthy
    } else if (aqi <= 301) {
      return("purple")     # Very Unhealthy
    } else {
      return("black")      # Hazardous
    }
  }
  
  get_aqi_classification <- function(aqi) {
    if (aqi < 51) {
      return("Good")
    } else if (aqi <= 101) {
      return("Moderate")
    } else if (aqi <= 151) {
      return("Unhealthy for Sensitive Groups")
    } else if (aqi <= 201) {
      return("Unhealthy")
    } else if (aqi <= 301) {
      return("Very Unhealthy")
    } else {
      return("Hazardous")
    }
  }
  
  output$avg_aqi <- renderValueBox({
    avg_aqi <- round(mean(mock_data$aqi), 1)
    valueBox(
      avg_aqi,
      paste0("Average AQI (", get_aqi_classification(avg_aqi), ")"),
      icon = icon("wind"),
      color = get_aqi_color(avg_aqi)
    )
  })
  
  output$overall_particles <- renderValueBox({
    valueBox(
      round(mean(mock_data$pm2_5 + mock_data$pm10), 1),
      "Average Particles",
      icon = icon("cloud"),
      color = "purple"
    )
  })
  
  # Top 10 countries table
  output$top_countries <- renderTable({
    mock_data %>%
      group_by(country) %>%
      summarize(avg_aqi = mean(aqi)) %>%
      arrange(desc(avg_aqi)) %>%
      head(10) %>%
      mutate(avg_aqi = round(avg_aqi, 1))
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Global heatmap
  output$global_heatmap <- renderPlot({
    ggplot(data = world_data) +
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
  })
  
  # Histograms
  output$aqi_hist <- renderPlot({
    ggplot(mock_data, aes(x = aqi)) +
      geom_histogram(fill = "#2c3e50", bins = 30, alpha = 0.8) +
      theme_minimal() +
      labs(x = "AQI", y = "Count") +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  })
  
  output$heat_hist <- renderPlot({
    ggplot(mock_data, aes(x = temperature)) +
      geom_histogram(fill = "#e74c3c", bins = 30, alpha = 0.8) +
      theme_minimal() +
      labs(x = "Temperature (°C)", y = "Count") +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  })
  
  output$dew_hist <- renderPlot({
    ggplot(mock_data, aes(x = dew_point)) +
      geom_histogram(fill = "#3498db", bins = 30, alpha = 0.8) +
      theme_minimal() +
      labs(x = "Dew Point (°C)", y = "Count") +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  })
  
  # Server logic for AQI Predictor
  observeEvent(input$predict_aqi_btn, {
    pred_aqi <- 0.5 * input$pred_pm25 + 0.25 * input$pred_pm10 + 5 * input$pred_co + 100 * input$pred_o3 + 0.1 * input$pred_so2 + 0.1 * input$pred_no2
    pred_aqi <- round(pred_aqi, 1)
    pred_class <- get_aqi_classification(pred_aqi)
    aqi_color <- switch(pred_class,
                        "Good" = "#00a65a",
                        "Moderate" = "#f39c12",
                        "Unhealthy for Sensitive Groups" = "#f39c12",
                        "Unhealthy" = "#dd4b39",
                        "Very Unhealthy" = "#605ca8",
                        "Hazardous" = "#222",
                        "#00a65a"
    )
    output$predicted_aqi_result <- renderUI({
      div(class = "aqi-predictor-result", style = paste0("background:", aqi_color, "; color:#fff;"),
          h4("Predicted AQI"),
          span(class = "aqi-value", pred_aqi),
          span(class = "aqi-class", pred_class)
      )
    })
  })
  
  # Initialize output so it doesn't show NULL
  output$predicted_aqi_result <- renderUI({ NULL })
  
  # Update country choices in the select input
  updateSelectInput(session, "country",
                    choices = sort(unique(mock_data$country)),
                    selected = "USA")
  
  # Country-specific value boxes
  output$country_aqi <- renderValueBox({
    req(input$country)
    country_avg_aqi <- round(mean(mock_data$aqi[mock_data$country == input$country]), 1)
    valueBox(
      country_avg_aqi,
      paste0("Air Quality (", get_aqi_classification(country_avg_aqi), ")"),
      icon = icon("wind"),
      color = get_aqi_color(country_avg_aqi)
    )
  })
  
  # Classification for Heat Index (Temperature)
  get_heat_classification <- function(temp) {
    if (temp < 26.7) {
      return("Caution")
    } else if (temp < 32.2) {
      return("Extreme Caution")
    } else if (temp < 39.4) {
      return("Danger")
    } else {
      return("Extreme Danger")
    }
  }
  
  # Classification for Wind Speed (Beaufort Scale)
  get_wind_classification <- function(wind) {
    if (wind < 1) {
      return("Calm")
    } else if (wind < 6) {
      return("Light air")
    } else if (wind < 12) {
      return("Light breeze")
    } else if (wind < 20) {
      return("Gentle breeze")
    } else if (wind < 29) {
      return("Moderate breeze")
    } else if (wind < 39) {
      return("Fresh breeze")
    } else if (wind < 50) {
      return("Strong breeze")
    } else if (wind < 62) {
      return("High wind, moderate gale, near gale")
    } else if (wind < 75) {
      return("Gale, fresh gale")
    } else if (wind < 89) {
      return("Strong/severe gale")
    } else if (wind < 103) {
      return("Storm, whole gale")
    } else if (wind < 118) {
      return("Violent storm")
    } else {
      return("Hurricane force")
    }
  }
  
  # Classification for Dew Point
  get_dew_classification <- function(dew) {
    if (dew > 24) {
      return("Oppressive, uncomfortable for most, possible heat stress issues")
    } else if (dew > 20) {
      return("Muggy, quite uncomfortable")
    } else if (dew > 15) {
      return("Starting to feel muggy, though still comfortable for most")
    } else if (dew > 10) {
      return("Comfortable")
    } else if (dew > 5) {
      return("Dry")
    } else {
      return("Very dry")
    }
  }
  
  output$country_heat <- renderValueBox({
    req(input$country)
    avg_temp <- round(mean(mock_data$temperature[mock_data$country == input$country]), 1)
    valueBox(
      paste0(avg_temp, ' °C'),
      paste0("Heat Index (", get_heat_classification(avg_temp), ")"),
      icon = icon("temperature-high"),
      color = "red"
    )
  })
  
  output$country_wind <- renderValueBox({
    req(input$country)
    avg_wind <- round(mean(mock_data$wind_speed[mock_data$country == input$country]), 1)
    valueBox(
      avg_wind,
      paste0("Wind Speed (", get_wind_classification(avg_wind), ")"),
      icon = icon("wind"),
      color = "aqua"
    )
  })
  
  output$country_dew <- renderValueBox({
    req(input$country)
    avg_dew <- round(mean(mock_data$dew_point[mock_data$country == input$country]), 1)
    valueBox(
      avg_dew,
      paste0("Dew Point (", get_dew_classification(avg_dew), ")"),
      icon = icon("tint"),
      color = "purple"
    )
  })
  
  
  # Helper for particle classification (from Country-Analytics.r)
  get_PARTICLE_CATEGORY = function(particle,value){
    switch(
      particle,
      "Fine Particles" = {
        if (value == 0){Category <- "N/A"}
        else if (value <= 9.0){Category <- "Good"}
        else if (value <= 35.4){Category <- "Moderate"}
        else if (value <= 55.4){Category <- "Unhealthy for Sensitive Groups"}
        else if (value <= 125.4) {Category <- "Unhealthy"}
        else if (value <= 225.4) {Category <- "Very Unhealthy"}
        else {Category <- "Hazardous"}
      },
      "Coarse Particles" = {
        if (value == 0){Category <- "N/A"}
        else if (value <= 54){Category <- "Good"}
        else if (value <= 154){Category <- "Moderate"}
        else if (value <= 254){Category <- "Unhealthy for Sensitive Groups"}
        else if (value <= 354) {Category <- "Unhealthy"}
        else if (value <= 424) {Category <- "Very Unhealthy"}
        else {Category <- "Hazardous"}
      },
      "Carbon Monoxide" = {
        if (value == 0){Category <- "N/A"}
        else if (value <= 4.4){Category <- "Good"}
        else if (value <= 9.4){Category <- "Moderate"}
        else if (value <= 12.4){Category <- "Unhealthy for Sensitive Groups"}
        else if (value <= 15.4) {Category <- "Unhealthy"}
        else if (value <= 30.4) {Category <- "Very Unhealthy"}
        else {Category <- "Hazardous"}
      },
      "Ozone" = {
        if (value == 0){Category <- "N/A"}
        else if (value <= 0.054){Category <- "Good"}
        else if (value <= 0.070){Category <- "Moderate"}
        else if (value <= 0.085){Category <- "Unhealthy for Sensitive Groups"}
        else if (value <= 0.105) {Category <- "Unhealthy"}
        else if (value <= 0.200) {Category <- "Very Unhealthy"}
        else {Category <- "Hazardous"}
      },
      "Sulphur Dioxide" = {
        if (value == 0){Category <- "N/A"}
        else if (value <= 35){Category <- "Good"}
        else if (value <= 75){Category <- "Moderate"}
        else if (value <= 185){Category <- "Unhealthy for Sensitive Groups"}
        else if (value <= 304) {Category <- "Unhealthy"}
        else if (value <= 604) {Category <- "Very Unhealthy"}
        else {Category <- "Hazardous"}
      },
      "Nitrogen Dioxide" = {
        if (value == 0){Category <- "N/A"}
        else if (value <= 53){Category <- "Good"}
        else if (value <= 100){Category <- "Moderate"}
        else if (value <= 360){Category <- "Unhealthy for Sensitive Groups"}
        else if (value <= 649) {Category <- "Unhealthy"}
        else if (value <= 1249) {Category <- "Very Unhealthy"}
        else {Category <- "Hazardous"} 
      }
    )
    return(Category)
  }
  
  output$country_hourly_line <- renderPlot({
    req(input$country)
    country_data <- mock_data[mock_data$country == input$country,]
    var <- switch(input$country_hourly_var,
                  "AQI" = "aqi",
                  "Temperature" = "temperature",
                  "Wind" = "wind_speed",
                  "Humidity" = "humidity")
    ylab <- switch(input$country_hourly_var,
                   "AQI" = "AQI",
                   "Temperature" = "Temperature (°C)",
                   "Wind" = "Wind Speed (km/h)",
                   "Humidity" = "Humidity (%)")
    color <- switch(input$country_hourly_var,
                    "AQI" = "#2c3e50",
                    "Temperature" = "#e74c3c",
                    "Wind" = "#3498db",
                    "Humidity" = "#27ae60")
    # Average by hour across all days
    hourly_avg <- country_data %>%
      group_by(hour) %>%
      summarize(val = mean(.data[[var]], na.rm = TRUE))
    ggplot(hourly_avg, aes(x = hour, y = val)) +
      geom_line(color = color, size = 1) +
      geom_point(color = color, alpha = 0.6) +
      theme_minimal() +
      labs(x = "Hour", y = ylab) +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  })
  
  output$country_daily_line <- renderPlot({
    req(input$country)
    country_data <- mock_data[mock_data$country == input$country,]
    var <- switch(input$country_daily_var,
                  "AQI" = "aqi",
                  "Temperature" = "temperature",
                  "Wind" = "wind_speed",
                  "Humidity" = "humidity")
    ylab <- switch(input$country_daily_var,
                   "AQI" = "AQI",
                   "Temperature" = "Temperature (°C)",
                   "Wind" = "Wind Speed (km/h)",
                   "Humidity" = "Humidity (%)")
    color <- switch(input$country_daily_var,
                    "AQI" = "#2c3e50",
                    "Temperature" = "#e74c3c",
                    "Wind" = "#3498db",
                    "Humidity" = "#27ae60")
    # Average by day
    daily_avg <- country_data %>%
      group_by(day) %>%
      summarize(val = mean(.data[[var]], na.rm = TRUE))
    ggplot(daily_avg, aes(x = day, y = val)) +
      geom_line(color = color, size = 1) +
      geom_point(color = color, alpha = 0.6) +
      theme_minimal() +
      labs(x = "Day", y = ylab) +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
  })
  
  # About tab text outputs
  output$dataset_details <- renderText({
    HTML('All data found within the dashboard was scraped from: <a href = https://www.iqair.com/>https://www.iqair.com/</a>.
    <br>
    
    <br>Original dataset is a live ranking of all Countries around the world with a 
    huge emphasis on their Air Quality Index (AQI). In this dashboard, the results 
    of Global and Country Dataset were calculated and visualized using the Tidyverse, 
    and Plotly package as of MAY 23, 2025 - 9:15 PM. Moreover, Each Country had their 
    respective Hourly/Daily Weather Forecasts, and Pollutants scraped for a deeper analysis 
    about Air Quality. 
    <br>
    
    <br> As for Heat Index and Dew Point Index, both of these variables are feature engineered from the countries 
    weather forecasts to assess its behavior alongside with AQI.
    <br>
    
    <br> Lastly, the AQI Computation, and categorizing of Fine Particles, Coarse Particles, 
    Carbon Monoxide, Ozone, Sulphur Dioxide, as well as Nitrogen Dioxide will be based on the 
    Technical Assistance Document for the Reporting of Daily Air Quality, the Air Quality Index 
    (AQI) created by the United States - Environmental Protection Agency.
    ')
  })
  
  output$dashboard_purpose <- renderText({
    HTML('
    This dashboard provides an interactive visualization of air quality data, 
    allowing users to explore global trends and country-specific patterns. 
    It helps in understanding the relationship between various environmental 
    factors and air quality indices.
    <br>
    
    <br> The primary goal is to graphically show the Air Quality Index (AQI) 
    all around the world as per a given time period. With this in mind, 
    displaying all statistical data may proven to be useful in generating 
    AQI Reports that will then be used by several Air Quality Organizations to 
    assess which countries should be warned about hazardous pollutants floating in the 
    air. Not only that, Weather Forecast Reports can also be gathered from this 
    dashboard that can also provide useful insights as to what possible connections 
    does Air Quality have in regards with the recorded climate.
    ')
  })
  
  output$definition_terms <- renderUI({
    HTML('
      <b>AQI (Air Quality Index):</b> A measure that directly indicates air quality, determining how clean or polluted the air is, together with its associated respiratory health effects. It is calculated through 5 major air pollutants: ground-level ozone, particle pollution, carbon monoxide, sulfur dioxide, and nitrogen dioxide.<br><br>
      
      <b>AQI Classifications:</b>
      <ul>
        <li><b>Good (0-50):</b> Air quality is considered satisfactory; air pollution poses little or no risk.</li>
        <li><b>Moderate (51-100):</b> Air quality is acceptable; may be a moderate health concern for a very small number of people who are unusually sensitive to air pollution.</li>
        <li><b>Unhealthy for Sensitive Groups (101-150):</b> Members of sensitive groups may experience health effects; the general public is not likely to be affected.</li>
        <li><b>Unhealthy (151-200):</b> Everyone may begin to experience health effects; members of sensitive groups may experience more serious health effects.</li>
        <li><b>Very Unhealthy (201-300):</b> Health alert: everyone may experience more serious health effects.</li>
        <li><b>Hazardous (301+):</b> Health warnings of emergency conditions; the entire population is more likely to be affected.</li>
      </ul>
      
      <b>Heat Index:</b> A measure that estimates heat stress on the human body, calculated from ambient temperature and relative humidity.<br>
      <b>Heat Index Classifications:</b>
      <ul>
        <li><b>Caution:</b> Fatigue possible with prolonged exposure and/or physical activity.</li>
        <li><b>Extreme Caution:</b> Heat stroke, heat cramps, or heat exhaustion possible with prolonged exposure and/or physical activity.</li>
        <li><b>Danger:</b> Heat cramps or heat exhaustion likely, and heat stroke possible with prolonged exposure and/or physical activity.</li>
        <li><b>Extreme Danger:</b> Heat stroke highly likely.</li>
      </ul>
      
      <b>Dew Point Index:</b> The temperature at which water vapor will condense into liquid water, forming dew, fog, or clouds. It has a direct relationship with moisture, where the higher or lower the dew point, the more moist or dry the air is, respectively, directly affecting the comfort people feel outside. Moreover, it is a key factor in determining heat index.<br><br>
      
      <b>Fine Particles (PM2.5):</b>
      <ul>
        <li>Fine Particles are particles that are 2.5 microns or less in diameter, also known as PM2.5. They are inhalable particles that derive from combustion emissions, landfills, and fires.</li>
        <li>PM2.5 can travel into deeper parts of the lungs and can lead to adverse respiratory health effects such as acute and chronic bronchitis, asthma attacks, and other respiratory symptoms.</li>
      </ul>
      
      
      <b>Coarse Particles (PM10):</b>
      <ul>
        <li>Coarse Particles are particles that are generally 10 microns in diameter, larger than PM2.5, and can travel and stick on the surface level of the lung.</li>
        <li>They are a less concerning type of particle but can lead to lung, nose, and throat irritation.</li>
      </ul>
      
      
      <b>Ozone (O<sub>3</sub>):</b>
      <ul>
        <li>Ozone is a molecule made up of 3 oxygen atoms and comprises the Ozone layer in our Stratosphere.></li>
        <li>At ground level, it is a pollutant that contributes to respiratory problems by aggressively attacking lung tissue, and contributes to smog or haze formation.</li>
      </ul>
      
      
      <b>Other Key Terms:</b>
      <ul>
        <li><b>NO<sub>2</sub>:</b> Nitrogen dioxide concentration</li>
        <li><b>Dew Point:</b> Temperature at which air becomes saturated with water vapor</li>
      </ul>
    ')
  })
  
  particle_cards_valueBox_server(
    "particle_cards",
    reactive(input$country),
    mock_data,
    get_PARTICLE_CATEGORY
  )
}

shinyApp(ui, server) 


library(shinydashboard)

# UI for valueBox cards
particle_cards_valueBox_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4, valueBoxOutput(ns("fine_card"), width = NULL)),
    column(4, valueBoxOutput(ns("coarse_card"), width = NULL)),
    column(4, valueBoxOutput(ns("co_card"), width = NULL)),
    column(4, valueBoxOutput(ns("o3_card"), width = NULL)),
    column(4, valueBoxOutput(ns("so2_card"), width = NULL)),
    column(4, valueBoxOutput(ns("no2_card"), width = NULL))
  )
}

# Server for valueBox cards
particle_cards_valueBox_server <- function(id, country, mock_data, get_PARTICLE_CATEGORY) {
  moduleServer(id, function(input, output, session) {
    country_data <- reactive({
      mock_data[mock_data$country == country(),]
    })
    
    output$fine_card <- renderValueBox({
      fine <- mean(country_data()$pm2_5)
      fine_cat <- get_PARTICLE_CATEGORY("Fine Particles", fine)
      valueBox(
        value = paste0(round(fine, 1), " µg/m³"),
        subtitle = HTML(paste0(
          "Fine (", fine_cat, ")<br>",
          "<small>Inhalable particles ≤2.5µm; can reach deep into lungs.</small>"
        )),
        icon = icon("circle"),
        color = "teal"
      )
    })
    output$coarse_card <- renderValueBox({
      coarse <- mean(country_data()$pm10)
      coarse_cat <- get_PARTICLE_CATEGORY("Coarse Particles", coarse)
      valueBox(
        value = paste0(round(coarse, 1), " µg/m³"),
        subtitle = HTML(paste0(
          "Coarse (", coarse_cat, ")<br>",
          "<small>Particles 2.5–10µm; can irritate nose/throat.</small>"
        )),
        icon = icon("dot-circle"),
        color = "olive"
      )
    })
    output$co_card <- renderValueBox({
      co <- mean(country_data()$co)
      co_cat <- get_PARTICLE_CATEGORY("Carbon Monoxide", co)
      valueBox(
        value = paste0(round(co, 2), " ppm"),
        subtitle = HTML(paste0(
          "CO (", co_cat, ")<br>",
          "<small>Colorless, odorless gas; harmful at high levels.</small>"
        )),
        icon = icon("cloud"),
        color = "aqua"
      )
    })
    output$o3_card <- renderValueBox({
      o3 <- mean(country_data()$o3)
      o3_cat <- get_PARTICLE_CATEGORY("Ozone", o3)
      valueBox(
        value = paste0(round(o3, 3), " ppm"),
        subtitle = HTML(paste0(
          "O3 (", o3_cat, ")<br>",
          "<small>Ground-level ozone; can cause respiratory issues.</small>"
        )),
        icon = icon("cloud-sun"),
        color = "blue"
      )
    })
    output$so2_card <- renderValueBox({
      so2 <- mean(country_data()$so2)
      so2_cat <- get_PARTICLE_CATEGORY("Sulphur Dioxide", so2)
      valueBox(
        value = paste0(round(so2, 1), " ppb"),
        subtitle = HTML(paste0(
          "SO2 (", so2_cat, ")<br>",
          "<small>Gas from burning fossil fuels; can irritate airways.</small>"
        )),
        icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    })
    output$no2_card <- renderValueBox({
      no2 <- mean(country_data()$no2)
      no2_cat <- get_PARTICLE_CATEGORY("Nitrogen Dioxide", no2)
      valueBox(
        value = paste0(round(no2, 1), " ppb"),
        subtitle = HTML(paste0(
          "NO2 (", no2_cat, ")<br>",
          "<small>Gas from vehicles/industry; can worsen lung disease.</small>"
        )),
        icon = icon("radiation"),
        color = "purple"
      )
    })
  })
} 


