library(shinydashboard)
library(tidyverse)

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
particle_cards_valueBox_server <- function(id, country, raw_data, get_PARTICLE_CATEGORY) {
  moduleServer(id, function(input, output, session) {
    country_data <- reactive({
      raw_data %>%
        filter(City_comb == country())
    })
    
    output$fine_card <- renderValueBox({
      fine <- country_data()$Fine_Particles
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
      coarse <- country_data()$Coarse_Particles
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
      co <- country_data()$Carbon_Monoxide
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
      o3 <- country_data()$Ozone
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
      so2 <- country_data()$Sulphur_Dioxide
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
      no2 <- country_data()$Nitrogen_Dioxide
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
