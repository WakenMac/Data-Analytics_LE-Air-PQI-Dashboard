library(tidyverse)
library(plotly)

#RAW DATA
data <- read_csv(file.choose())

#CURRENT DATA 2
current_data <- read_csv(file.choose())

#===================================HOURLY FORECAST DATA============================================


#1. FUNCTION TO GET HOURLY FORECAST OF 1 COUNTRY
get_HOURLYFORECAST = function(countryIndex){
  fHours <- unlist(strsplit(data$Hours[countryIndex], ","))
  fHourlyAQI <- unlist(strsplit(data$Hourly_AQI[countryIndex], ","))
  fHourlyTemp <- unlist(strsplit(data$Hourly_Temp[countryIndex], ","))
  fHourlyTemp <- gsub("°","",fHourlyTemp)
  fHourlyWind <- unlist(strsplit(data$Hourly_Wind[countryIndex], ","))
  fHourlyHumid <- unlist(strsplit(data$Hourly_Humid[countryIndex], ","))
  fHourlyHumid <- gsub("%","",fHourlyHumid)
  
  hourlyForecast <- tibble(
    Timestamp = fHours,
    AQI = fHourlyAQI,
    Temperature_Celsius = fHourlyTemp,
    Wind_KmPerHour = fHourlyWind,
    Humidity_Percentage = fHourlyHumid
  )
  
  hourlyForecast <- hourlyForecast |>
    mutate(AQI = as.numeric(AQI),
           Temperature_Celsius = as.numeric(Temperature_Celsius),
           Wind_KmPerHour = as.numeric(Wind_KmPerHour),
           Humidity_Percentage = as.numeric(Humidity_Percentage))
  
  return(hourlyForecast)
}

trialCountry <- get_HOURLYFORECAST(1)
trialCountry

#===================================DAILY FORECAST DATA============================================

#1. FUNCTION TO GET DAILY FORECAST OF 1 COUNTRY
get_DAILYFORECAST = function(countryIndex){
  fDays <- unlist(strsplit(data$Days[countryIndex], ","))
  fDailyAQI <- unlist(strsplit(data$Daily_AQI[countryIndex], ","))
  
  fDailyMaxTemp <- unlist(strsplit(data$Daily_MaxTemp[countryIndex], ","))
  fDailyMaxTemp <- gsub("°","",fDailyMaxTemp)
  fDailyMinTemp <- unlist(strsplit(data$Daily_MinTemp[countryIndex], ","))
  fDailyMinTemp <- gsub("°","",fDailyMinTemp) 
  
  fDailyWind <- unlist(strsplit(data$Daily_Wind[countryIndex], ","))
  fDailyHumid <- unlist(strsplit(data$Daily_Humid[countryIndex], ","))
  fDailyHumid <- gsub("%","",fDailyHumid)
  
  dailyForecast <- tibble(
    Timestamp = fDays,
    AQI = fDailyAQI,
    MaxTemperature_Celsius = fDailyMaxTemp,
    MinTemperature_Celsius = fDailyMinTemp,
    Wind_KmPerHour = fDailyWind,
    Humidity_Percentage = fDailyHumid
  )
  
  dailyForecast <- dailyForecast |>
    mutate(AQI = as.numeric(AQI),
           MaxTemperature_Celsius = as.numeric(MaxTemperature_Celsius),
           MinTemperature_Celsius = as.numeric(MinTemperature_Celsius),
           Wind_KmPerHour = as.numeric(Wind_KmPerHour),
           Humidity_Percentage = as.numeric(Humidity_Percentage))
  
  
  return(dailyForecast)
}

trialCountry <- get_HOURLYFORECAST(1)
trialCountry

#===================================DASHBOARD STATISTICS============================================

#SELECT COUNTRY INDEX

countryIndex <- 1
country <- data.frame(current_data[countryIndex,])

#1. AQI

country$AQI_Index
country$AQI_Category

#2. HEAT

country$Heat_Index
country$Heat_Category

#3. DEW POINT

country$DewPoint_Index
country$DewPoint_Category

#4. WIND SPEED

country$Wind_Kmph
country$Wind_Category

#5. DAILY FORECAST

country_daily <- get_DAILYFORECAST(countryIndex)
country_daily <- country_daily |>
  mutate(Timestamp = as.factor(Timestamp))

#Daily AQI
ggplotly(ggplot(data = country_daily, aes(x = fct_inorder(Timestamp), y = AQI,group = 1)) + geom_point() +
  geom_smooth(aes(colour = "red")) +
  labs (title = "Daily AQI Index",
        x = "Day",
        y = "AQI Index") +
  theme(legend.position = "none"))


#Daily Min-Max Temperature
ggplotly(ggplot(data = country_daily, aes(x = fct_inorder(Timestamp), group = 1)) + geom_point(aes(y = MinTemperature_Celsius)) +
  geom_point(aes(y = MaxTemperature_Celsius)) +
  geom_smooth(aes(y = MinTemperature_Celsius, colour = "yellow")) + 
  geom_smooth(aes(y = MaxTemperature_Celsius, colour = "orange")) + 
  labs (title = "Daily Min and Max Temperature",
        x = "Day",
        y = "Temperature (Celsius)")+
    theme(legend.position = "none"))

#Daily Wind

ggplotly(ggplot(data = country_daily, aes(x = fct_inorder(Timestamp), y = Wind_KmPerHour, group = 1)) + geom_point() +
  geom_smooth(aes(colour = "darkgreen")) + theme(legend.position = "none") +
  labs (title = "Daily Wind Speed",
        x = "Day",
        y = "Wind Speed (Km/h)") +
  scale_colour_manual(values = "darkgreen"))

#Humidity Percentage

ggplotly(ggplot(data = country_daily, aes(x = fct_inorder(Timestamp), y = Humidity_Percentage, group = 1)) + geom_point() +
  geom_smooth(aes(colour = "blue")) + theme(legend.position = "none") +
  labs (title = "Daily Humidity",
        x = "Day",
        y = "Humidity %") +
  scale_colour_manual(values = "blue"))


#6. HOURLY FORECAST

country_hourly <- get_HOURLYFORECAST(countryIndex)
country_hourly <- country_hourly |>
  mutate(Timestamp = as.factor(Timestamp)) |>
  distinct(Timestamp, .keep_all = TRUE)

#AQI Index
ggplotly(ggplot(data = country_hourly, aes(x = fct_inorder(Timestamp), y = AQI, group = 1)) + geom_point() + 
           geom_smooth(aes(colour = "red")) +   labs (title = "Hourly AQI Index",
                                   x = "Hour",
                                   y = "AQI Index") +
           theme(legend.position = "none") +
           scale_colour_manual(values = "red"))

#Temperature
ggplotly(ggplot(data = country_hourly, aes(x = fct_inorder(Timestamp), y = Temperature_Celsius, group = 1)) + geom_point() + 
           geom_smooth(aes(colour = "orange")) +   labs (title = "Hourly Temperature",
                                   x = "Hour",
                                   y = "Temperature (Celsius)") +
           theme(legend.position = "none")+
           scale_colour_manual(values = "orange"))

#Wind Speed
ggplotly(ggplot(data = country_hourly, aes(x = fct_inorder(Timestamp), y = Wind_KmPerHour, group = 1)) + geom_point() + 
           geom_smooth(aes(colour = "darkgreen")) +   labs (title = "Hourly Wind Speed",
                                                         x = "Hour",
                                                         y = "Wind Speed (Km/h)") +
           theme(legend.position = "none")+
           scale_colour_manual(values = "darkgreen"))

#Humidity
ggplotly(ggplot(data = country_hourly, aes(x = fct_inorder(Timestamp), y = Humidity_Percentage, group = 1)) + geom_point() + 
           geom_smooth(aes(colour = "blue")) +   labs (title = "Hourly Humidity",
                                                            x = "Hour",
                                                            y = "Humidity %") +
           theme(legend.position = "none")+
           scale_colour_manual(values = "blue"))


#7. PARTICLE PIE CHART
