# Assignment - 1
# Question 1
# install.packages(c("tidyverse"))
# install.packages("ggplot2")


library("readxl")
library("tidyverse")

weather_data <- read_excel("C:\\MMA860_Assignment1_Data_vf.xlsx",
                           sheet = "Island Airport Weather", col_names = TRUE, skip = 16)

# Answer a

weather_data$"air density" <- (weather_data[,16]*1000)/(287.05*(weather_data[,4] + 273.15))
weather_data  %>% 
  head(5)


# Answer b

conversion_factor <- 0.277778
weather_data$"Wind Spd (m/s)" <- (weather_data[,12] * conversion_factor)
weather_data  %>% 
  head(5)

# Answer c

cut_in <- 4
cut_out <- 32
turbine_nominal_power <- 4e6
turbine_swept_area <- 13273.23
max_power_coefficient <- 0.35
weather_data$"power" <- weather_data[,"air density"] * turbine_swept_area * 0.5 * (weather_data[,"Wind Spd (m/s)"]^3) * max_power_coefficient
weather_data$"power"[weather_data[,'Wind Spd (m/s)'] < cut_in | weather_data[,'Wind Spd (m/s)'] > cut_out] <- 0
weather_data$"power"[weather_data$"power" > turbine_nominal_power] <- turbine_nominal_power
weather_data  %>% 
  head(5)

#Answer d

total_electricity_produced <- 49 * (sum(weather_data$"power") /1e6)

#Answer e

ggplot( data = weather_data  %>%
          group_by(Day) %>%
          summarise(day_power = sum(power,na.rm = TRUE)/1e6),
        mapping = aes(Day,day_power))+
  geom_line() +
  labs(title = "Per day power production: January",
       x = "Day",
       y = "Power(MW)")


