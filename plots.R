library(esquisse)
library(ggplot2)
library(dplyr)

# set the working directory
setwd("C:/Users/elisa/Documents/Data set")
# Load the data
data <- read.csv("Pedestrian_Motor_Vehicle_Accidents.csv")
# show the first 6 rows of the data
str(data)

# Create a bar chart to analyze how many accidents per year
## convert date column to date format from character
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

## create a new column for year
data$year <- format(data$Date, "%Y")


# Define function to extract hour component from POSIXct objects
extract_hour <- function(time_value) {
  time_posix <- as.POSIXct(sprintf("%04d", time_value), format = "%H%M", tz = "UTC")
  hour <- format(time_posix, format = "%H")
  return(hour)
}

# Apply function to "Time" column and create new "hour" column
data$hour <- sapply(data$Time, extract_hour)


# Define function to determine if time falls within daytime or nighttime
get_time_period <- function(time_value) {
  time_posix <- as.POSIXct(sprintf("%04d", time_value), format = "%H%M", tz = "UTC")
  hour <- as.numeric(format(time_posix, format = "%H"))
  
  if (hour >= 6 && hour < 18) {
    return("Daytime")
  } else {
    return("Nighttime")
  }
}

# Add new column to dataframe indicating daytime or nighttime
data$Time_Period <- sapply(data$Time, get_time_period)

ggplot(data) +
 aes(x = year) +
 geom_bar(fill = "gold") +
 theme_minimal() +
 labs(x = "Year", y = "Number of Accidents", title = "Accidents by Year")


library(dplyr)
library(ggplot2)

data %>%
 filter(!(year %in% "2015")) %>%
 ggplot() +
 aes(x = year, fill = year) +
 geom_bar() +
 scale_fill_manual(values = c(`2009` = "#DD3226", 
`2010` = "#BD321A", `2011` = "#872807", `2012` = "#D00C6C", `2013` = "#E07CA2", `2014` = "#750202", `2015` = "#D98272"
)) +
 labs(x = "Year", y = "Number of Accidents", title = "Pedestrian Motor Vehicle Accidents from 2009 to 2014 in Richmond VA", 
 subtitle = "Pedestrians killed or injured per year", caption = "Data provided by Richmond Police Department") +
 theme_bw() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(face = "bold.italic"), 
 plot.caption = element_text(face = "italic", hjust = 0), axis.title.y = element_text(size = 13L, 
 face = "bold"), axis.title.x = element_text(size = 13L, face = "bold"))



# Create bar chart to explore accidents by time period

ggplot(data) +
  aes(x = Time_Period, fill = Time_Period) +
  geom_bar() +
  scale_fill_manual(values = c(Daytime = "#EBD406", 
                               Nighttime = "#070105")) +
  labs(x = "Time Period", y = "Number of Accidents", title = "Pedestrian Motor 
       Vehicle Accidents from 2009 to 2015 in Richmond VA ", 
       subtitle = "Pedestrian killed or injured by time of day", caption = "Data
       provided by Richmond Police Department") +
  ggthemes::theme_economist() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 13L, face = "bold.italic"), 
        plot.caption = element_text(size = 11L, 
                                                                                                    face = "italic", hjust = 0), axis.title.y = element_text(size = 15L, face = "bold"), axis.title.x = element_text(size = 15L, 
                                                                                                                                                                                                                     face = "bold")) 
ggplot(data, aes(x = Time_Period, fill = Time_Period)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(x = "Time Period", y = "Number of Accidents", title = "Accidents by Time Period")

# scatter plots
data2 <- as.data.frame(table(data$year, data$Time_Period))

# subset data for accidents in daytime
daytime_accidents <- data2[data2$Var2 == "Daytime",]



# subset data for accidents in nighttime
nighttime_accidents <- data2[data2$Var2 == "Nighttime",]

# plot scatterplot to show daytime accidents per year
ggplot(daytime_accidents) +
  aes(x = Var1, y = Freq, size = Freq) +
  geom_point(shape = "circle", colour = "Red") +
  labs(x = "Year", y = "Number of Accidents", title = "Daytime Accidents per Year") +
  theme_minimal()



# plot scatterplot to show nighttime accidents per year
ggplot(nighttime_accidents) +
  aes(x = Var1, y = Freq, size = Freq) +
  geom_point(shape = "circle", colour = "#112446") +
  labs(x = "Year", y = "Number of Accidents", title = "Nighttime Accidents per Year") +
  theme_minimal()






