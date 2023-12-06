library(ggplot2)
library(plotly)
library(readxl)
library(lubridate)
library(dplyr)


#reading the given excel file
cough_data <- read_excel("cough-project/1465-0008-coughdata.xlsx")

#converting excel file to data frame
cough_data <- data.frame(cough_data)

#test dataset for some session for some subject 
# Select the columns with sessionid = 22 and usubjid = 1002001 into a new data frame
new_df <- cough_data[cough_data$VISIT == "Visit 3" & cough_data$USUBJID == 1002001,]


# Keeping the same headers
colnames(new_df) <- colnames(cough_data)


# Converting the 'fadtc' column to POSIXct format
new_df$FADTC <- as.POSIXct(new_df$FADTC, format = "%Y-%m-%dT%H:%M:%S")

#Extracting the time of coughs
new_df$time <- format(new_df$FADTC, "%H:%M:%S")
new_df$time

cough_time <- new_df$time[new_df$FAOBJ == "Cough"]
cough_time


# Convert to datetime
data <- hms(cough_time)
data

#converting to dataframe

data<- data.frame(time = data, index = c(1:length(data)))

# Convert time to POSIXct
data$time <- as.POSIXct(strptime(data$time, format = "%HH %MM %SS"), origin = "1970-01-01")

# Calculate time differences and group
data <- data %>%
  mutate(diff = c(0, as.numeric(difftime(time[-1], time[-length(time)], units = "secs")))) %>%
  mutate(group = ifelse(diff <= 6 & diff > 1, TRUE, FALSE)) %>%
  mutate(group = ifelse(lead(group, default = group[n()]), TRUE, group))



data

# Plot
ggplotly(ggplot(data, aes(x = time, y = index, color = as.factor(group))) +
           geom_point() +
           scale_x_datetime(labels = scales::time_format("%H:%M:%S")) +
           labs(x = "Time (Hours:Minutes:Seconds)", y = "Index", color = "Group") +
           theme_minimal()
)

