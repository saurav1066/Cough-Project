#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

library(readxl)
library(plotly)

#Loaded required files
coughs_data <- read_excel('cough-project/1465-0008-coughdata.xlsx')
activity_data <- read.csv('cough-project/Cough-Activity/activity_190_15ea06a7-baaa-4de2-8b67-07d32d9c88ee_activityepochs10.csv')

#Converted both files to dataframe
coughs_data <- data.frame(coughs_data)
activity_data <- data.frame(activity_data)


#selected a random patient with many values
cough_data_trial <- coughs_data[coughs_data$VISIT == "Visit 2" & coughs_data$USUBJID == 6004009,]
cough_data_trial

#Converted time for comparision
cough_data_trial$FADTC <- as.POSIXct(cough_data_trial$FADTC, format = "%Y-%m-%dT%H:%M:%S")


cough_data_trial$time <- format(cough_data_trial$FADTC, "%H:%M:%S")
cough_data_trial$time

#Converted time for comparision
activity_data$TIMESTAMPUTC <- as.POSIXct(activity_data$TIMESTAMPUTC, format = "%Y-%m-%d %H:%M:%S")
activity_data$time <- format(activity_data$TIMESTAMPUTC,"%H:%M:%S" )
activity_data$time

#Merging both dataframes with same times
cough_activity_data <- merge(cough_data_trial,activity_data, by = 'time')

#Discarding unnecessary columns
cough_activity_data <- cough_activity_data[c('time','USUBJID', 'FAOBJ', 'STEPS', 'MET', 'CALORIES')]
cough_activity_data


ggplotly(ggplot(cough_activity_data, aes(x = time, y = CALORIES)) +
  geom_point() +
  geom_line()+
  xlab("Time") +
  ylab("Calories")
)