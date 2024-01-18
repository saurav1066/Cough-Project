# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com/
# #
# 
# library(readxl)
# library(plotly)
# library(dplyr)
# 
# #Loaded required files
# coughs_data <- read_excel('cough-project/1465-0008-coughdata.xlsx')
# activity_data <- read.csv('cough-project/Cough-Activity/activity_190_15ea06a7-baaa-4de2-8b67-07d32d9c88ee_activityepochs10.csv')
# 
# #Converted both files to dataframe
# coughs_data <- data.frame(coughs_data)
# activity_data <- data.frame(activity_data)
# 
# 
# #selected a random patient with many values
# cough_data_trial <- coughs_data[coughs_data$VISIT == "Visit 2" & coughs_data$USUBJID == 6004009 &coughs_data$FAOBJ=='Cough',]
# cough_data_trial
# 
# #Converted time for comparision
# cough_data_trial$FADTC <- as.POSIXct(cough_data_trial$FADTC, format = "%Y-%m-%dT%H:%M:%S")
# 
# 
# cough_data_trial$time <- format(cough_data_trial$FADTC, "%H:%M:%S")
# cough_data_trial$time
# 
# #Converted time for comparision
# activity_data$TIMESTAMPUTC <- as.POSIXct(activity_data$TIMESTAMPUTC, format = "%Y-%m-%d %H:%M:%S")
# 
# #Converting timestamp to date 
# activity_data$date <- as.Date(activity_data$TIMESTAMPUTC)
# 
# #Filtering dataframe with a single date
# activity_data <- activity_data[activity_data$date == as.Date('2022-03-30'),]
# 
# 
# activity_data$time <- format(activity_data$TIMESTAMPUTC,"%H:%M:%S" )
# activity_data$time
# 
# #Merging both dataframes with same times
# cough_activity_data <- merge(cough_data_trial,activity_data, by = 'time')
# 
# #Discarding unnecessary columns
# cough_activity_data <- cough_activity_data[c('time','USUBJID', 'FAOBJ', 'STEPS', 'MET', 'CALORIES')]
# cough_activity_data
# 
# 
# # ggplotly(ggplot(cough_activity_data, aes(x = time, y = CALORIES)) +
# # geom_point() +
# # geom_line()+
# # xlab("Time") +
# # ylab("Calories")
# # )
# 
# #Simple Plot for calories
# cough_activity_data %>%
# group_by(time) %>%
# summarise(count = n(), total_calories = mean(CALORIES)) %>%
# ggplot(aes(x = time)) +
# geom_point(aes(y = count))+
# geom_line(aes(y = count), color = "blue", group = 1) +
# geom_point(aes(y = total_calories))+
# geom_line(aes(y = total_calories), color = "red", linetype = "dashed", group = 1) +
# scale_y_continuous(sec.axis = sec_axis(~., name = "Total Calories")) +
# labs(y = "Count of Cough")
# 
# # # Creating cough Count and calorie total
# # cough_activity_data_summary <- cough_activity_data %>%
# # group_by(time) %>%
# # summarise(count_cough = n(),
# # total_calories = sum(CALORIES))
# # 
# # # Creating the plot
# # fig <- plot_ly(cough_activity_data_summary, x = ~time, y = ~count_cough, name = 'Count of cough', type = 'scatter', mode = 'lines') %>%
# # add_trace(y = ~total_calories, name = 'Total Calories', mode = 'lines', yaxis = 'y2') %>%
# # layout(title = "Count of Cough and Total Calories over Time",
# # yaxis2 = list(overlaying = "y", side = "right"))
# # 
# # fig
# 
# # #Plot with ggplotly
# # cough_activity_data_summary <- cough_activity_data %>%
# # group_by(time) %>%
# # summarise(count_cough = n(),
# # total_calories = sum(CALORIES))
# # 
# # # Creating the ggplot
# # p <- ggplot(cough_activity_data_summary, aes(x = time)) +
# # geom_line(aes(y = count_cough), color = 'blue', group= 1) +
# # geom_line(aes(y = total_calories), color = 'red', linetype = "twodash", group = 1) +
# # labs(title = "Count of Cough and Total Calories over Time",
# # y = "Count of Cough",
# # y.sec.axis = sec_axis(~ . , name = "Total Calories")) +
# # theme_minimal()
# # 
# # p
# # # Converting to plotly
# # fig <- ggplotly(p)
# # 
# # fig
# 
# # Creating a new dataframe with 'type' column for the legend
# cough_activity_data_summary <- cough_activity_data_summary %>%
#   gather(key = "type", value = "value", count_cough, total_calories)
# 
# # Creating the ggplot
# # p <- ggplot(cough_activity_data_summary, aes(x = time, y = value, color = type)) +
# #   geom_line(group=1) +
# #   scale_color_manual(values = c("blue", "red")) +
# #   labs(title = "Count of Cough and Total Calories over Time",
# #        y = "Count",
# #        color = "Type") +
# #   theme_minimal()
# # 
# # # Converting to plotly
# # fig <- ggplotly(p)
# # 
# # fig
# 
# #Simple Plot for MET
# cough_activity_data %>%
#   group_by(time) %>%
#   summarise(count = n(), ave_met = mean(MET)) %>%
#   ggplot(aes(x = time)) +
#   geom_point(aes(y = count))+
#   geom_line(aes(y = count), color = "blue", group = 1) +
#   geom_point(aes(y = avg_met))+
#   geom_line(aes(y = avg_met), color = "red", linetype = "dashed", group = 1) +
#   scale_y_continuous(sec.axis = sec_axis(~., name = "Avergae Met")) +
#   labs(y = "Count of Cough")
# 
# 
# 
# 
# 

library(shiny)
library(readxl)
library(plotly)
library(dplyr)
library(ggplot2)

#UI
ui <- fluidPage(
  titlePanel("Cough Activity Analysis"),
  sidebarLayout(
    sidebarPanel(
      
    ),
    mainPanel(
      plotOutput("coughPlot")
    )
  )
)

#Server
server <- function(input, output) {
  output$coughPlot <- renderPlot({
    
    #Reading the csv files
    coughs_data <- read_excel('cough-project/1465-0008-coughdata.xlsx')
    activity_data <- read.csv('cough-project/Cough-Activity/activity_190_15ea06a7-baaa-4de2-8b67-07d32d9c88ee_activityepochs10.csv')
    
    #Converting them to dataframes
    coughs_data <- data.frame(coughs_data)
    activity_data <- data.frame(activity_data)
    
    #Selecting a patient and visit with most data
    cough_data_trial <- coughs_data[coughs_data$VISIT == "Visit 2" & coughs_data$USUBJID == 6004009 &coughs_data$FAOBJ=='Cough',]
    
    #Applying postinc foromat to date and time
    cough_data_trial$FADTC <- as.POSIXct(cough_data_trial$FADTC, format = "%Y-%m-%dT%H:%M:%S")
    cough_data_trial$time <- format(cough_data_trial$FADTC, "%H:%M:%S")
    
    activity_data$TIMESTAMPUTC <- as.POSIXct(activity_data$TIMESTAMPUTC, format = "%Y-%m-%d %H:%M:%S")
    
    #Extracting date
    activity_data$date <- as.Date(activity_data$TIMESTAMPUTC)
    
    #Filtering for a specific date
    activity_data <- activity_data[activity_data$date == as.Date('2022-03-30'), ]
    activity_data$time <- format(activity_data$TIMESTAMPUTC,"%H:%M:%S" )
    
    
    #Merging cough and activity
    cough_activity_data <- merge(cough_data_trial,activity_data, by = 'time')
    cough_activity_data <- cough_activity_data[c('time','USUBJID', 'FAOBJ', 'STEPS', 'MET', 'CALORIES')]
    
    
    #Generating the plot
    cough_activity_data %>%
      group_by(time) %>%
      summarise(count = n(), total_calories = mean(CALORIES)) %>%
      ggplot(aes(x = time)) +
      geom_point(aes(y = count))+
      geom_line(aes(y = count), color = "blue", group = 1) +
      geom_point(aes(y = total_calories))+
      geom_line(aes(y = total_calories), color = "red", linetype = "dashed", group = 1) +
      scale_y_continuous(sec.axis = sec_axis(~., name = "Total Calories")) +
      labs(y = "Count of Cough")
  })
}

shinyApp(ui = ui, server = server)
