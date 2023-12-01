#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(readxl)
library(shiny)
library(gt)
library(dplyr)
library(plotly)
library(gridExtra)

#reading the given excel file
cough_data <- read_excel("../1465-0008-coughdata.xlsx")

#converting excel file to data frame
cough_data <- data.frame(cough_data)

#checking unique subject ids
subject_id <- unique(cough_data$USUBJID)
subject_id

# #checking unique sessions
# session_id <- unique(cough_data$SessionID)
# session_id

#checking unique visits
visit_id <- unique(cough_data$VISIT)
visit_id

#UI
ui <- fluidPage(
  
  titlePanel("Cough Data Application"),
  tabsetPanel(
    tabPanel("Visualization",
             sidebarLayout(
               sidebarPanel(
                 h3("Select Options"),
                 
                 #Selecting required visit and subject for observation as input
                 selectInput(inputId = "SubjectID", 
                             label = "Select Subject ID", 
                             choices = subject_id),
                 
                 # selectInput("Session", "Select Session ID",choices = session_id),
                 
                 # Condition Statement for popping out another input 
                 selectInput(inputId = "Condition",
                             label = "Select Choice",
                             choices = c("Single Insight", "Comparision")),
                 
                 conditionalPanel(
                   condition = "input.Condition == 'Single Insight'",
                   selectInput(inputId = "Visit",
                               label = "Select Visit Number", 
                               choices = visit_id)
                 )
               ),
               
               mainPanel(
                 h3("Generated Plot"),
                 
                 # selectInput(inputId = "Visit", 
                 #             label = "Select Visit Number", 
                 #             choices = visit_id),
                 
                 #generating plot as output
                 plotOutput(outputId = "cough_plot")
                 plotlyOutput(outputId = "coughly_plot")
                 
                 
                 # #generating a table output
                 # tableOutput("cough_table")
                 
               )
               
             )
             
    ),
    tabPanel("Tables",
             mainPanel(
               h3("Table for the cough counts per hour"),
               gt_output("gt_table")
             )
    ),
    
    tabPanel("Help",
             mainPanel(
               h3("Introduction to the application"),
               textOutput('help')
             )
    )
  )
  
)

#Server
server <- function(input, output, session) {
  
  
  observe({
    if(input$Condition == "Comparision"){
      selected_data <- reactive({
        cough_data[cough_data$USUBJID == input$SubjectID,]
      })
      
      output$cough_plot <- renderPlot({
        new_df <- selected_data()
        
        # Converting the 'fadtc' column to POSIXct format
        new_df$FADTC <- as.POSIXct(new_df$FADTC, format = "%Y-%m-%dT%H:%M:%S")
        
        # Creating a new column 'hour' to extract the hour from the timestamp
        new_df$hour <- format(new_df$FADTC, "%H")
        
        #grouping the dataframe based on visits
        grouped_df <- split(new_df,new_df$VISIT)
        
        cough_counts <- data.frame()
        plot_list <- list()
        
        #getting hourly counts of each group of visits
        for (i in 1:length(grouped_df)) {
          
          #Getting sleep, wake and start time of recording values
          sleep_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Sleep"])
          wake_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Wake"])
          
          
          start_time<- 0 # needs initialization because some subjects do not have a record
          
          start_time <-  as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Actual Recording Start Time"])
          
          #Getting time of cough 
          time_cough <- new_df$FADTC[new_df$FAOBJ == "Cough"]

          #Extract hours,minutes seconds
          hours <- as.numeric(format(time_cough,"%H")) 
          minutess <- as.numeric(format(time_cough,"%M"))
          Seconds <- as.numeric(format(time_cough,"%S")) 

          #Create a dataframe
          df <- data.frame(hours,minutes,seconds)

          #Plotting the time
          output$coughly_plot <- renderPlotly(
            ggplotly(
              ggplot(
                df, aes(x= hours, y = minutes, color = seconds)
              )+
              geom_point()+
              scale_color_gradient(low = "blue", high = "red") +
              labs(x = "Hours", y = "Minutes", color = "Seconds")+
              theme_minimal
            )
          )
          
          
          # Counting  the occurrences of "cough" for each hour of the day
          hourly_count <- table(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ =="Cough"])
          hourly_count<- data.frame(hourly_count)
          
          #Converting the hour data to integer
          hourly_count$Var1 <- as.integer(as.character(hourly_count$Var1))
          
          #Filling Missing Values
          hourly_count <- merge(hourly_count,
                                data.frame("Var1" = 0:23), 
                                by = 'Var1', 
                                all.y = TRUE)
          
          hourly_count$Freq[is.na(hourly_count$Freq)] <- 0
          
          #Creating a Sleep pointer as a column in dataframe
          hourly_count$sleep <- ifelse((sleep_time > wake_time & 
                                           (hourly_count$Var1 >= sleep_time | 
                                              hourly_count$Var1 <= wake_time)) 
                                        | 
                                          (sleep_time <= wake_time 
                                           & (hourly_count$Var1 >= sleep_time 
                                              & hourly_count$Var1 <= wake_time))
                                        , TRUE, FALSE)
          
          #Simple rearranging according to the start of recording time
          hourly_count <- hourly_count[order((hourly_count$Var1 - start_time) 
                                               %% nrow(hourly_count)),]
          
          row.names(hourly_count) <- 1:nrow(hourly_count)
          
          
          plot <- ggplot(hourly_count, 
                          aes(x = factor(Var1, levels = unique(Var1)),
                              y = Freq, color = sleep, group=1)) +
                     geom_line() +
                     geom_point() +
                     scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
                     labs(x = "Hours", y = "Frequency of Cough") +
                     ggtitle(paste("Cough Plot for", unique(grouped_df[[i]]$VISIT))) +
                     theme_minimal()
          
          
          plot_list[[i]]<- plot
          
          hourly_count$Visit <- grouped_df[[i]]$VISIT[1]
          
          cough_counts <- rbind(cough_counts, hourly_count)
        }
        
        #Grouping the cough counts df for better visualization 
        grouped_cough_counts <- cough_counts %>%
          group_by(Visit)
        
        
        #Outputting the table
        output$gt_table <- render_gt({
          
          gt_table <- gt(grouped_cough_counts) 
          
          gt_table|>tab_header("Cough Counts for all Visits of a Subject per hour")
          
          gt_table%>%
            cols_label(Var1 = "Time(hour)",
                       Freq = "Number of Cough")
        })
        # 
        # #plotting all the visits in same graph
        # ggplotly(ggplot(cough_counts, 
        #        aes(x = Var1, y = Freq, group = Visit, color = Visit)) +
        #   geom_line() +
        #   geom_point()+
        #   labs(x = "Hours", y = "Frequency of Cough") +
        #   theme_minimal()
        # )
        
        #Plotting individual graphs
        
        gridExtra :: grid.arrange(grobs = plot_list, ncol = 1)
        
      })
      
      
    } else{
      
      # Selecting required data as per input
      selected_data <- reactive({
        cough_data[cough_data$VISIT == input$Visit & 
                     cough_data$USUBJID == input$SubjectID,]
      })
      
      
      # #Trying dynamic Visit according to the Subject ID
      # selected_data <- reactive({
      #   cough_data[cough_data$STUDYID == input$SubjectID,]
      # })
      # 
      # #Update the input for choices of visit
      # observeEvent(selected_data()$STUDYID,{
      #   choice <- unique(selected_data()$VISIT)
      #   updateSelectInput(inputId = "Visit", choices = choice)
      # })
      # 
      # #Applying filter on visit
      # selected_data<- reactive({
      #   req(input$Visit)
      #   filter(selected_data(), VISIT == input$Visit)
      # })
      # 
      
      
      # Creating the plot
      output$cough_plot <- renderPlot({
        new_df <- selected_data()
        
        if (nrow(new_df) > 0) {
          # Converting the 'fadtc' column to POSIXct format
          new_df$FADTC <- as.POSIXct(new_df$FADTC, format = "%Y-%m-%dT%H:%M:%S")
          
          # Creating a new column 'hour' to extract the hour from the timestamp
          new_df$hour <- format(new_df$FADTC, "%H")
          
          #Getting sleep, wake and start time of recording values
          sleep_time <- as.integer(new_df$hour[new_df$FAOBJ == "Sleep"])
          wake_time <- as.integer(new_df$hour[new_df$FAOBJ == "Wake"])
          
          
          start_time<- 0 # needs initialization because some subjects do not have a record
          
          start_time <-  as.integer(new_df$hour[new_df$FAOBJ == "Actual Recording Start Time"])
          
          # Counting the occurrences of "cough" for each hour of the day
          hourly_counts <- table(new_df$hour[new_df$FAOBJ == "Cough"])
          hourly_counts <- data.frame(hourly_counts)
          
          #Converting the hour data to integer
          hourly_counts$Var1 <- as.integer(as.character(hourly_counts$Var1))
          
          #Filling Missing Values
          hourly_counts <- merge(hourly_counts,
                                 data.frame("Var1" = 0:23), 
                                 by = 'Var1', 
                                 all.y = TRUE)
          
          hourly_counts$Freq[is.na(hourly_counts$Freq)] <- 0
          
          #Creating a Sleep pointer as a column in dataframe
          hourly_counts$sleep <- ifelse((sleep_time > wake_time & 
                                           (hourly_counts$Var1 >= sleep_time | 
                                              hourly_counts$Var1 <= wake_time)) 
                                        | 
                                          (sleep_time <= wake_time 
                                           & (hourly_counts$Var1 >= sleep_time 
                                              & hourly_counts$Var1 <= wake_time))
                                        , TRUE, FALSE)
          
          #Simple rearranging according to the start of recording time
          hourly_counts <- hourly_counts[order((hourly_counts$Var1 - start_time) 
                                               %% nrow(hourly_counts)),]
          
          row.names(hourly_counts) <- 1:nrow(hourly_counts)
          
          
          #Plotting the counts of cough with respect to the hours
          
          #PLOT WITHOUT SLEEP INDICATOR
          # ggplot(data = NULL, aes(x = hourly_counts$Var1, y = hourly_counts$Freq)) +
          #   geom_point(color = "red") + geom_line(color = "blue") +
          #   labs(x = "Hour", y = "Count", title = "Count of 'cough' by Hour") +
          #   ylim(0, 40)
          
          #Generating and output the table 
          output$gt_table <- render_gt({
            gt_table <- gt(hourly_counts) 
            
            gt_table|>tab_header("Cough count per hour starting from Recording start period and sleep status")
            
            gt_table%>%
              cols_label(Var1 = "Time(hour)",
                         Freq = "Number of Cough")
          })
          
          
          # Plot with sleep indicators
          ggplot(hourly_counts, 
                 aes(x = factor(Var1, levels = unique(Var1)),
                     y = Freq, color = sleep, group=1)) +
            geom_line() +
            geom_point() +
            scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
            labs(x = "Hours", y = "Frequency of Cough") +
            theme_minimal()
          
        } 
        
        else {
         ggplot() +
            labs(x = "Hour", y = "Count", title = "No data found for the given Visit ID and USUBJID")
          
          }
      })
      
    }
  })
  
  
  # # Table of hourly counts as table
  # 
  # output$cough_table <-renderTable((table(hourly_counts) )) 
}


shinyApp(ui, server)



