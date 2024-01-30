library(shiny)
library(readxl)
library(haven)
library(ggplot2) hashtag#Function that does all required preprocessing
preprocessing <- function() { hashtag#reading the given excel file adn the sas file
  cough_data <- read_excel("../1465-0008-coughdata.xlsx")
  extension_data <- read_sas("../adsl.sas7bdat") hashtag#converting excel file to data frame
  cough_data <- data.frame(cough_data)
  extension_data <- data.frame(extension_data) hashtag#Subsetting for the columns in consideration
  extension_data <- extension_data[c('SUBJID','AGE', 'SEX', 'HRCT', 'SMOKSTAT')] hashtag#checking unique subject ids
  subject_id <- unique(cough_data$USUBJID) hashtag#checking unique visits
  visit_id <- unique(cough_data$VISIT)
  
  # Converting the 'fadtc' column to POSIXct format
  cough_data$FADTC <- as.POSIXct(cough_data$FADTC, format = "%Y-%m-%dT%H:%M:%S")
  
  # Creating a new column 'hour' to extract the hour from the timestamp
  cough_data$hour <- format(cough_data$FADTC, "%H")
  
  
  # Return the processed data and unique IDs
  list(cough_data = cough_data, extension_data = extension_data, subject_id = subject_id, visit_id = visit_id)
} hashtag#Calling the function above for access of variabless
result <- preprocessing() hashtag#UI ui <- fluidPage(
titlePanel('Cough Application'),

sidebarLayout(
  sidebarPanel(
    # Condition Statement for popping out another input
    selectInput(inputId = "Condition",
                label = "Select Choice",
                choices = c("Single Insight", "Comparision", "Baseline")), hashtag#Condition Single Insight
    conditionalPanel(
      condition = "input.Condition == 'Single Insight'", hashtag#Selecting required visit and subject for observation as input
      selectInput(inputId = "SubjectID",
                  label = "Select Subject ID",
                  choices = result$subject_id),
      
      selectInput(inputId = "Visit",
                  label = "Select Visit Number",
                  choices = result$visit_id)
    ), hashtag#Condition Comparision
    conditionalPanel(
      condition = "input.Condition == 'Comparision'", hashtag#Selecting required visit and subject for observation as input
      selectInput(inputId = "SubjectID",
                  label = "Select Subject ID",
                  choices = result$subject_id),
      
      selectInput(inputId = "Visit",
                  label = "Select Visit Number",
                  choices = result$visit_id)
    ), hashtag#Condition Baseline
    conditionalPanel(
      condition = "input.Condition == 'Baseline'", hashtag#Selecting baseline visit
      selectInput(inputId = "Visit",
                  label = "Select Visit Number",
                  choices = result$visit_id)
    )
  ),
  
  mainPanel( hashtag#Condition Single Insight
             conditionalPanel(
               condition = "input.Condition == 'Single Insight'",
               plotOutput(outputId = "cough_plot")
             ), hashtag#Condition Comparision
             conditionalPanel(
               condition = "input.Condition == 'Comparision'",
               plotOutput(outputId = "comparision_plot")
             ), hashtag#Condition Baseline
             conditionalPanel(
               condition = "input.Condition == 'Baseline'",
               
               uiOutput(outputId = 'baseline_plot')
               
               
             )
  )
)
)



server <- function(input, output, session) {
  observe({ hashtag#Condition BAseline
    if(input$Condition == 'Baseline'){ hashtag#Selecting required data from the input
      selected_data <- reactive({
        result$cough_data[result$cough_data$VISIT == input$Visit,]
      })
      
      
      new_df <- selected_data()
      
      
      # Generate UI elements dynamically
      id_list <- lapply(subject_id, as.character)
      
      output$baseline_plot <- renderUI({
        lapply(id_list, function(id) {
          plotOutput(id)
        })
      })
      
      
      # Render plots for each ID
      lapply(id_list, function(subject) {
        
        output[[subject]] <- renderPlot({
          df <- new_df[new_df$USUBJID == as.numeric(subject),] hashtag#Getting sleep, wake and start time of recording values
          sleep_time <- as.integer(df$hour[df$FAOBJ == "Sleep"])
          wake_time <- as.integer(df$hour[df$FAOBJ == "Wake"])
          
          
          start_time<- 0 # needs initialization because some subjects do not have a record
          
          start_time <- as.integer(df$hour[df$FAOBJ == "Actual Recording Start Time"])
          
          
          # Counting the occurrences of "cough" for each hour of the day
          hourly_counts <- table(df$hour[df$FAOBJ == "Cough"])
          hourly_counts <- data.frame(hourly_counts) hashtag#Converting the hour data to integer
          hourly_counts$Var1 <- as.integer(as.character(hourly_counts$Var1))
          print(nrow(hourly_counts))
          
          if (nrow(hourly_counts) == 0){
            ggplot() +
              ggtitle(paste("Cough Plot for" ,subject) )+
              labs(x = "Hour", y = "Count", title = "No data found for the given Visit ID and USUBJID")
          }
          else { hashtag#Filling Missing Values
            hourly_counts <- merge(hourly_counts,
                                   data.frame("Var1" = 0:23),
                                   by = 'Var1',
                                   all.y = TRUE)
            
            hourly_counts$Freq[is.na(hourly_counts$Freq)] <- 0 hashtag#Creating a Sleep pointer as a column in dataframe
            hourly_counts$sleep <- ifelse((sleep_time > wake_time &
                                             (hourly_counts$Var1 >= sleep_time |
                                                hourly_counts$Var1 <= wake_time))
                                          |
                                            (sleep_time <= wake_time
                                             & (hourly_counts$Var1 >= sleep_time
                                                & hourly_counts$Var1 <= wake_time))
                                          , TRUE, FALSE) hashtag#Simple rearranging according to the start of recording time
            hourly_counts <- hourly_counts[order((hourly_counts$Var1 - start_time)
                                                 %% nrow(hourly_counts)),]
            
            row.names(hourly_counts) <- 1:nrow(hourly_counts)
            
            
            # Plot with sleep indicators for each visit
            ggplot(hourly_counts,
                   aes(x = factor(Var1, levels = unique(Var1)),
                       y = Freq, color = sleep, group=1)) +
              geom_line() +
              geom_point() +
              ggtitle(paste("Cough Plot for" ,subject) )+
              scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
              labs(x = "Hours", y = "Frequency of Cough") +
              scale_y_continuous(limits = c(0, 290))+
              theme_minimal()
            
            
            
          }
        })
      })
      
    }
  })
}

shinyApp(ui, server)