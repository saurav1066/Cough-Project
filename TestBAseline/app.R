library(shiny)
library(readxl)
library(haven)
library(ggplot2)
library(gt)
library(dplyr)



# Helper Functions --------------------------------------------------------


#Function that does all required preprocessing
preprocessing <- function() {
  #reading the given excel file adn the sas file
  cough_data <- read_excel("../1465-0008-coughdata.xlsx")
  extension_data <- read_sas("../adsl.sas7bdat")
  
  
  #Subsetting for the columns in consideration
  extension_data <- extension_data[c('SUBJID','AGE', 'SEX', 'HRCT', 'SMOKSTAT')]
  
  #checking unique subject ids
  subject_id <- unique(cough_data$USUBJID)
  
  #checking unique visits
  visit_id <- unique(cough_data$VISIT)
  
  # Converting the 'fadtc' column to POSIXct format
  cough_data$FADTC <- as.POSIXct(cough_data$FADTC, format = "%Y-%m-%dT%H:%M:%S")
  
  # Creating a new column 'hour' to extract the hour from the timestamp
  cough_data$hour <- format(cough_data$FADTC, "%H")
  
  
  # Return the processed data and unique IDs
  list(cough_data = cough_data, extension_data = extension_data, subject_id = subject_id, visit_id = visit_id)
}


#Helper Function to compute Hourly cough Counts
hourly_counts_table <- function(new_df, sleep, wake, start) {
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
  hourly_counts$sleep <- ifelse((sleep > wake &
                                   (hourly_counts$Var1 >= sleep |
                                      hourly_counts$Var1 <= wake))
                                |
                                  (sleep <= wake
                                   & (hourly_counts$Var1 >= sleep
                                      & hourly_counts$Var1 <= wake))
                                , TRUE, FALSE)
  
  #Simple rearranging according to the start of recording time
  hourly_counts <- hourly_counts[order((hourly_counts$Var1 - start)
                                       %% nrow(hourly_counts)),]
  
  row.names(hourly_counts) <- 1:nrow(hourly_counts)
  
  return(hourly_counts)
}



# UI ----------------------------------------------------------------------


#Calling the function above for access of variables
result <- preprocessing()

#UI
ui <- fluidPage(
  titlePanel('Cough Application'),
  
  sidebarLayout(
    sidebarPanel(
      # Condition Statement for popping out another input
      selectInput(inputId = "Condition",
                  label = "Select Choice",
                  choices = c("Single Insight", "Comparision", "Baseline")),
      
      #Condition Single Insight
      conditionalPanel(
        condition = "input.Condition == 'Single Insight'",
        
        #Selecting required visit and subject for observation as input
        selectInput(inputId = "SubjectID",
                    label = "Select Subject ID",
                    choices = result$subject_id),
        
        selectInput(inputId = "Visit",
                    label = "Select Visit Number",
                    choices = result$visit_id),
        
        #Characteristics of the Subjects
        fluidRow(h3("Subject Characteristics:"),
                 
                 #Output subject Characteristics
                 gt_output("characteristics_table")
        )
      ),
      
      #Condition Comparision
      conditionalPanel(
        condition = "input.Condition == 'Comparision'",
        
        #Selecting required visit and subject for observation as input
        selectInput(inputId = "comparision_SubjectID",
                    label = "Select Subject ID",
                    choices = result$subject_id),
        
        #Characteristics of the Subjects
        fluidRow(h3("Subject Characteristics:"),
                 
                 #Output subject Characteristics
                 gt_output("characteristics_table_comparision")
        )
        
        
      ),
      
      #Condition Baseline
      conditionalPanel(
        condition = "input.Condition == 'Baseline'",
        
        
        # Dropdown
        # Condition Statement for popping out another input
        selectInput(inputId = "baseline_condition",
                    label = "Select Baseline Choice",
                    choices = c("Visit", "Sex")),
        
        conditionalPanel(
          condition = "input.baseline_condition == 'Visit'",
          
          #Selecting baseline visit
          selectInput(inputId = "baseline_Visit",
                      label = "Select Visit Number",
                      choices = result$visit_id)
        ),
        conditionalPanel(
          condition = "input.baseline_condition == 'Sex'",
          
          # Condition Statement for popping out another input
          selectInput(inputId = "baseline_Sex",
                      label = "Select Sex",
                      choices = c("Male", "Female")),
          
        ),
        
      ),
      
    ),
    
    mainPanel(
      #Condition Single Insight
      conditionalPanel(
        condition = "input.Condition == 'Single Insight'",
        plotOutput(outputId = "cough_plot"),
        plotOutput(outputID = "bout_plot")
      ),
      
      #Condition Comparision
      conditionalPanel(
        condition = "input.Condition == 'Comparision'",
        plotOutput(outputId = "comparision_plot")
      ),
      
      #Condition Baseline
      conditionalPanel(
        condition = "input.Condition == 'Baseline'",
        
        uiOutput(outputId = 'baseline_plot')
        
        
      )
    )
  )
)


# Server ------------------------------------------------------------------

#Server Function
server <- function(input, output, session) {
  observe({
    
    
    # Baseline ----------------------------------------------------------------
    
    
    #Condition BAseline dropdown
    if(input$Condition == 'Baseline'){
      if(input$baseline_condition == 'Visit'){
        #Selecting required data from the input
        selected_data <- reactive({
          result$cough_data[result$cough_data$VISIT == input$baseline_Visit,]
        })
        
        
        new_df <- selected_data()
        
        
        # Generate UI elements dynamically
        id_list <- lapply(result$subject_id, as.character)
        
        output$baseline_plot <- renderUI({
          lapply(id_list, function(id) {
            plotOutput(id)
          })
        })
        
        
        # Render plots for each ID
        lapply(id_list, function(subject) {
          
          output[[subject]] <- renderPlot({
            
            df <- new_df[new_df$USUBJID == as.numeric(subject),]
            
            # #Filtering dataframe so that only needed columns are selected
            # df <- new_df %>%
            #   dplyr::select(c("FAOBJ", "USUBJID", "VISIT", "FADTC", "hour")) %>%
            #   dplyr::filter(USUBJID == as.numeric(subject))
            
            #Getting sleep, wake and start time of recording values
            sleep_time <- as.integer(df$hour[df$FAOBJ == "Sleep"])
            wake_time <- as.integer(df$hour[df$FAOBJ == "Wake"])
            
            
            start_time<- 0 # needs initialization because some subjects do not have a record
            
            start_time <-  as.integer(df$hour[df$FAOBJ == "Actual Recording Start Time"])
            
            
            # Counting the occurrences of "cough" for each hour of the day
            hourly_counts <- table(df$hour[df$FAOBJ == "Cough"])
            hourly_counts <- data.frame(hourly_counts)
            
            #Converting the hour data to integer
            hourly_counts$Var1 <- as.integer(as.character(hourly_counts$Var1))
            
            if (nrow(hourly_counts) == 0){
              ggplot() +
                ggtitle(paste("Cough Plot for" ,subject) )+
                labs(x = "Hour", y = "Count", title = paste0("No data found for ", subject , ' in ', input$baseline_Visit ))
            }
            else {
              #Calling helper function
              hourly_counts <- hourly_counts_table(df, sleep_time , wake_time,start_time)
              
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
      else{
        
        #Selecting Filtered sex data
        selected_extension <- reactive({
          result$extension_data[result$extension_data$SEX == input$baseline_Sex,]
          
        })
        
        extension <- selected_extension()
        
        # Generate UI elements dynamically
        id_list <- lapply(extension$SUBJID, as.character)
        
        output$baseline_plot <- renderUI({
          lapply(id_list, function(id) {
            plotOutput(id)
          })
        })
        
        
        
        # Render plots for each ID
        lapply(id_list, function(subject) {
          
          
          output[[subject]] <- renderPlot({
            df <- result$cough_data[result$cough_data$USUBJID == as.numeric(subject),]
            
            if(nrow(df) != 0) {
              #grouping the dataframe based on visits
              grouped_df <- split(df, df$VISIT,df$USUBJID)
              
              #Initilizing for later use to store plots and dataframes
              cough_counts <- data.frame()
              
              
              #getting hourly counts of each group of visits
              for (i in 1:length(grouped_df)) {
                
                
                #Getting the hour where the subject slept
                sleep_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Sleep"])
                
                #Getting the hour where the subject woke up
                wake_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Wake"])
                
                #Getting the hour where the recording started
                start_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Actual Recording Start Time"])
                
                #Calling helper function
                hourly_count <- hourly_counts_table(grouped_df[[i]], sleep_time,wake_time, start_time)
                
                #Adding Visit Column in the hourly count  table
                hourly_count$Visit <- grouped_df[[i]]$VISIT[1]
                
                #Storing all cough counts in a separate dataframe
                cough_counts <- rbind(cough_counts, hourly_count)
              }
              
              
              #plotting all the visits in same graph
              ggplot(cough_counts,
                     aes(x = Var1, y = Freq, group = Visit, color = Visit)) +
                geom_line() +
                geom_point()+
                ggtitle(paste0("Comparision of all visits for subject ", subject)) +
                labs(x = "Hours", y = "Frequency of Cough")+
                theme_minimal()
            }
            else{
              ggplot() +
                ggtitle(paste("Cough Plot for" ,subject) )+
                labs(x = "Hour", y = "Count", title = paste0("No data found for ", subject ))
            }
            
            
          })
        })
        
        
      }
      
    }
    
    
    # Single Insight ----------------------------------------------------------
    
    
    #Condition Single Insight
    else if (input$Condition == 'Single Insight'){
      # Selecting required data as per input
      selected_data <- reactive({
        result$cough_data[result$cough_data$VISIT == input$Visit &
                            result$cough_data$USUBJID == input$SubjectID,]
      })
      
      #Selecting required extension data
      
      selected_extension <- reactive({
        result$extension_data[result$extension_data$SUBJID == input$SubjectID,]
      })
      
      new_df <- selected_data()
      extension <- selected_extension()
      # Creating the plot
      output$cough_plot <- renderPlot({
        
        
        if (nrow(new_df) > 0) {
          
          #Getting sleep, wake and start time of recording values
          sleep_time <- as.integer(new_df$hour[new_df$FAOBJ == "Sleep"])
          wake_time <- as.integer(new_df$hour[new_df$FAOBJ == "Wake"])
          
          
          start_time<- 0 # needs initialization because some subjects do not have a record
          
          start_time <-  as.integer(new_df$hour[new_df$FAOBJ == "Actual Recording Start Time"])
          
          #Calling helper Function
          hourly_counts <- hourly_counts_table(new_df, sleep_time,wake_time, start_time)
          
          #Characteristics Table
          output$characteristics_table <- render_gt({
            gt_table <- gt(extension) %>%
              cols_align(
                align = "center",
                columns = everything()
              )
            
          })
          
          
          # Plot with sleep indicators
          ggplot(hourly_counts,
                 aes(x = factor(Var1, levels = unique(Var1)),
                     y = Freq, color = sleep, group=1)) +
            geom_line() +
            geom_point() +
            scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
            labs(x = "Hours", y = "Frequency of Cough") +
            scale_y_continuous(limits = c(0, 290))+
            theme_minimal()
        }
        
        else {
          ggplot() +
            labs(x = "Hour", y = "Count", title = "No data found for the given Visit ID and USUBJID")
        }
      })
      
      
      #Creating bout plot
      output$bout_plot <- renderPlot({
        
        if(nrow(new_df) >0 ){
          
          start_time<- 0 # needs initialization because some subjects do not have a record
          
          start_time <-  as.integer(new_df$hour[new_df$FAOBJ == "Actual Recording Start Time"])
          
          
          #Getting cough times
          cough_times <- new_df$FADTC[new_df$FAOBJ == "Cough"]
          
          #Formatting for conversion
          cough_times <- format(cough_times, "%H:%M:%S")
          
          cough_times <- as.POSIXct(cough_times, format = "%H:%M:%S" )
          
          #Creating a dataframe
          df <- data.frame(cough_times)
          
          #Creating a group variavle based on 6 second duration condition'
          df <- df %>%
            arrange(cough_times) %>%
            mutate(group = cumsum(c(0, diff(cough_times) >6)))
          
          #Filter the data to only include required
          df_filtered <- df %>%
            group_by(hour, group) %>%
            filter(n() >=2)
          
          #Extract minuutes
          df_filtered <- format(df_filtered$cough_times, "%M")
          
          #Calculate the count of coughs in each group
          df_filtered <- df_filtered %>%
            group_by(group) %>%
            mutate(group_count = n())

          #Simple rearranging according to start time
          df_filtered <- df_filtered[order((df_filtered$hour - start_time) 
                                           %% nrow(df_filtered)),]
                    
          #Plot
          ggplot(df_filtered, aes(x = factor(hour, levels = unique(hour)), y = minutes, color = as.factor(group_count))) +
            geom_point()+
            labs(x = "Hour",y = "Group")+
            scale_color_discrete(name = "Cough Counts per Bout")+
            theme_minimal()
        }
        
      })
      
    }
    
    
    # Comparision -------------------------------------------------------------
    
    
    #Condition Comparision
    else {
      selected_data <- reactive({
        result$cough_data[result$cough_data$USUBJID == input$comparision_SubjectID,]
        
      })
      
      
      #Selecting required extension data
      
      selected_extension <- reactive({
        result$extension_data[result$extension_data$SUBJID == input$comparision_SubjectID,]
      })
      
      new_df <- selected_data()
      
      extension <- selected_extension()
      
      
      #Characteristics Table
      output$characteristics_table_comparision <- render_gt({
        gt_table <- gt(extension) %>%
          cols_align(
            align = "center",
            columns = everything()
          )
        
      })
      
      #Generating Plot
      output$comparision_plot <- renderPlot({
        
        #grouping the dataframe based on visits
        grouped_df <- split(new_df,new_df$VISIT)
        
        
        #Initilizing for later use to store plots and dataframes
        cough_counts <- data.frame()
        plot_list <- list()
        
        #getting hourly counts of each group of visits
        for (i in 1:length(grouped_df)) {
          
          
          #Getting the hour where the subject slept
          sleep_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Sleep"])
          
          #Getting the hour where the subject woke up
          wake_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Wake"])
          
          #Getting the hour where the recording started
          start_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Actual Recording Start Time"])
          
          #Calling helper function
          hourly_count <- hourly_counts_table(grouped_df[[i]], sleep_time,wake_time, start_time)
          
          
          
          #Creating plot for each visit
          plot <- ggplot(hourly_count,
                         aes(x = factor(Var1, levels = unique(Var1)),
                             y = Freq, color = sleep, group=1)) +
            geom_line() +
            geom_point() +
            scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
            labs(x = "Hours", y = "Frequency of Cough") +
            ggtitle(paste("Cough Plot for" ,unique(grouped_df[[i]]$VISIT) ))+
            scale_y_continuous(limits = c(0, 290))+
            theme_minimal()
          
          plot_list[[i]] <- plot
          
          #Adding Visit Column in the hourly count  table
          hourly_count$Visit <- grouped_df[[i]]$VISIT[1]
          
          #Storing all cough counts in a separate dataframe
          cough_counts <- rbind(cough_counts, hourly_count)
        }
        
        
        #plotting all the visits in same graph
        p<- ggplot(cough_counts,
                   aes(x = Var1, y = Freq, group = Visit, color = Visit)) +
          geom_line() +
          geom_point()+
          ggtitle("Comparision of all visits") +
          labs(x = "Hours", y = "Frequency of Cough")+
          theme_minimal()
        
        
        
        plot_list[[length(plot_list)+1]] <- p
        
        
        #Output multiple graphs
        gridExtra::grid.arrange(grobs = plot_list)
        
        
      })
    }
    
  })
}

shinyApp(ui, server)