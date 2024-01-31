library(shiny)
library(readxl)
library(haven)
library(ggplot2) 
library(gt)

#Function that does all required preprocessing
preprocessing <- function() { 
  
  #reading the given excel file adn the sas file
  cough_data <- read_excel("../1465-0008-coughdata.xlsx")
  extension_data <- read_sas("../adsl.sas7bdat") 
  
  #converting excel file to data frame
  cough_data <- data.frame(cough_data)
  extension_data <- data.frame(extension_data)
  
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

#Helper Function

hourly_counts_table <- function(df, sleep, wake){
  
  # Counting the occurrences of "cough" for each hour of the day
  hourly_counts <- table(df$hour[df$FAOBJ == "Cough"])
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
                                , TRUE, FALSE)  #Simple rearranging according to the start of recording time
  hourly_counts <- hourly_counts[order((hourly_counts$Var1 - start_time)
                                       %% nrow(hourly_counts)),]
  
  row.names(hourly_counts) <- 1:nrow(hourly_counts)
  
  return(hourly_counts)
  
}


#Calling the function above for access of variabless
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
                  choices = result$visit_id)
    )
    ),
    
    #Condition Comparision
    conditionalPanel(
      condition = "input.Condition == 'Comparision'", 
      
      #Selecting required visit and subject for observation as input
      selectInput(inputId = "SubjectID",
                  label = "Select Subject ID",
                  choices = result$subject_id),
      
      selectInput(inputId = "Visit",
                  label = "Select Visit Number",
                  choices = result$visit_id)
    )
    ),
    
    #Condition Baseline
    conditionalPanel(
      condition = "input.Condition == 'Baseline'", 
      
      # Condition Statement for popping out another input
      selectInput(inputId = "baseline_condition",
                  label = "Select Choice",
                  choices = c("Visit","Sex"), 
      
      conditionalPanel(
        condition = "input.baseline_condition == 'Visit'",
        
        #Selecting baseline visit
        selectInput(inputId = "Visit",
                    label = "Select Visit Number",
                    choices = result$visit_id)
      ),
      
      conditionalPanel(
        condition = "input.baseline_condition == 'Sex'",
        
        #Selecting baseline visit
        selectInput(inputId = "Sex",
                    label = "Select Sex",
                    choices = c("MAle", "Female"))
      
    )
  ),
  
  mainPanel(
    
    #Condition Single Insight
             conditionalPanel(
               condition = "input.Condition == 'Single Insight'",
               plotOutput(outputId = "cough_plot")
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


#Server
server <- function(input, output, session) {
  observe({ 
    #Condition BAseline
    if(input$Condition == 'Baseline'){
      if(input$baseline_condition == 'Visit'){ #Selecting required data from the input
        selected_data <- reactive({
          result$cough_data[result$cough_data$VISIT == input$baseline_Visit,]
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
            df <- new_df[new_df$USUBJID == as.numeric(subject),]  #Getting sleep, wake and start time of recording values
            sleep_time <- as.integer(df$hour[df$FAOBJ == "Sleep"])
            wake_time <- as.integer(df$hour[df$FAOBJ == "Wake"])
            
            
            start_time<- 0 # needs initialization because some subjects do not have a record
            
            start_time <- as.integer(df$hour[df$FAOBJ == "Actual Recording Start Time"])
            
            
            # Counting the occurrences of "cough" for each hour of the day
            hourly_counts <- table(df$hour[df$FAOBJ == "Cough"])
            hourly_counts <- data.frame(hourly_counts)  #Converting the hour data to integer
            hourly_counts$Var1 <- as.integer(as.character(hourly_counts$Var1))
            
            if (nrow(hourly_counts) == 0){
              ggplot() +
                ggtitle(paste("Cough Plot for" ,subject) )+
                labs(x = "Hour", y = "Count", title = paste0("No data found for ", subject , ' in ', input$baseline_Visit ))
            }
            else {  #Calling helper function
              hourly_counts <- hourly_counts_table(df, sleep_time , wake_time)
              
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
      else{  #Selecting Filtered sex data
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
            
            if(nrow(df) != 0) {  #grouping the dataframe based on visits
              grouped_df <- split(df, df$VISIT,df$USUBJID)  #Initilizing for later use to store plots and dataframes
              cough_counts <- data.frame()  #getting hourly counts of each group of visits
              for (i in 1:length(grouped_df)) {  #Getting the hour where the subject slept
                sleep_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Sleep"])  #Getting the hour where the subject woke up
                wake_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Wake"])  #Getting the hour where the recording started
                start_time <- as.integer(grouped_df[[i]]$hour[grouped_df[[i]]$FAOBJ == "Actual Recording Start Time"])  #Calling helper function
                hourly_count <- hourly_counts_table(grouped_df[[i]], sleep_time,wake_time)  #Adding Visit Column in the hourly count table
                hourly_count$Visit <- grouped_df[[i]]$VISIT[1]  #Storing all cough counts in a separate dataframe
                cough_counts <- rbind(cough_counts, hourly_count)
              }  #plotting all the visits in same graph
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
    #Condition Single Insight
    else if (input$Condition == 'Single Insight'){
      # Selecting required data as per input
      selected_data <- reactive({
        result$cough_data[result$cough_data$VISIT == input$Visit &
                            result$cough_data$USUBJID == input$SubjectID,]
      })  #Selecting required extension data
      
      selected_extension <- reactive({
        result$extension_data[result$extension_data$SUBJID == input$SubjectID,]
      })
      
      new_df <- selected_data()
      extension <- selected_extension()
      
      # Creating the plot
      output$cough_plot <- renderPlot({
        
        
        if (nrow(new_df) > 0) {  #Getting sleep, wake and start time of recording values
          sleep_time <- as.integer(new_df$hour[new_df$FAOBJ == "Sleep"])
          wake_time <- as.integer(new_df$hour[new_df$FAOBJ == "Wake"])
          
          
          start_time<- 0 # needs initialization because some subjects do not have a record
          
          start_time <- as.integer(new_df$hour[new_df$FAOBJ == "Actual Recording Start Time"])  #Calling helper Function
          hourly_counts <- hourly_counts_table(new_df, sleep_time,wake_time)  #Characteristics Table
          output$characteristics_table <- render_gt({
            gt_table <- gt(extension)
            
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
      
    }
    
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
        gt_table <- gt(extension)
        
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
          hourly_count <- hourly_counts_table(grouped_df[[i]], sleep_time,wake_time) 
          
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
          
          #Adding Visit Column in the hourly count table
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
