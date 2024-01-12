#
#This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#



library(shiny)
library(readxl)
library(dplyr)
library(zoo)
library(ggplot2)
library(plotly)
library(tidyverse)

#Reading required excel files
ds <- read_excel("ds.xlsx")
pro <- read_excel("pro.xlsx")



#converting csv to data frame
subject_list <- data.frame(ds)
input_file <- data.frame(pro)

# Helper Function to calculate running average
calculate_running_avg <- function(df, column, past_days = 7, min_days = 4) {
  # Convert the column to numeric
  df[[column]] <- as.numeric(df[[column]])
  
  # Apply the running average calculation to each row in the dataframe
  sapply(1:nrow(df), function(row) {
    # Get the QSDY value for the current row
    current_qsd_value <- df$QSDY[row]
    
    # Get the rows for the past 'past_days' QSDY values
    past_rows <- which(df$QSDY %in% (current_qsd_value-1):(current_qsd_value-past_days))
    
    # If there are less than 'min_days' past rows, return NA
    if (length(past_rows) < min_days) {
      return(NA)
    }
    
    # Otherwise, calculate and return the average for the desired column
    return(mean(df[past_rows, column]))
  })
}


#UI
ui <- fluidPage(
  titlePanel("Patient Visualization"),
  sidebarLayout(
    sidebarPanel(
      fluidRow( h3("Select Options"),
                #Selection box for subject Ids
                selectInput(inputId = "subject_id",
                            label = "Select Subject ID",
                            choices = subject_list$USUBJID)
      ),
      fluidRow(
        checkboxGroupInput("checkID", label = "Select Choices", choices = c("Stool", "Abdominal-Pain", "Well-Being"), selected = "Stool")
      )
      
    ),
    
    #Plot in the main panel
    mainPanel(
      h3("Plot"),
      plotlyOutput(outputId = "plot")
    )
  )
  
  
)

#Server
server <- function(input, output, session) {
  
  #Selecting the data only for selected subject id
  observe({
    selected_subject <- reactive({
      input_file[input_file$USUBJID == input$subject_id,]
      
    })
    
    selected_box <- reactive({input$checkID})
    
    output$plot <- renderPlotly({
      input_file <- selected_subject()
      option <- selected_box()
      
      
      #creating 3 different dataframes for the three required subsets
      stool <- input_file[input_file$QSTEST == "CDAI01-Daily Number of Stools",]
      abdominal_pain <- input_file[input_file$QSTEST == "CDAI01-Daily Average Abdominal Pain",]
      well_being <- input_file[input_file$QSTEST == "CDAI01-Daily Average General Well-being",]
      
      
      # #Running average with a group of 7
      # stool <- stool %>%
      # arrange(QSDY) %>%
      # mutate((avg = ifelse(row_number() >= 7 & !is.na(QSSTRESN),
      # rollapply(QSSTRESN, width = 7,
      # FUN = function(x) if(sum(!is.na(x)) >= 4)
      # mean(x, na.rm = TRUE)
      # else NA, align = "right", fill = NA),
      # NA)))
      #
      # print(stool['QSDY'])
      #
      # abdominal_pain <- abdominal_pain %>%
      # arrange(QSDY) %>%
      # mutate((avg = ifelse(row_number() >= 7 & !is.na(QSSTRESN),
      # rollapply(QSSTRESN, width = 7,
      # FUN = function(x) if(sum(!is.na(x)) >= 4)
      # mean(x, na.rm = TRUE)
      # else NA, align = "right", fill = NA),
      # NA)))
      #
      # well_being <- well_being %>%
      # arrange(QSDY) %>%
      # mutate((avg = ifelse(row_number() >= 7 & !is.na(QSSTRESN),
      # rollapply(QSSTRESN, width = 7,
      # FUN = function(x) if(sum(!is.na(x)) >= 4)
      # mean(x, na.rm = TRUE)
      # else NA, align = "right", fill = NA),
      # NA)))
      
      
      #Running average based on visit (QSDY) from above helper function
      stool$running_average_stool <- calculate_running_avg(stool, "QSSTRESC")
      abdominal_pain$running_average_abdominal_pain <- calculate_running_avg(abdominal_pain,"QSSTRESC")
      well_being$running_average_well_being <- calculate_running_avg(well_being,'QSSTRESC')
      
      
      # Selecting only necessary columns and rename the running_average column in each data frame
      stool <- stool %>% select(QSDY, running_average_stool)
      abdominal_pain <- abdominal_pain %>% select(QSDY, running_average_abdominal_pain)
      well_being <- well_being %>% select(QSDY, running_average_well_being)
      
      
      
      #Joining the three dataframes with required columns
      merged_df <- stool %>%
        full_join(abdominal_pain, by = "QSDY") %>%
        full_join(well_being, by = "QSDY")
      
      #Renaming the column names to match the choices
      colnames(merged_df) <- c("QSDY", "Stool", "Abdominal-Pain", "Well-Being")
      
      
      # Checking if any checkboxes are selected
      if (is.null(option)) {
        return(NULL)
      }
      
      # Converting data to long format
      df_long <- tidyr::pivot_longer(merged_df, cols = option)
      
      # Create a plot based on the selected checkboxes
      ggplot(df_long, aes(x = QSDY, y = value, color = name)) +
        geom_point(data = df_long[is.na(df_long$value),], aes(y=0), shape = 3, color = 'black', alpha = 0.5)+
        scale_x_continuous(limits = c(1, max(merged_df$QSDY)))+
        geom_line() +
        labs(title = paste( paste(option, collapse = ", ")), x = "QSDY", y = "Running Average") +
        scale_color_discrete(name = "Variable")+
        theme_minimal()
      
      
      # # Plotting individual plots
      #
      # ggplot(data = merged_df, aes(x = QSDY, y = running_average_stool))+
      # scale_x_continuous(limits = c(1,max(stool["QSDY"])))+
      # geom_point()+
      # geom_line()+
      # labs(x = "Visit", y = 'Seven visit average')+
      # theme_minimal()
      
      # ggplot(data = abdominal_pain, aes(x = QSDY, y = abdominal_pain[,ncol(abdominal_pain)]))+
      # scale_x_continuous(limits = c(1,max(abdominal_pain["QSDY"])))+
      # geom_point()+
      # geom_line()+
      # labs(x = "Visit", y = 'Seven day average')+
      # theme_minimal()
      
      # ggplotly(ggplot(data = well_being, aes(x = QSDY, y = well_being[,ncol(well_being)]))+
      # scale_x_continuous(limits = c(1,max(well_being["QSDY"])))+
      # geom_point()+
      # geom_line()+
      # labs(x = "Visit", y = 'Seven day average')+
      # theme_minimal())
      
      #
      # # Converting data to long format
      # df_long <- tidyr::pivot_longer(merged_df, cols = c(Stool, Abdominal-Pain, Well-Being))
      #
      #
      # # Plotting together
      # ggplot(df_long, aes(x = QSDY, y = value, color = name)) +
      # # geom_point()+
      # geom_line() +
      # labs(x = "QSDY", y = "Running Average", color = "Measurement") +
      # theme_minimal()
      #
      
      
      
      
    })
    
  })
  
  
  
}

shinyApp(ui, server)